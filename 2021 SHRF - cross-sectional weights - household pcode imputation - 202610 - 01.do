* ============================================================================ *
* 2021 SHRF - cross-sectional weights - household pcode imputation - 202610 - 01
* ============================================================================ *
* For:     Filling missing postal codes in the CanCHEC mobility files using the
*          OBSERVED postal codes of other members of the same census household,
*          before any Fines et al. (2017) style imputation. The rules never use
*          hospitalization or death records, so imputation does not depend on
*          the outcome being evaluated.
*
* Rule (for person i, year t, own postal code not observed):
*   1. Donor j is in i's census household and has an OBSERVED postal code in t.
*   2. Co-residence evidence: in the nearest year before t and the nearest year
*      after t where it can be determined, i and j shared the same postal code.
*      The census year counts as "shared" (they lived together on census day)
*      unless both postal codes are observed and differ.
*   3. Two-sided evidence -> accept. One-sided evidence (other side unknown,
*      never contradicted) -> accept only within $onesided_max years of the
*      census, or for strong ties (couple; child under 18 receiving from parent).
*   4. Several donors: two-sided before one-sided, then by pair type
*      (couple > child<-parent > parent<-child > other relative > non-relative).
*   5. Donors give only observed codes, never imputed ones (no chaining).
*
* Source of final postal code (pc_src):
*   1 = own observed        2 = household, two-sided   3 = household, one-sided
*   4 = Fines, full code    5 = Fines, partial (FSA+)  9 = unknown
*
* Inputs (EDIT IN SECTION 0):
*   - RAW (not imputed) mobility keyfile, wide: uniqid pc2001 ... pc20XX
*   - Fines-imputed mobility keyfile, wide: uniqid pc2001 ... pc20XX
*   - Cohort file with uniqid, census household id, census relationship,
*     age at census (and year of death if available)
*
* Outputs:
*   - canchec`cohort'_pcode_hh_long_USE.dta   (uniqid year pc_final pc_src ...)
*   - canchec`cohort'_hh_validation_*.dta     (coverage and accuracy tables)
* ============================================================================ *

* CONTENTS
* 0. SETUP
* 1. PREPARE COHORT FILE (HOUSEHOLD ROLES)
* 2. PREPARE LONG POSTAL CODE FILE
* 3. HOUSEHOLD IMPUTATION PROGRAM
* 4. COMBINE OWN, HOUSEHOLD AND FINES POSTAL CODES
* 5. VALIDATION (MASK AND RECOVER)
* X. EXECUTE

* 0. SETUP                                                                  ****
* ---------------------------------------------------------------------------- *
clear all
set more off
version 16

* >>> EDIT: working directory <<<
* cd "Z:\VRDC-PROJ-10058\...\cross-sectional_weight"

* >>> EDIT: cohort and year range <<<
global cohort    2006
global yr_first  2001
global yr_last   2016

* >>> EDIT: input files <<<
global raw_file    ".\Data\canchec2006_key_mobility_RAW.dta"       // NOT imputed
global fines_file  ".\Data\canchec2006_key_mobility_imputed_USE.dta"
global cohort_src  ".\Data\canchec2006_cohort_USE.dta"

* >>> EDIT: variable names in cohort_src <<<
global hh_var     "hhid_cens"      // census household identifier
global rel_var    "rel_cens"       // census relationship to Person 1
global age_var    "age_cens"       // age on census day
global death_var  ""               // year of death; leave "" if not available

* Rule parameters
global onesided_max  2             // years from census allowed for one-sided
global mask_share    0.10          // share of observed codes masked (validation)
global seed          20261026

* Intermediate and output files
global cohort_file  ".\Data\canchec${cohort}_hh_roles.dta"
global long_file    ".\Data\canchec${cohort}_pcode_long_obs.dta"
global hh_out       ".\Data\canchec${cohort}_pcode_hh_donor.dta"
global final_file   ".\Data\canchec${cohort}_pcode_hh_long_USE.dta"
global val_masked   ".\Data\canchec${cohort}_hh_val_masked.dta"
global val_out      ".\Data\canchec${cohort}_hh_val_donor.dta"


* 1. PREPARE COHORT FILE (HOUSEHOLD ROLES)                                  ****
* ---------------------------------------------------------------------------- *
capture program drop prep_cohort
program define prep_cohort

  use "$cohort_src", clear
  keep uniqid $hh_var $rel_var $age_var $death_var
  rename ($hh_var $age_var) (hhid age_cens)
  if "$death_var" != "" rename $death_var death_year

  * Collapse census relationship to Person 1 into four roles.
  *   1 = Person 1, or spouse / common-law partner of Person 1
  *   2 = child of Person 1 (incl. step/adopted/foster)
  *   3 = other relative
  *   4 = non-relative (roommate, lodger, etc.)
  * >>> EDIT: fill in the codes from the census relationship variable <<<
  generate byte role = .
  * replace role = 1 if inlist($rel_var, ...)
  * replace role = 2 if inlist($rel_var, ...)
  * replace role = 3 if inlist($rel_var, ...)
  * replace role = 4 if inlist($rel_var, ...)

  * Stops here until the codes above are filled in
  assert !missing(role)

  label define role_lbl 1 "P1/spouse" 2 "child" 3 "other rel" 4 "non-rel"
  label values role role_lbl

  isid uniqid
  save "$cohort_file", replace

end


* 2. PREPARE LONG POSTAL CODE FILE                                          ****
* ---------------------------------------------------------------------------- *
capture program drop prep_long
program define prep_long

  use uniqid pc${yr_first}-pc${yr_last} using "$raw_file", clear

  reshape long pc, i(uniqid) j(year)

  * Clean and keep only valid, complete postal codes as "observed"
  replace pc = upper(subinstr(pc, " ", "", .))
  generate pc_obs = pc if ustrregexm(pc, "^[A-Z][0-9][A-Z][0-9][A-Z][0-9]$")
  drop pc

  merge m:1 uniqid using "$cohort_file", keep(match) nogen

  * No person-years before birth
  drop if age_cens + (year - $cohort) < 0

  * No person-years after death (if available)
  capture confirm variable death_year
  if !_rc drop if !missing(death_year) & year > death_year

  isid uniqid year
  compress
  save "$long_file", replace

  * Starting point: share of person-years with an observed code, by year
  generate byte observed = !missing(pc_obs)
  tab year observed, row

end


* 3. HOUSEHOLD IMPUTATION PROGRAM                                           ****
* ---------------------------------------------------------------------------- *
* Usage: hh_impute "<long file>" "<output file>"
* The long file must contain: uniqid hhid year pc_obs role age_cens
* Output: one row per imputed person-year with the chosen donor's code.

capture program drop hh_impute
program define hh_impute

  args infile outfile

  * Person-level file, multi-person households only
  use uniqid hhid role age_cens using "`infile'", clear
  duplicates drop uniqid, force
  bysort hhid: generate hhsize = _N
  keep if hhsize > 1
  drop hhsize
  tempfile persons
  save `persons'

  * Donor copies (person-level and long)
  rename (uniqid role age_cens) (donor_id donor_role donor_age_cens)
  tempfile donors
  save `donors'

  use uniqid year pc_obs using "`infile'", clear
  rename (uniqid pc_obs) (donor_id pc_j)
  tempfile dlong
  save `dlong'

  * Recipients: anyone with at least one unobserved year
  use uniqid year pc_obs using "`infile'", clear
  bysort uniqid: egen n_miss = total(missing(pc_obs))
  keep if n_miss > 0
  keep uniqid
  duplicates drop
  merge 1:1 uniqid using `persons', keep(match) nogen

  * All recipient-donor pairs within the household
  joinby hhid using `donors'
  drop if uniqid == donor_id
  egen long pair = group(uniqid donor_id)

  * Pair type (recipient <- donor); lower = stronger
  generate byte pairtype = 4
  replace pairtype = 1 if role == 1 & donor_role == 1
  replace pairtype = 2 if role == 2 & donor_role == 1
  replace pairtype = 3 if role == 1 & donor_role == 2
  replace pairtype = 5 if role == 4 | donor_role == 4
  label define pair_lbl 1 "couple" 2 "child<-parent" 3 "parent<-child" ///
                        4 "other relative" 5 "non-relative", replace
  label values pairtype pair_lbl
  keep pair uniqid donor_id pairtype age_cens
  tempfile pairs
  save `pairs'

  * Pair x year panel with both postal codes
  use uniqid year pc_obs using "`infile'", clear
  joinby uniqid using `pairs'
  rename pc_obs pc_i
  merge m:1 donor_id year using `dlong', keep(master match) nogen

  * Shared address indicator where determinable
  generate byte shared = .
  replace shared = (pc_i == pc_j) if !missing(pc_i) & !missing(pc_j)
  replace shared = 1 if year == $cohort & missing(shared)

  * Nearest determinable value BEFORE t
  sort pair year
  generate byte last = shared
  by pair: replace last = last[_n-1] if missing(last) & _n > 1
  by pair: generate byte ev_before = last[_n-1]

  * Nearest determinable value AFTER t
  generate negyear = -year
  sort pair negyear
  generate byte nxt = shared
  by pair: replace nxt = nxt[_n-1] if missing(nxt) & _n > 1
  by pair: generate byte ev_after = nxt[_n-1]
  drop negyear last nxt

  * Candidates: recipient unobserved, donor observed
  keep if missing(pc_i) & !missing(pc_j)

  generate yrs_from_cens = abs(year - $cohort)
  generate age_i = age_cens + (year - $cohort)

  generate byte evidence = .
  replace evidence = 2 if ev_before == 1 & ev_after == 1
  replace evidence = 1 if missing(evidence) &                          ///
          ((ev_before == 1 & missing(ev_after)) |                      ///
           (ev_after  == 1 & missing(ev_before)))
  drop if missing(evidence)          // no evidence, or contradicted

  generate byte strong = pairtype == 1 | (pairtype == 2 & age_i < 18)
  drop if evidence == 1 & !(yrs_from_cens <= $onesided_max | strong)

  * Flag disagreement among accepted donors
  bysort uniqid year (pc_j): generate byte hh_conflict = pc_j[1] != pc_j[_N]

  * Choose one donor: two-sided first, then strongest pair type
  generate negev = -evidence
  sort uniqid year negev pairtype donor_id
  by uniqid year: keep if _n == 1

  rename (pc_j evidence pairtype) (pc_hh hh_evidence hh_pairtype)
  keep uniqid year pc_hh hh_evidence hh_pairtype hh_conflict donor_id
  isid uniqid year
  compress
  save "`outfile'", replace

end


* 4. COMBINE OWN, HOUSEHOLD AND FINES POSTAL CODES                          ****
* ---------------------------------------------------------------------------- *
capture program drop combine_sources
program define combine_sources

  * Fines-imputed codes in long format
  use uniqid pc${yr_first}-pc${yr_last} using "$fines_file", clear
  reshape long pc, i(uniqid) j(year)
  replace pc = upper(subinstr(pc, " ", "", .))
  rename pc pc_fines
  tempfile fines
  save `fines'

  use "$long_file", clear
  merge 1:1 uniqid year using "$hh_out", keep(master match) nogen
  merge 1:1 uniqid year using `fines', keep(master match) nogen

  generate pc_final = ""
  generate byte pc_src = 9

  * 1. Own observed
  replace pc_final = pc_obs if !missing(pc_obs)
  replace pc_src   = 1      if !missing(pc_obs)

  * 2-3. Household donor
  replace pc_src   = 2 if pc_src == 9 & !missing(pc_hh) & hh_evidence == 2
  replace pc_src   = 3 if pc_src == 9 & !missing(pc_hh) & hh_evidence == 1
  replace pc_final = pc_hh if inlist(pc_src, 2, 3)

  * 4. Fines full code (ideally from a re-run with p = 1)
  generate byte fines_full = ustrregexm(pc_fines, "^[A-Z][0-9][A-Z][0-9][A-Z][0-9]$")
  replace pc_src   = 4        if pc_src == 9 & fines_full
  replace pc_final = pc_fines if pc_src == 4

  * 5. Fines partial code with at least the FSA (allocate to WA later)
  generate byte fines_fsa = ustrregexm(pc_fines, "^[A-Z][0-9][A-Z]") & ///
                            strpos(pc_fines, "*") > 0
  replace pc_src   = 5        if pc_src == 9 & fines_fsa
  replace pc_final = pc_fines if pc_src == 5

  label define src_lbl 1 "own observed" 2 "household 2-sided"            ///
                       3 "household 1-sided" 4 "Fines full"              ///
                       5 "Fines partial (FSA)" 9 "unknown", replace
  label values pc_src src_lbl

  * Where each year's postal codes come from (round counts before vetting)
  tab year pc_src, row
  tab hh_pairtype if inlist(pc_src, 2, 3)
  tab hh_conflict if inlist(pc_src, 2, 3)

  keep uniqid hhid year pc_final pc_src hh_pairtype hh_evidence hh_conflict
  compress
  save "$final_file", replace

end


* 5. VALIDATION (MASK AND RECOVER)                                          ****
* ---------------------------------------------------------------------------- *
* Hide a random share of OBSERVED codes among people in multi-person
* households, re-run the household rule, and measure:
*   coverage = share of masked codes the rule fills
*   accuracy = exact postal code match, and FSA match, among those filled
* After PCCF+, repeat the comparison at DA and WA level, which is what the
* weights actually use. Masking also removes some anchors used as evidence,
* so results are slightly conservative.

capture program drop hh_validate
program define hh_validate

  use "$long_file", clear
  set seed $seed

  egen byte tagp = tag(hhid uniqid)
  bysort hhid: egen hhsize = total(tagp)
  drop tagp

  sort uniqid year
  generate double u = runiform()
  generate byte masked = !missing(pc_obs) & hhsize > 1 & u < $mask_share
  generate pc_true = pc_obs if masked
  replace pc_obs = "" if masked
  drop u hhsize
  save "$val_masked", replace

  hh_impute "$val_masked" "$val_out"

  use uniqid year pc_true using "$val_masked" if !missing(pc_true), clear
  merge 1:1 uniqid year using "$val_out", keep(master match)
  generate byte filled = _merge == 3
  drop _merge

  generate byte exact = pc_hh == pc_true                         if filled
  generate byte fsa   = substr(pc_hh,1,3) == substr(pc_true,1,3) if filled

  generate yrs = abs(year - $cohort)
  generate byte yrs_band = cond(yrs <= 2, 1, cond(yrs <= 5, 2, 3))
  label define band_lbl 1 "0-2 yrs" 2 "3-5 yrs" 3 "6+ yrs", replace
  label values yrs_band band_lbl

  * Coverage by distance from census
  preserve
    collapse (count) n_masked = filled (mean) coverage = filled, by(yrs_band)
    list, clean noobs
    save ".\Data\canchec${cohort}_hh_validation_coverage.dta", replace
  restore

  * Accuracy among filled, by pair type, evidence and distance from census
  keep if filled
  collapse (count) n_filled = exact (mean) exact fsa,                    ///
           by(hh_pairtype hh_evidence yrs_band)
  list, clean noobs
  save ".\Data\canchec${cohort}_hh_validation_accuracy.dta", replace

end


* X. EXECUTE                                                                ****
* ---------------------------------------------------------------------------- *
* Run in order. For national files, consider running by province (subset the
* cohort file on province of residence at census) if memory is tight.

* prep_cohort
* prep_long
* hh_validate                              // check rule before using it
* hh_impute "$long_file" "$hh_out"
* combine_sources

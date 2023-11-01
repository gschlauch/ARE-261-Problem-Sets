********************************************************************************
* Purpose: clean the raw data
********************************************************************************

* Initialize settings and filepaths
do "\\tsclient\Documents\github\ARE-261-Problem-Sets\Pset3\scripts\setup.do"

* Load the raw data
import delimited "$dirpath_data\raw\raw_emissions_data.csv", clear

* Keep key variables
keep statecode facilityname facilityid unitid date noxmass
rename statecode state
rename facilityname facility_name
rename facilityid facility_id
rename unitid unit_id
rename noxmass nox_mass

* Check that the data are uniquely identified as expected
gunique state facility_id unit_id date
assert r(unique) == _N

* Create stata date variables
gen date_stata = date(date, "YMD")
format date_stata %td
gen year = year(date_stata)
drop date

* Conver the NOx mass variable to numeric
replace nox_mass = "" if nox_mass == "NA"
destring nox_mass, replace

* Create NBP binary indicator = 1 for NBP states and 0 otherwise. Note that I 
* exclude Missouri from the list because the NBP did not begin operating there
* until 2007 and the problem set is focused on 2002 vs 2005.
gen nbp = 0 
foreach stabv in AL CT DE DC IL IN KY MD MA MI NJ NY NC OH PA RI SC TN VA WV {
	qui replace nbp = 1 if state == "`stabv'"
}

* Create indicator for eastern states, excluding states that were excluded in
* the paper (including Missouri, since the NBP did not begin operating there 
* until 2007). I use this indicator later when I run east vs west regressions
gen east = nbp
foreach stabv in WI IA MO GA MS ME NH VT AK HI {
	qui replace east = . if state == "`stabv'"
}

* Output
compress *
save "$dirpath_data\clean\NOx_data_cleaned.dta", replace

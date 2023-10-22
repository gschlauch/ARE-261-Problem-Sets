********************************************************************************
* ARE 261 Pset 1 - Joe's Half
* Author: Gary Schlauch
* Last updated: October 21, 2023
* Purpose: clean the raw data
********************************************************************************

* Initialize settings and filepaths
do "\\tsclient\Documents\github\ARE-261-Problem-Sets\Pset3\scripts\setup.do"

* Append raw data
local files: dir "$dirpath_data\raw" files "*.csv"
local i = 1
foreach file of local files {
	import delimited using "$dirpath_data\raw\\`file'", clear
	tempfile file`i'
	save `file`i''
	local i = `i' + 1
}
clear
forval i = 1/4 {
	append using `file`i''
}

* Keep key variables
keep state date nox* 
rename noxmassshorttons nox_mass
rename noxratelbsmmbtu nox_rate

* Create date variables
gen date_stata = date(date, "YMD")
format date_stata %td
gen year = year(date_stata)
drop date

* Create NBP binary indicator = 1 for NBP states and 0 otherwise
gen nbp = 0 
foreach stabv in AL CT DE DC IL IN KY MD MA MI MO NJ NY NC OH PA RI SC TN VA WV {
	qui replace nbp = 1 if state == "`stabv'"
}

* Create indicator for eastern states, excluding states that were excluded in
* the paper
gen east = nbp
foreach stabv in AK GA HI IA ME MS NH VT WI  {
	qui replace east = . if state == "`stabv'"
}

* Output
compress *
save "$dirpath_data\clean\NOx_data_cleaned.dta", replace

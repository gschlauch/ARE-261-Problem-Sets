******************************************************************************
* Purpose: analyze the cleaned data
******************************************************************************

* Initialize settings and filepaths
do "\\tsclient\Documents\github\ARE-261-Problem-Sets\Pset3\scripts\setup.do"

* Question 1 *******************************************************************

use "$dirpath_data\clean\NOx_data_cleaned.dta", clear

* Keep NBP participating states
keep if nbp == 1

* Collapse the emissions data by day-year
gcollapse (sum) nox_mass, by(year date_stata)

* Convert to thousands of tons
replace nox_mass = nox_mass / 1000

* Create day of week indicators
gen dow = dow(date_stata)
tab dow, gen(dow)

* Regress NOx emissions on 6 day-of-week indicators and a constant
reghdfe nox_mass dow1-dow3 dow5-dow7 dow4, noabs residuals(resid_nox_emit)
gen fit_nox_emit = resid_nox_emit + _b[_cons]

* Plot Figure 1
gen doy = doy(date_stata)
gsort year doy
twoway ///
	(line fit_nox_emit doy if year == 2002, /// 
		lcolor(blue) lpattern(dash)) ///
	(line fit_nox_emit doy if year == 2005, ///
		lcolor(blue) lpattern(solid) lwidth(thick)), ///
	xtitle("Day of Year") ///
	legend(order(1 "2002" 2 "2005") pos(5) ring(0) rows(2)) ///
	xlab(1 "Jan 1" 121 "May 1" 274 "Oct 1" 365 "Dec 31") ///
	ytitle("")
graph export "$dirpath_output\Figures\Fig1.png", replace


* Questions 2 and 3 **********************************************************

clear all
use "$dirpath_data\clean\NOx_data_cleaned.dta"

* Keep NBP participating states
keep if nbp == 1

* Restrict to the year 2005
keep if year == 2005

* Get the total emissions by date
gcollapse (sum) nox_mass, by(date_stata)
gen month = month(date_stata)

* Create treatment indicator = 1 during ozone season
gen summer =  inlist(month, 5, 6, 7, 8, 9)

* Create RD window indicators
gen rdwindow_1 = (inrange(date_stata, td(01may2005) - 30, td(01may2005) + 30))
gen rdwindow_2 = (inrange(date_stata, td(30sep2005) - 30, td(30sep2005) + 30))

* Create running variables cenetered at the cutoff values
gen runvar_minus_c1 = date_stata - td(01may2005)
gen runvar_minus_c1_sq = runvar_minus_c1^2

gen runvar_minus_c2 = date_stata - td(30sep2005)
gen runvar_minus_c2_sq = runvar_minus_c2^2

* Run the regressions for question 2
forvalues i = 1/2 {
	eststo: reghdfe nox_mass summer ///
		runvar_minus_c`i' runvar_minus_c`i'_sq ///
		if rdwindow_`i' == 1, noabs vce(robust)
	if `i' == 1 {
		estadd local cutoff "May 1"
	}
	else {
		estadd local cutoff "Sep. 30"
	}
}

* Run the regressions for question 3
forvalues i = 1/2 {
	eststo: reghdfe nox_mass summer ///
		runvar_minus_c`i' runvar_minus_c`i'_sq ///
		c.runvar_minus_c`i'#i.summer ///
		c.runvar_minus_c`i'_sq#i.summer ///
		if rdwindow_`i' == 1, noabs
	if `i' == 1 {
		estadd local cutoff "May 1"
	}
	else {
		estadd local cutoff "Sep. 30"
	}
}

* Create latex table containing the point estimates on summer
cd "$dirpath_output\Tables"
la var nox_mass "NOx"
la var summer "1(NBP Operating)"

local longnote "\emph{Notes}: The table dislpays estimates for the the effect of the NOx Budget Trading Program on average total daily NOx emissions. Columns 1--2 report the results using the polynomial regresion discontinuity, and Columns 3--4 report the results using the spline regression discontinuity. Columns 1 and 3 include the 30 days before and after May 1st in the sample, and Columns 2 and 4 include the 30 days before and after September 30th. Robust errors are in parentheses. * p<0.05, ** p<0.01, *** p<0.001"

esttab using "Table_RD_estimates.tex", replace ///
	title("Polynomial and Spline RD estimates for the effect of the NBP on NOx emissions \label{tab1}") ///
	label b(3) se(3) keep(summer) ///
	mgroups( ///
		"Polynomial RD" "Spline RD", pattern(1 0 1 0) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) ///
		erepeat(\cmidrule(lr){@span}) ///
		) ///
	substitute(\_ _ {l} {p{0.8\linewidth}}) wrap ///
	stats(cutoff, labels("Cutoff date") fmt(0)) booktabs ///
	nonotes addnotes("`longnote'")
eststo clear


* Question 4 ***************************************************************

use "$dirpath_data\clean\NOx_data_cleaned.dta", clear
keep if year == 2005 
keep if nbp == 1  
gen month = month(date_stata)
gen summer = inlist(month, 5, 6, 7, 8, 9)
gcollapse (sum) nox_mass, by(date_stata summer)

reghdfe nox_mass summer, noabs vce(robust)

local var1 summer
local b1 = _b[`var1']
local se1 = _se[`var1']

* Question 5 ***************************************************************
	
use "$dirpath_data\clean\NOx_data_cleaned.dta", clear
keep if east == 1 
gen month = month(date_stata)
gen summer = inlist(month, 5, 6, 7, 8, 9)
gcollapse (sum) nox_mass, by(date_stata year summer)
gen post = (year == 2005)
gen summerXpost = summer * post

reghdfe nox_mass summer post summerXpost, noabs vce(robust)

local var2 summerXpost
local b2 = _b[`var2']
local se2 = _se[`var2']
	
* Question 6 ***************************************************************
	
use "$dirpath_data\clean\NOx_data_cleaned.dta", clear
keep if year == 2005
drop if missing(east)
gen month = month(date_stata)
gen summer = inlist(month, 5, 6, 7, 8, 9)
gcollapse (sum) nox_mass, by(date_stata east summer)
gen summerXeast = summer * east

reghdfe nox_mass summer east summerXeast, noabs vce(robust)

local var3 summerXeast
local b3 = _b[`var3']
local se3 = _se[`var3']
	
* Question 7 ***************************************************************

use "$dirpath_data\clean\NOx_data_cleaned.dta", clear
drop if missing(east)
gen month = month(date_stata)
gen summer = inlist(month, 5, 6, 7, 8, 9)
gcollapse (sum) nox_mass, by(date_stata year east summer)
gen post = (year == 2005)
gen summerXeast = summer*east 
gen summerXpost = summer*post 
gen eastXpost = east*post
gen summerXeastXpost = summer*east*post 

reghdfe nox_mass summer post east summerXeast summerXpost ///
	eastXpost summerXeastXpost, noabs vce(robust)
	
local var4 summerXeastXpost
local b4 = _b[`var4']
local se4 = _se[`var4']
	
* Tabulate the estimates from Questions 4-7 **********************************
	
* Get the significance stars for the parameter estimate of interest in each 
* regression
forvalues i = 1/4 {
	
	local t_stat = `b`i'' / `se`i''
	local pval = 2 * ttail(e(df_r), abs(`t_stat'))
	if `pval' < 0.01 {
		local stars`i' = "***"
	} 
	else if `pval' < 0.05 {
		local stars`i' = "**"
	} 
	else if `pval' < 0.1 {
		local stars`i' = "*"
	} 
	else {
		local stars`i' = ""
	}
	
}

* Begin table
local own_file = 0
capture file close myfile
file open myfile using "$dirpath_output/tables/Table_cross-sectional_and_DiD_estimates.tex", write replace
if `own_file' == 1 {
file write myfile "\documentclass[12pt]{article}" _n
file write myfile "\usepackage{amsmath}" _n
file write myfile "\usepackage{tabularx}" _n
file write myfile "\usepackage{booktabs}" _n
file write myfile "\begin{document}" _n
file write myfile "\pagenumbering{gobble}" _n
file write myfile _n
}
file write myfile "\begin{table}[ht]" _n
file write myfile "\caption{Cross-sectional and DiD estimates of the effect of the NBP on NOx emissions}" _n 
file write myfile "\centering" _n
file write myfile "\normalsize" _n
file write myfile "\begin{tabular}{cccc}" _n
file write myfile "\toprule" _n
file write myfile "\centering" _n
file write myfile " (1) & (2) & (3) & (4) \\" _n
file write myfile "\midrule" _n


* Write the results
forvalues i = 1(1)4 {
	local b`i' = round(`b`i'', 0.001)
	file write myfile "`b`i''`stars`i''" _tab
	if `i' != 4 {
		file write myfile " &" _tab
	}
}
file write myfile "\\" _n

forvalues i = 1(1)4 {
	local se`i' = round(`se`i'', 0.001)
	file write myfile "(`se`i'')" _tab
	if `i' != 4 {
		file write myfile " &" _tab
	}
}
file write myfile "\\" _n

* End table
file write myfile "\bottomrule" _n
file write myfile "\end{tabular}" _n
file write myfile "\caption*{\footnotesize \emph{Notes:} The table dislpays estimates for the the effect of the NOx Budget Trading Program on average total daily NOx emissions. Columns 1 reports the estimated coefficient of interest from the cross-sectional specification in Question 4. Column 2 the estimated coefficient of interest from the Pre vs Post DiD in Question 5. Column 3 reports the estimated coefficient of interest from the East vs West DiD in Question 6. Finally, Column 4 reports the estimated coefficient of interest from the triple-difference specification in Question 7. Robust errors are in parentheses. * p<0.05, ** p<0.01, *** p<0.001}" _n
file write myfile "\label{table:CS_DiD}" _n
file write myfile "\end{table}" _n
if `own_file' == 1 {
file write myfile "\end{document}" _n
}
file close myfile
	
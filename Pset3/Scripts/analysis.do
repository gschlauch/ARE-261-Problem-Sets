********************************************************************************
* ARE 261 Pset 1 - Joe's Half
* Author: Gary Schlauch
* Last updated: October 21, 2023
* Purpose: analyze the cleaned data
********************************************************************************

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


* Questions 2 and 3 ************************************************************

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
gen nbp_operating =  inlist(month, 5, 6, 7, 8, 9)

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
	eststo: reghdfe nox_mass nbp_operating ///
		runvar_minus_c`i' runvar_minus_c`i'_sq ///
		if rdwindow_`i' == 1, noabs
	if `i' == 1 {
		estadd local cutoff "May 1"
	}
	else {
		estadd local cutoff "Sep. 30"
	}
}

* Run the regressions for question 3
forvalues i = 1/2 {
	eststo: reghdfe nox_mass nbp_operating ///
		runvar_minus_c`i' runvar_minus_c`i'_sq ///
		c.runvar_minus_c`i'#i.nbp_operating ///
		c.runvar_minus_c`i'_sq#i.nbp_operating ///
		if rdwindow_`i' == 1, noabs
	if `i' == 1 {
		estadd local cutoff "May 1"
	}
	else {
		estadd local cutoff "Sep. 30"
	}
}

* Create latex table containing the point estimates on nbp_operating
cd "$dirpath_output\Tables"
la var nox_mass "NOx"
la var nbp_operating "1(NBP Operating)"

local longnote "\emph{Notes}: The table dislpays estimates for the the estimated effect of the NOx Budget Trading Program on NOx emissions. Columns 1--2 report the results using the polynomial regresion discontinuity, and Columns 3--4 report the results using the spline regression discontinuity. Columns 1 and 3 include the 30 days before and after May 1st in the sample, and Columns 2 and 4 include the 30 days before and after September 30th. Standard errors are in parentheses. * p<0.05, ** p<0.01, *** p<0.001"

esttab using "Table_RD_estimates.tex", replace ///
	title("Polynomial and Spline RD Results \label{tab1}") ///
	label b(3) se(3) keep(nbp_operating) ///
	mgroups( ///
		"Polynomial RD" "Spline RD", pattern(1 0 1 0) span ///
		prefix(\multicolumn{@span}{c}{) suffix(}) ///
		erepeat(\cmidrule(lr){@span}) ///
		) ///
	substitute(\_ _ {l} {p{0.8\linewidth}}) wrap ///
	stats(cutoff, labels("Cutoff date") fmt(0)) booktabs ///
	nonotes addnotes("`longnote'")
eststo clear


	
	
	
	
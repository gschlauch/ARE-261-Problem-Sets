
global dirpath = "\\tsclient\Documents\github\ARE-261-Problem-Sets\Pset1"
global dirpath_data = "$dirpath\data"

* 2 Hedonic air quality analysis -----------------------------------------------

use "$dirpath_data/raw/poll7080", clear

drop dhghwy dwelfr dhlth deduc

* Drop if any variables are msising
foreach var of varlist * {
	qui drop if missing(`var')
}

local econ_shocks "dincome dunemp dmnfcg"
local controls "ddens dwhite dfeml dage65 dhs dcoll durban dpoverty dvacant downer dplumb drevenue dtaxprop depend"

* 1
reg dlhouse dgtsp [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_1_1.csv", replace
restore

reg dlhouse dgtsp `econ_shocks' `controls' [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "Yes"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_1_2.csv", replace
restore

reg dgtsp dincome dunemp dmnfcg [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_1_3.csv", replace
restore
corr dgtsp dincome dunemp dmnfcg

reg dlhouse dincome dunemp dmnfcg [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_1_4.csv", replace
restore
corr dlhouse dincome dunemp dmnfcg

* 2
// Regress regulatory status on economic shcoks
reg tsp7576 `econ_shocks' [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_2.csv", replace
restore

* 3
// first stage
reg dgtsp tsp7576 [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_3_1.csv", replace
restore

reg dgtsp tsp7576 `econ_shocks' `controls' [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "Yes"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_3_2.csv", replace
restore

// reduced form
reg dlhouse tsp7576 [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_3_3.csv", replace
restore

reg dlhouse tsp7576 `econ_shocks' `controls' [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "Yes"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_3_4.csv", replace
restore

// 2SLS
ivreg2 dlhouse (dgtsp = tsp7576) [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_3_5.csv", replace
restore

ivreg2 dlhouse (dgtsp = tsp7576) `econ_shocks' `controls' [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "Yes"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_3_6.csv", replace
restore

* 4

// first stage
reg dgtsp mtspgm74 [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_4_1.csv", replace
restore

reg dgtsp mtspgm74 `econ_shocks' `controls' [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "Yes"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_4_2.csv", replace
restore

// reduced form
reg dlhouse mtspgm74 [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_4_3.csv", replace
restore

reg dlhouse mtspgm74 `econ_shocks' `controls' [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "Yes"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_4_4.csv", replace
restore

// 2SLS
ivreg2 dlhouse (dgtsp = mtspgm74) [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_4_5.csv", replace
restore

ivreg2 dlhouse (dgtsp = mtspgm74) `econ_shocks' `controls' [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "Yes"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_4_6.csv", replace
restore


* 5
gen mtspgm74_A75 = (mtspgm74 > 75)

local bwidth = 8
local b = `bwidth' / 10
local varlist dgtsp dlhouse
foreach var of local varlist {
		
	if "`var'" == "dgtsp" {
		local ytitle = "Change in annual mean TSPs from 1969-72 to 1977-80"
	}
	if "`var'" == "dlhouse" {
		local ytitle = "Change in log housing values, 1970-1980"
	}
	
	twoway ///
		(lowess `var' mtspgm74 if mtspgm74_A75 == 1, bwidth(`b') lc(black)) ///
		(lowess `var' mtspgm74 if mtspgm74_A75 == 0, bwidth(`b') lc(black)), ///
		xline(75) ///
		graphregion(fcolor(white)) ///
		legend(off) ///
		ytitle("`ytitle'", size(small)) ///
		xtitle("TSPs level in 1974", size(small))
	graph export "$dirpath/output/figures/lowess_5_`var'_bwidth`bwidth'.png", replace

}


* 6
reg dlhouse `econ_shocks' `controls' [aweight = pop7080], robust
predict dlhouse_hat, xb
local bwidth = 8
local b = `bwidth' / 10
twoway ///
	(lowess dlhouse mtspgm74 if mtspgm74_A75 == 1, bwidth(`b') lc(blue)) ///
	(lowess dlhouse mtspgm74 if mtspgm74_A75 == 0, bwidth(`b') lc(blue)) ///
	(lowess dlhouse_hat mtspgm74 if mtspgm74_A75 == 1, bwidth(`b') lc(red)) ///
	(lowess dlhouse_hat mtspgm74 if mtspgm74_A75 == 0, bwidth(`b') lc(red)), ///
	xline(75, lc(black)) ///
	graphregion(fcolor(white)) ///
	legend(order(1 "Actual prices" 3 "Index")) ///
	ytitle("`ytitle'", size(small)) ///
	xtitle("TSPs level in 1974", size(small))
graph export "$dirpath/output/figures/lowess_6_bwidth`bwidth'.png", replace


* 7
gen mtspgm74_50to75 = (mtspgm74 < 75 & mtspgm74 >= 50)
local bwidth = 8
local b = `bwidth' / 10
local varlist dgtsp dlhouse
foreach var of local varlist {
		
	if "`var'" == "dgtsp" {
		local ytitle = "Change in annual mean TSPs from 1969-72 to 1977-80"
	}
	if "`var'" == "dlhouse" {
		local ytitle = "Change in log housing values, 1970-1980"
	}
	
	twoway ///
		(lowess `var' mtspgm74 if mtspgm74_50to75 == 1 & tsp7576 == 0, ///
			bwidth(`b') lc(blue)) ///
		(lowess `var' mtspgm74 if mtspgm74_50to75 == 1 & tsp7576 == 1, ///
			bwidth(`b') lc(red)), ///
		xline(75, lc(black)) ///
		graphregion(fcolor(white)) ///
		legend(order(1 "Unregulated" 2 "Regulated")) ///
		ytitle("`ytitle'", size(small)) ///
		xtitle("TSPs level in 1974", size(small))
	graph export "$dirpath/output/figures/lowess_7_`var'_bwidth`bwidth'.png", replace
	
	
}
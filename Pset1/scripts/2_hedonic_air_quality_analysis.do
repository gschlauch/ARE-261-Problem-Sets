
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
corr dgtsp dgtsp dincome dunemp dmnfcg

reg dlhouse dincome dunemp dmnfcg [aweight = pop7080], robust
preserve
regsave, tstat pval ci
gen depvar =  "`=e(depvar)'"
gen controls = "No"
gen weights = "Yes"
export delimited "$dirpath_data\intermediate\reg_hedonic_1_4.csv", replace
restore
corr dlhouse dgtsp dincome dunemp dmnfcg

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

local varlist dgtsp //dlhouse
foreach var of local varlist {
	foreach bwidth of numlist 2 {
		
		local b = `bwidth' / 10
		
		if "`var'" == "dgtsp" {
			local ytitle = "Change in annual mean TSPs from 1969-72 to 1977-80"
		}
		if "`var'" == "dlhouse" {
			local ytitle = "Change in log housing values, 1970-1980"
		}
		
		twoway ///
			(lowess `var' mtspgm74 if mtspgm74_A75 == 1, lc(black)) ///
			(lowess `var' mtspgm74 if mtspgm74_A75 == 0, lc(black)), ///
			xline(75) ///
			graphregion(fcolor(white)) ///
			legend(off) ///
			ytitle("`ytitle'", size(small)) ///
			xtitle("TSPs level in 1974", size(small))

	}
}

local varlist dgtsp dlhouse
foreach var of local varlist {
	foreach bwidth of numlist 2 3 4 {
		
		local b = `bwidth' / 10
		
		lowess `var' mtspgm74 if mtspgm74_A75 == 1, ///
			gen(`var'1) bwidth(`b') nograph
		lowess `var' mtspgm74 if mtspgm74_A75 == 0, ///
			gen(`var'0) bwidth(`b') nograph
			
		assert (missing(`var'1) == (mtspgm74_A75 == 0))
		assert (missing(`var'0) == (mtspgm74_A75 == 1))
		gen `var'_bwidth`bwidth' = `var'1
		replace `var'_bwidth`bwidth' = `var'0 if tsp7576 == 0
		drop `var'1 `var'0

	}
}

preserve
keep dgtsp* dlhouse* mtspgm74 mtspgm74_A75
export delimited "$dirpath_data\intermediate\lowess_hedonic_5.csv", replace
restore


* 6
reg dlhouse `econ_shocks' `controls' [aweight = pop7080], robust
predict dlhouse_hat, xb

foreach bwidth of numlist 2 3 4 {
	
	local b = `bwidth' / 10
	
	lowess dlhouse_hat mtspgm74 if mtspgm74_A75 == 1, ///
		gen(index1_bwidth`bwidth') bwidth(`b') nograph
	lowess dlhouse_hat mtspgm74 if mtspgm74_A75 == 0, ///
		gen(index0_bwidth`bwidth') bwidth(`b') nograph
		
	assert (missing(index1_bwidth`bwidth') == (mtspgm74_A75 == 0))
	assert (missing(index0_bwidth`bwidth') == (mtspgm74_A75 == 1))
	gen index_bwidth`bwidth' = index1_bwidth`bwidth'
	replace index_bwidth`bwidth' = index0_bwidth`bwidth' if tsp7576 == 0
	drop index1_bwidth`bwidth' index0_bwidth`bwidth'
}

preserve
keep index* mtspgm74 mtspgm74_A75
export delimited "$dirpath_data\intermediate\lowess_hedonic_6.csv", replace
restore


* 7
gen mtspgm74_50to75 = (mtspgm74 < 75 & mtspgm74 >= 50)
local varlist dgtsp dlhouse
foreach var of local varlist {
	foreach bwidth of numlist 7 8 9 {
		
		local b = `bwidth' / 10
	
		lowess `var' mtspgm74 if mtspgm74_50to75 == 1 & tsp7576==1, ///
			gen(`var'1) bwidth(`b') nograph
		lowess `var' mtspgm74 if mtspgm74_50to75 == 1 & tsp7576==0, ///
			gen(`var'0) bwidth(`b') nograph
			
		gen `var'_50to75_bwidth`bwidth' = `var'1
		replace `var'_50to75_bwidth`bwidth' = `var'0 if tsp7576 == 0
		drop `var'1 `var'0
	
	}
}

preserve
keep dgtsp dlhouse mtspgm74 mtspgm74_50to75 tsp7576 *50to75*
export delimited "$dirpath_data\intermediate\lowess_hedonic_7.csv", replace
restore

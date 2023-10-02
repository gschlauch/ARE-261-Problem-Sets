
	global dirpath = "\\tsclient\Documents\github\ARE-261-Problem-Sets\Pset1"
	global dirpath_data = "$dirpath\data"

	* 2 Hedonic air quality analysis -----------------------------------------------

	use "$dirpath_data/raw/poll7080", clear

	* Drop if any variables are missing
	foreach var of varlist * {
		qui drop if missing(`var')
	}
	local controls "ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty dvacant downer dplumb drevenue dtaxprop depend"

	* 1
	reg dlhouse dgtsp [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_1_1.csv", replace
	restore

	reg dlhouse dgtsp `controls' [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_1_2.csv", replace
	restore

	reg dgtsp dincome dunemp dmnfcg [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_1_3.csv", replace
	restore
	corr dlhouse dgtsp dincome dunemp dmnfcg

	* 2
	// Check relevance, ie the existence of a first stage relationship
	reg dgtsp tsp7576 `controls' [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_2_1.csv", replace
	restore

	// "Check" exclusion, ie if the instrument is uncorrelated with other 
	// observable determinants of housing prices
	reg tsp7576 `controls' [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_2_2.csv", replace
	restore

	* 3
	// first stage
	reg dgtsp tsp7576 [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_3_1.csv", replace
	restore

	reg dgtsp tsp7576 `controls' [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_3_2.csv", replace
	restore

	// reduced form
	reg dlhouse tsp7576 [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_3_3.csv", replace
	restore

	reg dlhouse tsp7576 `controls' [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_3_4.csv", replace
	restore

	// 2SLS
	ivreg2 dlhouse (dgtsp = tsp7576) [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_3_5.csv", replace
	restore

	ivreg2 dlhouse (dgtsp = tsp7576) `controls' [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_3_6.csv", replace
	restore

	* 4
	// create the instrument for pollution using mtspgm74
	gen regulation_iv = (mtspgm74 > 75)

	// first stage
	reg dgtsp regulation_iv [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_4_1.csv", replace
	restore

	reg dgtsp regulation_iv `controls' [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_4_2.csv", replace
	restore

	// reduced form
	reg dlhouse regulation_iv [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_4_3.csv", replace
	restore

	reg dlhouse regulation_iv `controls' [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_4_4.csv", replace
	restore

	// 2SLS
	ivreg2 dlhouse (dgtsp = regulation_iv) [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_4_5.csv", replace
	restore

	ivreg2 dlhouse (dgtsp = regulation_iv) `controls' [aweight = pop7080]
	preserve
	regsave, tstat pval ci
	gen depvar =  "`=e(depvar)'"
	export delimited "$dirpath_data\intermediate\reg_hedonic_4_6.csv", replace
	restore


* 5
local varlist dgtsp dlhouse
foreach var of local varlist {
	foreach bwidth of numlist 2 3 4 {
		
		local b = `bwidth' / 10
		
		lowess `var' mtspgm74 if tsp7576 == 1, ///
			gen(`var'1) bwidth(`b') nograph
		lowess `var' mtspgm74 if tsp7576 == 0, ///
			gen(`var'0) bwidth(`b') nograph
			
		assert (missing(`var'1) == (tsp7576 == 0))
		assert (missing(`var'0) == (tsp7576 == 1))
		gen `var'_bwidth`bwidth' = `var'1
		replace `var'_bwidth`bwidth' = `var'0 if tsp7576 == 0
		drop `var'1 `var'0

	}
}

preserve
keep dgtsp* dlhouse* mtspgm74 tsp7576
export delimited "$dirpath_data\intermediate\lowess_hedonic_5.csv", replace
restore


* 6
reg dlhouse `controls' [aweight = pop7080]
predict dlhouse_hat, xb

foreach bwidth of numlist 2 3 4 {
	
	local b = `bwidth' / 10
	
	lowess dlhouse_hat mtspgm74 if tsp7576 == 1, ///
		gen(index1_bwidth`bwidth') bwidth(`b') nograph
	lowess dlhouse_hat mtspgm74 if tsp7576 == 0, ///
		gen(index0_bwidth`bwidth') bwidth(`b') nograph
		
	assert (missing(index1_bwidth`bwidth') == (tsp7576 == 0))
	assert (missing(index0_bwidth`bwidth') == (tsp7576 == 1))
	gen index_bwidth`bwidth' = index1_bwidth`bwidth'
	replace index_bwidth`bwidth' = index0_bwidth`bwidth' if tsp7576 == 0
	drop index1_bwidth`bwidth' index0_bwidth`bwidth'
}

preserve
keep index* mtspgm74 tsp7576
export delimited "$dirpath_data\intermediate\lowess_hedonic_6.csv", replace
restore


* 7
gen mtspgm74_50to75 = (mtspgm74 < 75 & mtspgm74 >= 50)
local bwidth = 6
local b = `bwidth' / 10

local varlist dgtsp dlhouse
foreach var of local varlist {
	
	lowess `var' mtspgm74 if mtspgm74_50to75 == 1 & tsp7576==1, ///
		gen(`var'1) bwidth(`b') nograph
	lowess `var' mtspgm74 if mtspgm74_50to75 == 1 & tsp7576==0, ///
		gen(`var'0) bwidth(`b') nograph
		
	gen `var'_50to75_bwidth`bwidth' = `var'1
	replace `var'_50to75_bwidth`bwidth' = `var'0 if tsp7576 == 0
	drop `var'1 `var'0
	
}

preserve
keep dgtsp dlhouse mtspgm74 mtspgm74_50to75 tsp7576 *50to75*
export delimited "$dirpath_data\intermediate\lowess_hedonic_7.csv", replace
restore

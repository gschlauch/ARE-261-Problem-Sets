
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
foreach bwidth of numlist 2 3 4 {
	
	lowess dgtsp mtspgm74 if regulation_iv == 1, ///
		gen(dgtsp1_bwidth`bwidth') bwidth(`bwidth') nograph
	lowess dgtsp mtspgm74 if regulation_iv == 0, ///
		gen(dgtsp0_bwidth`bwidth') bwidth(`bwidth') nograph

	lowess dlhouse mtspgm74 if regulation_iv == 1, ///
		gen(dlhouse1_bwidth`bwidth') bwidth(`bwidth') nograph
	lowess dlhouse mtspgm74 if regulation_iv == 0, ///
		gen(dlhouse0_bwidth`bwidth') bwidth(`bwidth') nograph
}

preserve
keep dgtsp* dlhouse* mtspgm74 regulation_iv
export delimited "$dirpath_data\intermediate\lowess_hedonic_5.csv", replace
restore


* 6
reg dlhouse `controls' [aweight = pop7080]
predict dlhouse_hat, xb
lowess dlhouse_hat mtspgm74 if regulation_iv == 0, ///
	gen(index0) bwidth(2) nograph
lowess dlhouse_hat mtspgm74 if regulation_iv == 1, ///
	gen(index1) bwidth(2) nograph
preserve
keep index* mtspgm74 regulation_iv
export delimited "$dirpath_data\intermediate\lowess_hedonic_6.csv", replace
restore

* 7
gen regulation2 = (mtspgm74 < 75 & mtspgm74 >= 50)
local bwidth = 6

lowess dgtsp mtspgm74 if regulation2 == 1 & tsp7576==1, ///
	gen(dgtsp_reg) bwidth(`bwidth') nograph
lowess dgtsp mtspgm74 if regulation2 == 1 & tsp7576==0, ///
	gen(dgtsp_noreg) bwidth(`bwidth') nograph

lowess dlhouse mtspgm74 if regulation2 == 1 & tsp7576==1, ///
	gen(dlhouse_reg) bwidth(`bwidth') nograph
lowess dlhouse mtspgm74 if regulation2 == 1 & tsp7576==0, ///
	gen(dlhouse_noreg) bwidth(`bwidth') nograph

preserve
keep dgtsp dlhouse mtspgm74 regulation2 tsp7576 *_reg *_noreg
export delimited "$dirpath_data\intermediate\lowess_hedonic_7.csv", replace
restore

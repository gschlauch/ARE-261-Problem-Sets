
global dirpath = "\\tsclient\Documents\Materials\ARE261\Reed\pset1"
global dirpath_data = "$dirpath\data"

* 2 Hedonic air quality analysis -----------------------------------------------

use "$dirpath_data/raw/poll7080", clear
foreach var of varlist * {
	di "`var'"
	count if missing(`var')
	di "------------------"
}
local controls "ddens dmnfcg dwhite dfeml dage65 dhs dcoll durban dunemp dincome dpoverty dvacant downer dplumb drevenue dtaxprop depend"


* 1
reg dlhouse dgtsp [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_1_1.csv", replace
restore

reg dlhouse dgtsp `controls' [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_1_2.csv", replace
restore


* 2
// Check relevance, ie the existence of a first stage relationship
reg dgtsp tsp7576 `controls' [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_2_1.csv", replace
restore

// "Check" exclusion, ie if the instrument is uncorrelated with other 
// observable determinants of housing prices
reg tsp7576 `controls' [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_2_2.csv", replace
restore


* 3
// first stage
reg dgtsp tsp7576 [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_1.csv", replace
restore

reg dgtsp tsp7576 `controls' [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_2.csv", replace
restore

// reduced form
reg dlhouse tsp7576 [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_3.csv", replace
restore

reg dlhouse tsp7576 `controls' [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_4.csv", replace
restore

// 2SLS
ivreg2 dlhouse (dgtsp = tsp7576) [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_5.csv", replace
restore

ivreg2 dlhouse (dgtsp = tsp7576) `controls' [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_6.csv", replace
restore


* 4
// create the instrument for pollution using mtspgm74
gen regulation_iv = (mtspgm74 > 75)

// first stage
reg dgtsp regulation_iv [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_1.csv", replace
restore

reg dgtsp regulation_iv `controls' [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_2.csv", replace
restore

// reduced form
reg dlhouse regulation_iv [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_3.csv", replace
restore

reg dlhouse regulation_iv `controls' [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_4.csv", replace
restore

// 2SLS
ivreg2 dlhouse (dgtsp = regulation_iv) [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_5.csv", replace
restore

ivreg2 dlhouse (dgtsp = regulation_iv) `controls' [aweight = pop7080]
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_hedonic_3_6.csv", replace
restore


* 5






global dirpath = "\\tsclient\Documents\Materials\ARE261\Reed\pset1"
global dirpath_data = "$dirpath\data"

* 1.2 Climate Impacts -----------------------------------------------

use "$dirpath_data\raw\reis_combine", clear
destring fips, replace
merge 1:1 fips year using "$dirpath_data\raw\CountyAnnualTemperature1950to2012", nogen keep(3)

* 1
gen log_emp_farm = log(emp_farm)
local temp_bins "tempB0 temp0to4 temp4to8 temp8to12 temp12to16 o.temp16to20 temp20to24 temp24to28 temp28to32 tempA32"
reghdfe log_emp_farm `temp_bins', absorb(fips year)
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_climate_impacts_1.csv", replace
restore

* 2
gen log_farm_prop_income_pc = log(inc_farm_prop_income/pop_population)
reghdfe log_farm_prop_income_pc spline*, absorb(fips year)
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_climate_impacts_2.csv", replace
restore

* 3
local temp_bins "tempB0 temp0to4 temp4to8 temp8to12 temp12to16 o.temp16to20 temp20to24 temp24to28 temp28to32 tempA32"
local temp_bins_interacted "`temp_bins' c.tempB0#c.tempA32 c.temp0to4#c.tempA32 c.temp4to8#c.tempA32 c.temp8to12#c.tempA32 c.temp12to16#c.tempA32 c.temp20to24#c.tempA32 c.temp24to28#c.tempA32 c.temp28to32#c.tempA32 c.tempA32#c.tempA32 "
reghdfe log_emp_farm `temp_bins_interacted', absorb(fips year)
preserve
regsave, tstat pval ci
export delimited "$dirpath_data\intermediate\reg_climate_impacts_3.csv", replace
restore


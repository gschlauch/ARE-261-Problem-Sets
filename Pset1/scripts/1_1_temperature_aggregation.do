global dirpath = "\\tsclient\Documents\github\ARE-261-Problem-Sets\Pset1"
global dirpath_data = "$dirpath\data"

* 1.1 Temperature Aggregation -----------------------------------------------

use "$dirpath_data\raw\fips1001", clear

foreach var of varlist * {
	assert !missing(`var')
}

gen tAvg = (tMin+tMax) / 2
gen year = yofd(dateNum)

* a)
// Following Schlenker and Roberts (2009), create 3 separate degree-day 
// measures, calculating degree days above 30, 32, and 34C
// note: I obtained a template of this code from Wolfram Schlenker's website
local boundList 30 32 34
foreach b of local boundList {
	// create label for negative bounds by adding Minus
	if (`b' < 0) {
		local b2 = abs(`b')
		local bLabel Minus`b2'
	} 
	else {
		local bLabel `b'
	}

	// default case 1: tMax <= bound
	qui gen dday`bLabel' = 0

	// case 2: bound <= tMin
	qui replace dday`bLabel' = tAvg - `b' if (`b' <= tMin)

	// case 3: tMin < bound < tMax
	qui gen tempSave = acos( (2 * `b'- tMax - tMin) / (tMax - tMin) )
	qui replace dday`bLabel' = ///
		((tAvg - `b') * tempSave + (tMax - tMin) * sin(tempSave)/2) / _pi if ///
		((tMin < `b') & (`b' < tMax))
	drop tempSave
}

* b)
// Create binned temperature variables spanning 4 degree C bins: Below 0, 0-4, 
// 4-8, 8-12, 12-16, 16-20, 20-24, 24-28, 28-32, Above 32
foreach var of varlist tMin tMax tAvg {
	
	gen `var'B0 = (`var' < 0)
	foreach i of numlist 0(4)28 {
		local lower = `i'
		local upper = `i' + 4
		gen `var'`lower'to`upper' = (`var' >= `lower' & `var' < `upper')
	}
	gen `var'A32 = (`var' >= 32)

}

* c)
// Create restricted cubic splines with knots at 0 8 16 24 and 32
local knotList 0 8 16 24 32
foreach var of varlist tMin tMax tAvg {
	mkspline `var'sp = `var' , cubic knots(`knotList')
}

* d)
// Create piecewise linear functions, with breakpoints at 28 and 32.
foreach var of varlist tMin tMax tAvg {
	mkspline `var'1 28 `var'2 32 `var'3 = `var' , marginal
}

* 2
// Sum over these variables during the year, and then take the unweighted 
// mean across grid cells within the county
gcollapse (sum) tMin* tMax* tAvg* dday*, by(year gridNum)
gcollapse (mean) tMin* tMax* tAvg* dday*, by(year)

* 3
// Check that you are able to replicate the variables for Autauga County 
// (Fips 01001) in the following dataset: 
// CountyAnnualTemperature1950to2012.dta.
gen fips = 1001
merge 1:1 fips year using "$dirpath_data\CountyAnnualTemperature1950to2012.dta", keep(3) nogen

// Compare degree days
foreach i of numlist 30 32 34 {
	assert round(dday`i', 0.001) == round(dday`i'C, 0.001)
}

// Compare bins
local binList "B0" "0to4" "4to8" "8to12" "12to16" "16to20" "20to24" ///
	"24to28" "28to32" "A32"
foreach bin of local binlist {
	assert tAvg`bin' == temp`bin'
}

// Compare cubic splines
foreach i of numlist 1(1)4 {
	assert round(tAvgsp`i', 0.001) == round(splineC`i', 0.001)
}

// Compare piecewise linear
assert round(tAvg2, 0.001) == round(piece28, 0.001)
assert round(tAvg3, 0.001) == round(piece32, 0.001)

* Output
compress *
save "$dirpath_data\intermediate\Autauga_County_temperature_aggregation.dta", replace


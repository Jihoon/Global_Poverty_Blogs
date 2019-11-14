/*==================================================
project:       GIC with longest period for each comparable spell
Author:        R.Andres Castaneda & Christoph Lakner
E-email:       acastanedaa@worldbank.org
url:           
Dependencies:  The World Bank
----------------------------------------------------
Creation Date:    16 Oct 2019 - 20:49:38
Modification Date:   
Do-file version:    01
References:          
Output:             
==================================================*/

/*==================================================
0: Program set up
==================================================*/
version 14
drop _all


cap findfile scheme-plotplainblind.scheme
if (_rc) ssc install blindschemes

set scheme plotplainblind

//========================================================
//  User inputs
//========================================================

local country_list "ARG THA GHA"  // change this
local year_range "1990/2020"      // change this as a numlist (help nlist)


/*==================================================
1:  Load data and merge with metadata
==================================================*/

*----------1.1:

local dh "https://development-data-hub-s3-public.s3.amazonaws.com/"
import delimited using "`dh'ddhfiles/506801/povcalnet_comparability.csv", clear
tempfile metadata
save `metadata'

povcalnet, clear
merge 1:1 countrycode year coveragetype datatype using "`metadata'"


*----------1.2: keep relevant observations

* countries
local country_list: subinstr local country_list " " "|", all // do NOT change this
keep if regexm(countrycode, "`country_list'")

* Years
numlist "`year_range'"
local year_range = "`r(numlist)'"
local year_range: subinstr local year_range " " "|", all // do NOT change this
keep if regexm(strofreal(year), "`year_range'")

label var year "Year"
//------------find longest spell 

cap drop spell
gen spell = .

levelsof countrycode, local(countries)
levelsof comparability, local(breaks)

qui foreach country of local countries {
	
	foreach break of local breaks {
		
		local c_cb `" countrycode == "`country'" & comparability == `break' "' // condition country and break
		count if (`c_cb')
		if (r(N) == 0) continue  // skip if combination does not exist
		
		// Legend
		sum year if (`c_cb'), meanonly
		replace spell =  r(max) - r(min)  if (`c_cb')
		
	}
}

bysort countrycode: egen mcom = max(spell)               // max count of comparability
keep if spell == mcom   // keep longest spell

/*==================================================
2: calculate GIC
==================================================*/

*----------2.1: reshape and format
reshape long decile, i(countrycode year) j(dec)
egen panelid=group(countrycode dec)
xtset panelid year

replace dec=10*dec
replace decile=10*decile*mean

*----------2.2: create GIC
levelsof countrycode, local(countries)
gen g = .
gen m = .
foreach country of local countries {
	local condif `"countrycode == "`country'""'
	sum spell if `condif', meanonly
	local sp = r(min)
	replace g =(((decile/L`sp'.decile)^(1/`sp'))-1)*100	 if `condif'
	replace m =(((mean/L`sp'.mean)^(1/`sp'))-1)*100	 if `condif'
}

label var g "Annualized GIC"
label var m "Annualized growth of the mean"


/*==================================================
3:  Chart
==================================================*/



*----------3.1: parametter
local colors "sky turquoise reddish vermillion sea orangebrown" // help plotplainblind
local pattern "solid" // help linepatternstyle
local symbol "O"  // help symbolstyle

*----------3.2:
levelsof countrycode, local(countries)

local c = 0
global gic_lines ""
global glegend ""
foreach country of local countries {
	local ++c
	local color: word `c' of `colors'
	
	local condif `"countrycode == "`country'""'
	sum m if (`condif')
	local og = r(mean)
	
	// line
	local gline (sc g dec if (`condif'), c(l) lpattern(`pattern') /* 
	 */ lcolor(`color') mcolor(`color')  msymbol(`symbol') /* 
	  */ yline(`og', lcolor(`color'%70)) )
	global gic_lines "${gic_lines} `gline'"
	
	// Legend
	levelsof countryname if (`condif'), local(countryname) clean
	
	sum year if (`condif'), meanonly
	local min = r(min)
	local max = r(max)
	global glegend = `" ${glegend} `c' "`countryname' (`min'-`max')" "'
	
}

*----------3.3: Plot
*##s
sum g, meanonly
local mg = r(max)*.95

twoway ${gic_lines}, legend(order(${glegend}) rows(1) pos(6)) /* 
 */ ytitle("Annual growth in decile average income (%)",  size(small))  /* 
 */	xtitle("Decile group",  size(small)) text(`mg' 80 /* 
 */ "Horizontal lines stand for" "annualized mean growth", placement(e) /* 
	 */  color(gs9) size(vsmall))

*##e



exit
/* End of do-file */

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1.
2.
3.


Version Control:

* to send for publication in blog
keep countrycode countryname m g dec
drop if missing(g,m)
gen period = ""
replace period = cond(countrycode == "ARG", "2003-2017", /* 
             */  cond(countrycode == "GHA", "1991-2016", "2000-2013"))

reshape wide g m, i(countryname period) j(dec)
drop countrycode

preserve 

keep countryname period g*
rename g* d*
gen measure = "GIC"
tempfile gic
save `gic'

restore

keep countryname period m*
rename m* d*
gen measure = "Annualized growoth of the mean"

append using `gic'


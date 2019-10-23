/*==================================================
project:       Show different trends of the same country
Author:        R.Andres Castaneda 
E-email:       acastanedaa@worldbank.org
url:           
Dependencies:  The World Bank
----------------------------------------------------
Creation Date:    16 Oct 2019 - 18:33:35
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
local rawcontent "https://raw.githubusercontent.com/"
local metadata "`rawcontent'worldbank/povcalnet/master/metadata/povcalnet_metadata.dta"
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


//------------ scale gini
replace gini = gini * 100
format gini %2.0f


/*==================================================
          2:  create lines of code
==================================================*/

*----------2.1: parameters
// New country and break variable
cap drop code_break 
gen code_break = countrycode + strofreal(comparability) 


*##s
levelsof countrycode, local(countries)
levelsof comparability, local(breaks)

local colors "sky turquoise reddish vermillion sea orangebrown" // help plotplainblind
local patterns "shortdash  solid dash dash_dot longdash  dot shortdash_dot" // help linepatternstyle
local symbols "O T + S X"  // help symbolstyle


*----------2.2:
global two_lines ""
global legend    ""
local c = 0   // country counter
local i = 0   // chart counter
qui foreach country of local countries {
	local ++c
	local color: word `c' of `colors'
	local symbol: word `c' of `symbols'
	
	foreach break of local breaks {
		
		local c_cb `" countrycode == "`country'" & comparability == `break' "' // condition country and break
		count if (`c_cb')
		if (r(N) == 0) continue  // skip if combination does not exist
		
		// info
		local ++i
		local b = `break' + 1
		local pattern: word `b' of `patterns'
		
		// Legend
		levelsof countryname if (`c_cb'), local(countryname) clean
		
		sum year if (`c_cb'), meanonly
		local min = r(min)
		local max = r(max)
		global legend = `" ${legend} `i' "`countryname' (`min'-`max')" "'
			
		// line of code
		local line  (scatter gini year if code_break=="`country'`break'",   /* 
		  */  c(l) lpattern(`pattern') lcolor(`color') mcolor(`color')  msymbol(`symbol'))
		global two_lines `"${two_lines}  `line'"'
		
	}
}

/*==================================================
          3:  Plot chart
==================================================*/

*----------3.1:
twoway ${two_lines}, legend(order(${legend}) rows(3) pos(6))

//------------save chart


*##e




exit
/* End of do-file */

><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

Notes:
1.
2.
3.


Version Control:



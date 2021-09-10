******************
*** READ BELOW ***
******************
/*
This .do-file takes all comparable survey spells in PovcalNet
and calculates the distribution of annualized changes in the Gini as a function of the spell length
*/

cd "C:\Users\wb514665\OneDrive - WBG\Research\Twinning\JOEI_r&r"
graph set window fontface "Palatino Linotype"
global color0 "black"
global color1 "gs4"
global color2 "gs8"
global color3 "gs12"
global color4 "gs14"



***********************************
*** LOAD COMPARABILITY DATABASE ***
***********************************
import excel "RawDataSources\povcalnet_comparability.xlsx", sheet("Sheet1") firstrow clear
tempfile comparability
save `comparability'

******************************
*** LOAD/KEEP SURVEY DATA ***
******************************
*povcalnet, country(all) year(all)  clear
*save "RawDataSources\PovcalNet_surveydata.dta", replace
use  "RawDataSources\PovcalNet_surveydata.dta", clear
merge 1:1 countrycode coveragetype datatype year using `comparability', nogen keep(1 3)
// Add missing Gini's
merge 1:1 countrycode coveragetype datatype datayear using "ProcessedData/RecoveredMedians.dta", nogen
replace gini = gini_recovered if missing(gini)
keep if inlist(coveragetype,3,4) | countrycode=="ARG"
isid countrycode year datatype
keep countrycode datatype datayear comparability gini
mdesc
tempfile ginis
save `ginis'

**********************************
*** FIND ALL COMPARABLE SPELLS ***
**********************************
bysort countrycode datatype: gen N=_N
qui sum N
forvalues i=1/`=`r(max)'-1' {
bysort countrycode datatype (datayear): gen take`i' = datayear[_n+`i'] if comparability==comparability[_n+`i']
qui mdesc take`i'
if `r(percent)'==100 {
drop take`i'
}
}
// Drop rows without any comparable spells
egen spells = rownonmiss(take*)
drop if spells==0
drop comparability N spells
order countrycode datatype datayear

reshape long take, i(countrycode datatype datayear) j(spell)
drop if missing(take)
drop spell
rename take endspell
rename datayear startspell
label var startspell "Start of spell"
label var endspell "End of spell"

// Two types of weights
bysort countrycode: gen weight1 = 1/_N
gen spelllength = round(endspell-startspell)
bysort countrycode spelllength: gen weight2=1/_N
lab var weight1 "Weights where all countries have similar sum of weights"
lab var weight2 "Weights where all countries-spell lengths have similar sum of weights"
*drop spelllength

**********************
*** MERGE ON GINIs ***
**********************
rename startspell datayear
merge m:1 countrycode datatype datayear using `ginis', keepusing(gini) keep(3) nogen
rename datayear startspell
rename gini gini_startspell
rename endspell datayear
merge m:1 countrycode datatype datayear using `ginis', keepusing(gini) keep(3) nogen
rename gini gini_endspell
rename datayear endspell
*gen spelllength = endspell - startspell
lab var spelllength "Length of spell"
order countrycode datatype startspell endspell spelllength gini* weight*
lab var gini_startspell "Gini at start of spell"
lab var gini_endspell "Gini at end of spell"
gen ginichange = ((gini_endspell/gini_startspell)^(1/spelllength)-1)*100
lab var ginichange "Annualized percentage change in Gini"
format gini* weight* %3.2f

************************************************************************************
*** CREATE MATRIX WITH PERCENTILES OF CHANGE IN GINI AS FUNCTION OF SPELL LENGTH ***
************************************************************************************
matrix results = J(30,21,.)

forvalues row=1/30 {
_pctile ginichange if spelllength==`row', p(5,10,25,50,75,90,95)
forvalues col=1/7 {
capture matrix results[`row',`col'] = r(r`col')
}
_pctile ginichange if spelllength==`row' [aw=weight1], p(5,10,25,50,75,90,95)
forvalues col=1/7 {
capture matrix results[`row',7+`col'] = r(r`col')
}
_pctile ginichange if spelllength==`row' [aw=weight2], p(5,10,25,50,75,90,95)
forvalues col=1/7 {
capture matrix results[`row',14+`col'] = r(r`col')
}
}


****************************
*** CONVERT INTO DATASET ***
****************************
clear
set more off
svmat2 results
gen spelllength = _n
order spelllength


rename results1 weight0_p5
rename results2 weight0_p10
rename results3 weight0_p25
rename results4 weight0_p50
rename results5 weight0_p75
rename results6 weight0_p90
rename results7 weight0_p95
rename results8 weight1_p5
rename results9 weight1_p10
rename results10 weight1_p25
rename results11 weight1_p50
rename results12 weight1_p75
rename results13 weight1_p90
rename results14 weight1_p95
rename results15 weight2_p5
rename results16 weight2_p10
rename results17 weight2_p25
rename results18 weight2_p50
rename results19 weight2_p75
rename results20 weight2_p90
rename results21 weight2_p95

******************
*** DRAW GRAPH ***
******************
drop if spell>15

gen minus2 = -2
gen plus2  =  2
twoway line weight2_p5 spell if spell>2, lpattern(shortdash) lcolor("$color3") lwidth(medthick) || ///
	   line weight2_p10 spell, lpattern(dash) lcolor("$color2") lwidth(medthick)  || ///
	   line weight2_p25 spell, lpattern(longdash) lcolor("$color1") lwidth(medthick) || ///
       line weight2_p50 spell, lpattern(solid) lcolor("$color0") lwidth(medthick)  || ///
       line weight2_p75 spell, lpattern(longdash) lcolor("$color1") lwidth(medthick) || ///
	   line weight2_p90 spell, lpattern(dash) lcolor("$color2") lwidth(medthick) || ///
       line weight2_p95 spell if spell>1, lpattern(shortdash) lcolor("$color3") lwidth(medthick) || ///
	   line plus2 spell, lcolor(maroon) || ///
	   line minus2 spell, lcolor(maroon) ///
	   xsize(10) ysize(10) graphregion(color(white)) ///
	   ylab(-4(2)4, angle(horizontal)) xlab(0 5 10 15) ///
	   ytitle("Annualized percentage change in Gini") xtitle("Length of spell") ///
	   legend(order(1 "5th and 95th percentile" 2 "10th and 90th percentile" 3 "25th and 75th percentile" 4 "Median") ///
	   span symxsize(*0.5) region(lcolor(white)))
graph export "Results/Gini_Histogram.png", as(png) replace
graph export "Results/Fig4.eps", replace
/*
twoway lowess weight2_p5 spell if spell>1, lpattern(shortdash) lcolor("$color4") lwidth(medthick) || ///
	   lowess weight2_p10 spell, lpattern(dash) lcolor("$color3") lwidth(medthick) || ///
	   lowess weight2_p25 spell, lpattern(longdash) lcolor("$color2") lwidth(medthick) || ///
       lowess weight2_p50 spell, lpattern(solid) lcolor("$color1") lwidth(medthick) || ///
       lowess weight2_p75 spell, lpattern(longdash) lcolor("$color2") lwidth(medthick) || ///
	   lowess weight2_p90 spell, lpattern(dash) lcolor("$color3") lwidth(medthick) || ///
       lowess weight2_p95 spell if spell>1, lpattern(shortdash) lcolor("$color4") lwidth(medthick) || ///
	   line plus2 spell, lcolor(maroon) || ///
	   line minus2 spell, lcolor(maroon) ///
	   xsize(10) ysize(10) graphregion(color(white)) ///
	   ylab(-4(2)4, angle(horizontal)) xlab(0 5 10 15) ///
	   ytitle("Annualized percentage change in Gini") xtitle("Length of spell") ///
	   legend(order(1 "5th and 95th percentile" 2 "10th and 90th percentile" 3 "25th and 75th percentile" 4 "Median") ///
	   span symxsize(*0.5) region(lcolor(white)))

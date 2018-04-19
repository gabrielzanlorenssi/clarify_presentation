
use "C:\Users\DCPLAB\Downloads\Coxappend (1).dta", clear

net from http://gking.harvard.edu/clarify

net install clarify

gen multi_heterogeneity = multimember*heterogeneity

estsimp regress enps heterogeneity multimember multi_heterogeneity

setx heterogeneity 0 multimember 0 multi_heterogeneity 0

simqi, pv

****Outros cenários 1.1 
use "C:\Users\DCPLAB\Downloads\Coxappend (1).dta", clear

estsimp regress enps heterogeneity multimember multi_heterogeneity

setx heterogeneity 1 multimember 0 multi_heterogeneity 0

simqi, pv

****Outros cenários 1.2 
use "C:\Users\DCPLAB\Downloads\Coxappend (1).dta", clear

estsimp regress enps heterogeneity multimember multi_heterogeneity

setx heterogeneity 1 multimember 1 multi_heterogeneity 0

simqi, pv

****Outros cenários 1.3
use "C:\Users\DCPLAB\Downloads\Coxappend (1).dta", clear

estsimp regress enps heterogeneity multimember multi_heterogeneity

setx heterogeneity 1 multimember 1 multi_heterogeneity 1

simqi, pv

**********
use "C:\Users\DCPLAB\Downloads\Coxappend (1).dta", clear

estsimp regress lnenps lnml lneneth lmleneth2

setx lnml 0 lneneth 0.09984533 lmleneth2 0.09984533

simqi, pv genpv (_p)

di exp( .8458656)
di exp(.2017387)
di exp(1.532314)

****
use "C:\Users\DCPLAB\Downloads\Coxappend (1).dta", clear

estsimp regress lnenps lnml lneneth lmleneth2

* Predicted value if SMD = 1 and 

  setx lnml ln(1) lneneth ln(1.105) lmleneth2 ln(1.105)
  
 
  simqi, pv tfunc(exp)    
  ****
use "C:\Users\DCPLAB\Downloads\Coxappend (1).dta", clear

estsimp regress lnenps lnml lneneth lmleneth2

* Predicted value if SMD = 1 and 
  setx lnml ln(1) lneneth ln(1.105) lmleneth2 ln(1.105)
  simqi, pv tfunc(exp)               

* First-difference of increasing democratic control from half to 2/3
  summarize pop, meanonly      /* calculate mean of population    */
  scalar popmean = r(mean)             
  setx lpop ln(popmean) ldem ln(.5)     
  simqi, fd(ev) changex(ldem ln(.5) ln(2/3)) tfunc(exp)


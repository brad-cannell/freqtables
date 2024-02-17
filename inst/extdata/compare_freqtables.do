/*******************************************************************************
Comparing Stata and R freqtables
*******************************************************************************/

* Load data
use "/Users/bradcannell/Dropbox/R/Packages/freqtables/inst/extdata/freq_study.dta"

* One one-way table
tabulate outcome
proportion outcome

* One two-way table
tabulate exposure outcome, chi2 exact gamma lrchi2 taub V
proportion outcome, over(exposure)

* What do three way tables look like?
by sex, sort : tabulate exposure outcome, chi2 exact gamma lrchi2 taub V
proportion outcome, over(exposure sex)
proportion outcome, over(sex exposure)

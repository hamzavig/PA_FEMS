devtools::install_github("hamzavig/PA_FEMS")
library(PAFEMS)
library(FEMS)
library(FEMSdevPkg)

bank <- createInstitution("Bank")
bank

PAFEMS::samplePortfolio()
samplePortfolio()

library(roxygen2); # Read in the roxygen2 R package
roxygenise()

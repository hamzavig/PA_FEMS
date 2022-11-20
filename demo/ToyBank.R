devtools::install_github("hamzavig/PA_FEMS")
library(PAFEMS)
library(utils)

bank <- createInstitution("Bank")
bank

annPortfolio <- "src/data/bankA/ann_ptf.csv"
pamPortfolio <- "src/data/bankA/pam_ptf.csv"
riskFactorsFile <- "src/data/bankA/RiskFactors.csv"

riskFactors <- defineReferenceIndex(riskFactorsFile)

ann_ptf <- samplePortfolio(annPortfolio)
ann_ptf$contracts

pam_ptf <- samplePortfolio(pamPortfolio)
ptfc <- pam_ptf$contracts

serverURL <- "https://demo.actusfrf.org:8080/"

getCIDfromContract <- function(object) {
  return ( object$contractTerms$contractID)
}

c1 <- pam_ptf$contracts[
  sapply(pam_ptf$contracts, function(cntr){
    getCIDfromContract(cntr) == "LSD0002"
  }) ]

c1

evs <- generateEventSeries(c1,riskFactors,serverURL)
evs$events_df

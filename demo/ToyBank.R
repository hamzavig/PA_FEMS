devtools::install_github("hamzavig/PA_FEMS")
library(PAFEMS)

bank <- createInstitution("Bank")
bank

annPortfolio <- "src/data/bankA/ann_ptf.csv"
pamPortfolio <- "src/data/bankA/pam_ptf.csv"
riskFactorsFile <- "src/data/bankA/RiskFactors.csv"

riskFactors <- defineReferenceIndex(riskFactorsFile)

ann_ptf <- samplePortfolio(annPortfolio)
pam_ptf <- samplePortfolio(pamPortfolio)

serverURL <- "https://demo.actusfrf.org:8080/"

c1 <- getContract(pam_ptf, "LSD0002")
c1

evs <- generateEventSeries(c1,riskFactors,serverURL)
evs$events_df

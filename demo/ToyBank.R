devtools::install_github("hamzavig/PA_FEMS")
library(PAFEMS)

bank <- createInstitution("Bank")
bank

annPortfolio <- "src/data/bankA/ann_ptf.csv"
pamPortfolio <- "src/data/bankA/pam_ptf.csv"

ann_ptf <- samplePortfolio(annPortfolio)
ann_ptf$contracts

pam_ptf <- samplePortfolio(pamPortfolio)
pam_ptf$contracts

serverURL <- "https://demo.actusfrf.org:8080/"
rfx <- sampleReferenceIndex_YC_EA_AAA()

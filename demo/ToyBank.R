devtools::install_github("hamzavig/PA_FEMS")
library(PAFEMS)
library(FEMS)
library(FEMSdevPkg)

bank <- createInstitution("Bank")
bank

library(utils)
ann_ptf <- "src/data/bankA/ann_ptf.csv"
ptf <- samplePortfolio(ann_ptf)

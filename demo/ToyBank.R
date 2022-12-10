devtools::install_github("hamzavig/PA_FEMS")
library(PAFEMS)

bank <- createInstitution("Bank")

annPortfolio <- "src/data/bankA/ann_ptf.csv"
pamPortfolio <- "src/data/bankA/pam_ptf.csv"
opsPortfolio <- "src/data/bankA/operations.csv"
riskFactorsFile <- "src/data/bankA/RiskFactors.csv"

riskFactors <- defineReferenceIndex(riskFactorsFile)

ann_ptf <- samplePortfolio(annPortfolio, "contracts")
pam_ptf <- samplePortfolio(pamPortfolio, "contracts")
ptf <- mergePortfolios(ann_ptf, pam_ptf)

ops_ptf <- samplePortfolio(opsPortfolio, "operations")

bank <- assignContracts2Tree(bank, ptf)
bank <- assignContracts2Tree(bank, ops_ptf)
bank <- events(object=bank, riskFactors = riskFactors)


bank <- assignEvents2Tree(bank, riskFactors)

by <- timeSequence("2022-01-01", by="1 years", length.out=6)
tb <- timeBuckets(by, bucketLabs=2022:2026, 
                   breakLabs=substr(as.character(by),3,10))

scale = 1000000
val.nom <- value(bank, tb, scale=scale, digits=2)
inc.nom <- income(bank, tb, "marginal", scale=scale, digits=2)

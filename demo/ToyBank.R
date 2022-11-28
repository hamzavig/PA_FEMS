devtools::install_github("hamzavig/PA_FEMS")
library(PAFEMS)

bank <- createInstitution("Bank")

annPortfolio <- "src/data/bankA/ann_ptf.csv"
pamPortfolio <- "src/data/bankA/pam_ptf.csv"
riskFactorsFile <- "src/data/bankA/RiskFactors.csv"

riskFactors <- defineReferenceIndex(riskFactorsFile)

ann_ptf <- samplePortfolio(annPortfolio)
pam_ptf <- samplePortfolio(pamPortfolio)
ptf <- mergePortfolios(ann_ptf, pam_ptf)

serverURL <- "https://demo.actusfrf.org:8080/"

bank <- assignContracts2Tree(bank, ptf)
bank$leaves[[1]]$contracts

test <- bank$leaves[[1]]$contracts

leaf_dfs <- getLeafsAsDataFrames(bank)

length(leaf_dfs)

c1 <- getContract(ptf, "LSD0002")
c1
evs <- generateEventSeries(c1,serverURL, riskFactors)
evs
cashflowPlot(evs)
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

assignContracts2Tree(bank, ptf)

c1 <- getContract(ptf, "LSD0002")

evs <- generateEventSeries(c1,riskFactors,serverURL)

getContractIDs(ptf)




a2T <- function(institution, ptf) {
  
  tree_keys <-   c("ASL", "ALL",
                   "ALM", "AFA",
                   "LSD", "LLL",
                   "LEQ", "ORI",
                   "ORC", "ORR",
                   "ORO", "OEI",
                   "OES", "OER",
                   "OEO"
  )
  
  tree_values <- c(institution$Assets$ShortTerm$LiquidAssets, institution$Assets$LongTerm$Loans,
                   institution$Assets$LongTerm$Mortgages, institution$Assets$FixedAssets,
                   institution$Liabilities$ShortTerm$Deposits, institution$Liabilities$LongTerm$Loans,
                   institution$Liabilities$Equity, institution$Operations$Revenues$Interests,
                   institution$Operations$Revenues$Commissions, institution$Operations$Revenues$Rent,
                   institution$Operations$Revenues$Other, institution$Operations$Expenses$Interests,
                   institution$Operations$Expenses$Salaries, institution$Operations$Expenses$Rent,
                   institution$Operations$Expenses$Other
  )
  
  contracts_df <- getPortfolioAsDataFrame(ptf)
  
  for(i in 1:nrow(contracts_df)){
    
    key <- substr(contracts_df[i,"contractID"],1,3)
    idx <- which(key %in% tree_keys)
    
    value <- tree_values[idx]
  }
  
  return(value)
}
value <- a2T(bank, ptf)
value[[1]]$isLeaf

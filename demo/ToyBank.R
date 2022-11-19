devtools::install_github("hamzavig/PA_FEMS")
library(PAFEMS)
library(utils)

bank <- createInstitution("Bank")
bank

annPortfolio <- "src/data/bankA/ann_ptf.csv"
pamPortfolio <- "src/data/bankA/pam_ptf.csv"
riskFactorsFile <- "src/data/bankA/RiskFactors.csv"

riskFile2dataframe <- function(fname, sep = ","){
  # this read.csv works for csv with no dayCountConvention column. Warning
  df = utils::read.csv(fname)
  # convert all missing data into text null
  df[is.na(df)] <- "NULL"
  return(df)
}

riskFactors_df2list <- function(riskFactors_df){
  rfxlist <- list()
  nhdrs <- 4        # rfType, moc, base, dataPairCount are " row headers"
  for ( irow in 1:nrow(riskFactors_df)){
    rfRow <- riskFactors_df[irow,]
    tset <- as.character(rfRow[nhdrs-1+(1:rfRow$dataPairCount)*2])
    # vector of dates
    vset <- as.numeric(rfRow[nhdrs+(1:rfRow$dataPairCount)*2])
    # vector of numeric values
    rfID <- paste0("sample$",rfRow$marketObjectCode)
    rfxlist <-append(rfxlist,
                     Index(rfID,rfRow$marketObjectCode,rfRow$base,,
                           tset,vset))
  }
  return(rfxlist)
}


riskFactors <- riskFactors_df2list(riskFile2dataframe(riskFactorsFile))

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

generateEventSeries(c1,riskFactors,serverURL)

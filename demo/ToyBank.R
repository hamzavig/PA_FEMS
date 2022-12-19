devtools::install_github("hamzavig/PA_FEMS")
library(PAFEMS)

bank <- createInstitution("Bank")

annPortfolio <- "src/data/bankA/ann_ptf.csv"
pamPortfolio <- "src/data/bankA/pam_ptf.csv"
opsPortfolio <- "src/data/bankA/operations.csv"

rfDefault <- RFConn()

ann_ptf <- samplePortfolio(annPortfolio, "contracts")
pam_ptf <- samplePortfolio(pamPortfolio, "contracts")
ptf <- mergePortfolios(ann_ptf, pam_ptf)

ops_ptf <- samplePortfolio(opsPortfolio, "operations")

bank <- assignContracts2Tree(bank, ptf)
bank <- assignContracts2Tree(bank, ops_ptf)
bank <- events(object=bank, riskFactors = rfDefault)

# Interest Rate Risk: Parallel Yield Curve Shift

riskFactors <- "src/data/bankA/rf_contracts.csv"
rfList <- getRFList(riskFactors)
rf <- RFConn() 
add(rf, rfList)

yc <- rf$riskfactors[[1]]
spread <- 0.01
cycle <- "P1YL1"
ycShifted <- shiftYieldCurve(yc, spread)
ycShifted

bankShifted <- Clone(bank, pruneFun = NULL, attributes = FALSE)
bankShifted$name <- "BankShifted"

bankShifted <- addMarketObject2Contracts(bankShifted, ycShifted, spread, cycle)

rfShifted <- RFConn()
add(rfShifted, list(ycShifted))

bankShifted <- events(object = bankShifted, riskFactors = rfShifted)


by <- timeSequence("2022-01-01", by="1 years", length.out=6)
tb <- timeBuckets(by, bucketLabs=2022:2026, 
                  breakLabs=substr(as.character(by),3,10))
scale = 1000000

val <- value(bank, tb, scale=scale, digits=2)
inc <- income(bank, tb, type="marginal", scale=scale, digits=2)
liq <- liquidity(bank, tb, scale=scale, digits=2)

valShifted <- value(bankShifted, tb, scale=scale, digits=2)
incShifted <- income(bankShifted, tb, type="marginal", scale=scale, digits=2)
liqShifted <- liquidity(bankShifted, tb, scale=scale, digits=2)


equityRatio <- valueEquityRatio(val)
liquidityCoverageRatio <- valueLiquidityCoverageRatio(val)

equityRatioShifted <- valueEquityRatio(valShifted)
liquidityCoverageRatioShifted <- valueLiquidityCoverageRatio(valShifted)

sensitivity(bankShifted, ycShifted)
sen <- showSensitivity(bankShifted)


# Default Rate Risk

riskFactors <- "src/data/bankA/rf_defaultCurves.csv"
rfDCList <- getRFList(riskFactors)

rfDCList


bankDefault <- createInstitution("BankDefault")

annPortfolio <- "src/data/bankA/ann_ptf.csv"
pamPortfolio <- "src/data/bankA/pam_ptf.csv"
opsPortfolio <- "src/data/bankA/operations.csv"

rfDefault <- RFConn()

ann_ptf <- samplePortfolio(annPortfolio, "contracts")
pam_ptf <- samplePortfolio(pamPortfolio, "contracts")
ptf <- mergePortfolios(ann_ptf, pam_ptf)

ops_ptf <- samplePortfolio(opsPortfolio, "operations")

bankDefault <- assignContracts2Tree(bankDefault, ptf)
bankDefault <- assignContracts2Tree(bankDefault, ops_ptf)

ann_df <- contractFile2dataframe(annPortfolio)
pam_df <- contractFile2dataframe(pamPortfolio)

rawCtrs <- list(ann_df, pam_df)
bankDefault
default(bankDefault, rfDCList, "2024-01-01", rawCtrs)


bank <- events(object=bank, riskFactors = rfDefault)






leafs <- bankDefault$Assets$leaves
leafs

defCtrs <- list()

for(leaf in leafs){
  ctrs <- determineDefault(leaf, rfDCList, "2024-01-01", rawCtrs)
  
  for(j in 1:length(ctrs)){
    defCtrs <- append(defCtrs, ctrs[[j]])
  }
}

object$Assets$AddChild("Default")
object$Assets$Default$contracts <- defCtrs





determineDefault <- function(node, defaults, from, rawCtrs){
  
  ctrList <- list()
  
  if(!is.null(node$contracts) || lenght(node$contracts) > 0){
    
    ctrs <- leafs[[1]]$contracts
    defaultLabels <- c()
    for(i in 1:length(rfDCList)) defaultLabels <- c(defaultLabels, rfDCList[[i]]$label)
    defaultLabels
    
    ctrs
    
    for(ctr in ctrs){
      if(ctrs[[1]]$contractTerms$legalEntityIDCounterparty %in% defaultLabels &&
         ctrs[[1]]$contractTerms$maturityDate > from){
        
        cid <- ctr$contractTerms$contractID
        
        for(i in 1:length(rawCtrs)){
          ctrdf <- rawCtrs[[i]]
          
          if(cid %in% ctrdf[,"contractID"]){
            rawCtr <- ctrdf[ctrdf$contractID == cid,]
          }
        }
        
        defCtrs <- generateDefaultContracts(ctr, defaults, from, rawCtr)
        
        for(j in 1:length(defCtrs)){
          
          ctrList <- append(ctrList, defCtrs[[j]])
        }
      }
    }
  }
  return(ctrList)
}














generateDefaultContracts <- function(object, defaults, from, ctr){
            
            ctrInitialExchangeDate <- object$contractTerms$initialExchangeDate
            ctrMaturityDate <- object$contractTerms$maturityDate
            ctrCounterParty <- object$contractTerms$legalEntityIDCounterparty
            
            dcLabels <- c()
            for(i in 1:length(defaults)) dcLabels <- c(dcLabels, defaults[[i]]$label)
            dcIdx <- which(dcLabels==ctrCounterParty)
            
            defaultCurve <- defaults[[dcIdx]]
            defaultDates <- timeSequence(from = from, to = ctrMaturityDate, by = "1 years")
            defaultRates <- getRatesAsSeries(defaultCurve, defaultDates)
            
            paymentsPassed <- yearFraction(ctrInitialExchangeDate, from, "30E360")
            paymentsPending <- yearFraction(from, ctrMaturityDate, "30E360")
            paymentsTotal <- paymentsPassed + paymentsPending
            
            recoveryRate <- paymentsPassed/paymentsTotal
            defaultGivenRisk <- (1-recoveryRate)
            
            premiumDiscount <- defaultGivenRisk*defaultRates
            
            defCtrs <- list()
            
            for(i in 1:(length(defaultDates)-1)){
              
              def <- ctr
              
              def[1,"initialExchangeDate"] <- defaultDates[i]
              def[1,"contractRole"] <- "RPL"
              def[1,"statusDate"] <- defaultDates[i]
              def[1,"contractDealDate"] <- defaultDates[i]
              def[1,"premiumDiscountAtIED"] <- -(ctr[1,"notionalPrincipal"] * premiumDiscount[i])
              def[1,"notionalPrincipal"] <- ctr[1,"notionalPrincipal"] * defaultRates[i]
              
              defCtr <- contracts_df2list(def)
              defCtrs <- append(defCtrs, defCtr[[1]])
              
            }
            
            return(defCtrs)
}

















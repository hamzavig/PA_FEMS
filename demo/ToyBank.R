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

# Yield Curve Shift

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


sen <- sensitivity(bankShifted, ycShifted)


bankShifted$attributes

nodes <- Traverse(
  bankShifted,
  traversal = c("pre-order")
)

df <- data.frame()

for(i in 1:length(nodes)){
  
  
  df <- rbind(df, nodes[[i]]$sensitivity)
  
}
df
nodes.path <- Get(Traverse(bankShifted),"pathString")
nodes.path
rn <- capture.output(bankShifted)[-1]
rn
rn <- substring(rn,4,max(nchar(rn)))
rn

res <- bankShifted$sensitivity
res

rnams <- character(nrow(res))
rnams
for (i in 1:nrow(res)) {
  rnams[i] <- paste(format(i,width=2),rn[nodes.path==res[i,1]])
}
# t(t(rnams))
rownames(res) <- rnams
res[,-1]


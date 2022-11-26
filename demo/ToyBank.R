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
evs <- generateEventSeries(c1,riskFactors,serverURL)
evs
cashflowPlot(evs)

devtools::install_github("wbreymann/FEMS")


## Initialization -----------
t0 <- "2021-01-01"
# Start date of the contracts. Should already exist at analysis date
tstart <- "2020-12-31" # 
# scale for the amount of capital (in Mio CHF)
scale <- 1000000

# Fixed assets -----------
# must be modeled with Investment contract because of depreciation
asset.fixed.value <- 200*scale
# Define time pattern of depreciation
# yearly time steps 50 years into the future
times.re <- timeSequence(from = timeDate(tstart), by = "1 years", length.out = 51)
asset.fixed.func <- function(times) {
  timeSeries(seq(asset.fixed.value, 0, length.out=51), times)
}
asset.fixed.func(times.re)

# Define Investments contract
asset.fixed <- Investments(pattern=asset.fixed.func, 
                           args=list(times=times.re
                           ), Currency="CHF", ContractID="RE01")
asset.fixed

spot.rates <- c(0.01, 0.02, 0.03, 0.04)
yc <- YieldCurve(label = "YC", 
                 ReferenceDate = t0, 
                 Tenors = c("1M", "2Y", "5Y", "10Y"),
                 Rates = spot.rates)
yc
plot(yc)
rf <- RFConn(yc)

assets.liquid.value <- 100*scale
assets.liquid <- bond(ContractID="Liquid", start = tstart, maturity = "10 years", 
                      nominal = assets.liquid.value, coupon = yc$Rates[1], role="long", 
                      CycleOfRateReset="P1YL0", CycleAnchorDateOfRateReset=tstart, MarketObjectCodeOfRateReset="YC")

asset.fixed.rent <- 0.05 * asset.fixed.value
rent.yearly <- rep(asset.fixed.rent, 50)
#
rent.fun <- function(rent, times) {
  timeSeries(data=rent, charvec=times)
}
rent.fun(rent.yearly, times.re[-51])
Rent <- OperationalCF(ContractID="RE-Rent", Currency="CHF",  # Define contract
                      pattern=rent.fun, 
                      args=list(rent=rent.yearly, times=times.re[-51]) )

cfs <- cashFlows(Rent, from=as.character(t0), to=as.character(times.re[51]))
cfs
evsInv <- EventSeries(asset.fixed, t0)
evsOPCF <- EventSeries(Rent, t0)
evsInv.details <- evsInv$evs

assets.liquid$ContractTerms

detach("package:FEMS", unload = TRUE)
remove.packages("FEMS")





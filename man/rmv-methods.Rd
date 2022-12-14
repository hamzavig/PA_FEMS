% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RiskFactorConnector.R
\docType{methods}
\name{remove}
\alias{remove}
\alias{remove,RiskFactorConnector,character-method}
\alias{remove-method}
\title{Generic \code{remove} method}
\usage{
remove(object, what, ...)

\S4method{remove}{RiskFactorConnector,character}(object, what, ...)
}
\arguments{
\item{object}{The \code{\link{RiskFactorConnector}} object}

\item{what}{A character (vector) containing key(s) to
elements in \code{object} and which are to be removed}

\item{...}{}
}
\description{
Implemented for various purposes.

Each element of the \code{what} parameter is expected to
point to an element in the \code{RiskFactorConnector} by
their 'keys'. All elements that can be found in
\code{RiskFactorConnector} are removed.
}
\examples{
yc <- YieldCurve() # create a YieldCurve object
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(MarketObjectCode = "YC_Prim",
    Nodes = list(ReferenceDate = "2015-01-01", 
    Tenors = tenors, Rates = rates)))

ind <- Index() # create a ReferenceIndex object
times <- c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01",
           "2019-01-01")
values <- c(100, 110, 120, 130, 140)
set(ind, what=list(MarketObjectCode = "CHF_SMI",
    Data=list(Dates=times,Values=values)))
    
fx <- FxRate() # create an FX-Rate object
times <- c("2015-01-01", "2016-01-01", "2017-01-01", 
           "2018-01-01", "2019-01-01")
values <- c(1.04, 1.05, 1.2, 1.0, 0.9)
set(fx, what=list(MarketObjectCode = "CHF/USD",
    Data=list(Dates=times,Values=values)))   
    
rf <- RFConn() # create a RiskFactorConnector object
add(rf,list(yc,ind,fx)) # add all risk factors to the RiskFactorConnector
containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))
remove(rf, "YC_Prim")
containsID(rf, c("YC_Prim", "CHF_SMI", "YC_2"))

}
\seealso{
add
}

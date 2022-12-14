% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DynamicYieldCurve.R
\docType{methods}
\name{rates}
\alias{rates}
\alias{rates,}
\alias{DynamicYieldCurve,}
\alias{charachter,}
\alias{missing-method}
\alias{character,}
\alias{character-method}
\alias{rates,YieldCurve-method}
\alias{YieldCurve,}
\alias{rates,DynamicYieldCurve-method}
\title{Generic method to retrieve the rate(s) for (a) specific
tenor(s) from a \code{\link{DynamicYieldCurve}} object}
\usage{
rates(object, from, to, by, ad, refdate, ...)

\S4method{rates}{YieldCurve}(object, from, to, by, ad, refdate, ...)

\S4method{rates}{DynamicYieldCurve}(object, from, to, by, ad, refdate, ...)
}
\arguments{
\item{object}{An object of class \code{DynamicYieldCurve} for 
which to return the yield for a given tenor}

\item{from}{(optional) Date(s) t1. Should be a \code{character} variable with 
format 'YYYY-MM-DD'. Is set to \code{ad} if missing.}

\item{to}{(optional, character) The date(s) t2. Should have the same format as \code{from}.}

\item{by}{(optional) The tenor(s). Should be for form 'nX' where 'n' is an integer
and 'X'=("D"|"W"|"M"|"Q"|"H"|"Y") for days, weeks, months, quarters, half-years 
and years, respectively. Either \code{to} or \code{by} must be defined.}

\item{ad}{(optional) The analysis date \code{ad} with respect to which the interest
rates are calculated. Should have the same format as \code{from}. 
If missing or \code{ad}=\code{to}, spot rates are calculated. 
If \code{ad}<\code{to}, forward rates are calculated.}

\item{Additional}{parameters:

\itemize{
 \item{"isDateEnd"}{(deprecated) logical indicating whether to is 
       of date (TRUE) or term (FALSE) format. Date format is
       'YYYY-MM-DDTXX' with 'XX'=00 for beginning of day, or
       24 for end of day, and term format is 'IP' with 'I' 
       an integer and 'P' indicating the period (D=days, 
       W=weeks, M=months, Q=quarters, H=halfyears, Y=years).
       Default is isDateEnd=FALSE.}
  \item{"refdate"} {(deprecated) an alternative name for \code{ad}}
}}
}
\value{
\code{numeric} The interest rates for the defined tenor(s)
}
\description{
A yield curve (or more precisely spot rate curve) describes
the term structure of interest rates. The interest rates are calculated
with respect to a given (analysis) adte \code{ad} for money that is borrowed or
lend at t1>=ad until time t2>t1. 
The term or tenor is t2-t1.
Cf. below for details.
}
\details{
The 
\code{\link{DynamicYieldCurve}} object contains tenors with
associated spot interest rates. Rates can be retrieved for any
tenor by inter-/extrapolation.

For the forward rate at \code{ad} 
       between times t1 and t2, \code{to} refers to t2 and
       \code{from} refers to t1. Is a single value. If combined 
       with a vector for \code{to}, then \code{from} remains
       the same for all t2 defined in \code{to}.
}
\examples{
yc <- DynamicYieldCurve()
tenors <- c("1W", "1M", "6M", "1Y", "2Y", "5Y")
rates <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03)
set(yc, what = list(MarketObjectCode = "YC_Prim",
  Nodes = list(ReferenceDate = "2015-01-01T00", Tenors = tenors, Rates = rates)))

rates(yc, by="1Y")  # 1-year spot rate
rates(yc, to="2016-01-01T00") # again, 1-year spot rate
rates(yc, by="1Y", from="2015-07-01T00")  # 1-year forward rate at 2015-07-01T00

}
\seealso{
\code{\link{discountFactors}}
}

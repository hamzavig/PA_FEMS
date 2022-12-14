% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RiskFactorConnector.R
\docType{methods}
\name{RFConn}
\alias{RFConn}
\alias{RFConn-method}
\alias{RFConn,ANY-method}
\title{\code{RiskFactorConnector}-class constructor}
\usage{
RFConn(...)

\S4method{RFConn}{ANY}(...)
}
\arguments{
\item{...}{}
}
\value{
An object of class \code{RiskFactorConnector} 
         containing the reference to the Java object
}
\description{
Create an instance of \code{RiskFactorConnector} class. The 
constructor will also create an instance of the respective
Java class in the running JVM. The created instance will be 
an empty collection of \code{RiskFactor} objects.
}

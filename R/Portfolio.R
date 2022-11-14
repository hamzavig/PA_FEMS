# Portfolio.R  FEMS dev code by Vigan Hamzai Nov 2022
# Edits/subset of Portfolio.R in main FEMS branch
# Licensing and Copyright notices from there
# *********************************************************************
# class Portfolio
# *************************************
#' class Portfolio
#'
#' A Portfolio consists of a list of contracts such as
#' @include ContractType.R
#' @import methods
#' @importFrom methods new
#' @export Portfolio
#' @exportClass Portfolio
#' 
#' @field contracts  List of contracts, class=ContractType, in the portfolio.
#' @field riskFactors List of class=ReferenceIndex's defining a risk Scenario.
#'        possibly better to change to riskFactorIndexList 
setRefClass("Portfolio",
            fields = list(
              contracts = "list"   # contracts are instances of ContractType
            ))

# **************************************
# constructors Portfolio(...) for a portfolio object
# *************************************
#' Portfolio < >  -  generic function definition 
#'
#' Defines generic S4 constructor method on class Portfolio
#' @param  contract   S4 reference Class=ContractType, a contract to include. 
#' @param  ...        Not used
setGeneric(name = "Portfolio",
           def = function(contract, ...){
             standardGeneric("Portfolio")
           })

#' Portfolio ( )  - no parameters instance of Portfolio< > 
#' 
#' Creates an empty Portfolio object with no attributes initialized. 
#' @return  S4 reference with class=Portfolio and no attributes initialized.
setMethod(f = "Portfolio", signature = c(),
          definition = function( ){
            return(new("Portfolio"))
          })

#' Portfolio("ContractType")  Constructs Portfolio containing a single contract.
#' 
#' This instance of the generic Portfolio< > method takes a reference to a 
#' contract as its input parameter and returns a portfolio with no defined risk 
#' Scenario and this single contract as its contents
#' @param contract  S4 reference class=ContractType
#' @return   S4 reference class=Portfolio, initialized attributes
setMethod(f = "Portfolio", signature = "ContractType",
          definition = function (contract) {
            ptf <- Portfolio()
            ptf$contracts = list(contract)
            return(ptf)
          })


# ************************************************************
# samplePortfolio(contractDataFileName)
# ************************************************************
#' samplePortFolio
#'
#' samplePortfolio(cdfn) takes as input a contracts-data-filepath 
#'   reads this data and returns an initialized
#'   Portfolio object with contracts and from this csv file.
#' @param cdfn      character string -  a contract-data-filepath
#'
#' @return   Portfolio s4 object initialized with the data from the input files
#' @export
#' @include utils.R
#' @importFrom utils read.csv
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf <- samplePortfolio(cdfn)
#'    }
#'
samplePortfolio <- function(cdfn) {
  ptf <- Portfolio()  # create portfolio object no attributes set
  ptf$contracts <- contracts_df2list(contractFile2dataframe(cdfn))
  return(ptf)
}

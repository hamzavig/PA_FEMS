# RiskFactor.R  FEMSdev version Francis Parr edits Mar 2022
#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

##############################################################
# A Reference Class that represents all Risk Factor objects
#
# This class is only used in a virtual sense in that it is
# not intended to create instances of it. It's use is to
# serve as parent to various implementations of Risk Factor
# types such as  MarketIndex, YieldCurve .
#

#' Class RiskFactor
#'
#'  This class is only used in a virtual sense in that it is
#'  not intended to create instances of it. It's use is to
#'  serve as parent to various implementations of Risk Factor
#'  types such as  MarketIndex, YieldCurve .
#'
#' @field label character.
#' @field base numeric.
#' @field data timeSeries.
#'
#' @return  S4 object class RiskFactor
#' @import methods
#' @importFrom methods new
#' @exportClass RiskFactor
#'
setRefClass("RiskFactor",
            fields = list(riskFactorID =  "character",
                          rf_marketObjectCode = "character", # will move to RefIndx
 # FNP replaced          label = "character",
                          rf_base  = "numeric",  # will move to ReferenceIndex
                          rf_data =  "timeSeries" # will move to ReferenceIndex
            ))

setGeneric(name = "RF",
           def = function(object) standardGeneric("RF"))

# FNP unclear what the intended purpose of character parameter is 
setMethod(f = "RF", signature = c("character"),
          definition = function(object) {
            return(new(object))
          })

setGeneric(name = "valueAt",
           def = function(object, at, ...){
             standardGeneric("valueAt")
           })

# setGeneric(name = "as.timeSeries",
#           def = function(x){
#             standardGeneric("as.timeSeries")
#           })

# FNP replace with as.timeSeries.ReferenceIndex(rfx)
as.timeSeries.RiskFactor <- function(x) {return(x$data) }


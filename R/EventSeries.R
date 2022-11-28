# *************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
# *************************************************************

# *************************************************************
#' S4 class EventSeries holds a dataframe with the cashflow events generated for
#' a single Actus Contract, along with the contractID, contractType, statusDate
#' contructors: EventSeries()  EventSeries(<Contract>,<rfs>) 2B exported
#' @import  methods
#' @import  jsonlite
#' @import  httr
#' @include RiskFactor.R
#' @include Portfolio.R
#' @include ContractType.R
#' @include FEMSContract.R
#' @include Investments.R
#' @include OperationalCF.R
#' @include AnalysisDate.R
#' @include Events.R
#' @include utils.R
#' @export EventSeries
#' @exportClass EventSeries
#' @rdname EventSeries
#' 
setRefClass("EventSeries",
            fields = list(
              events_df = "data.frame",
              contractID = "character",
              contractType = "character",  # short form e.g. 'PAM'
              statusDate = "character",    # text yyyy-mm-dd
              riskFactors = "list")
            )

setGeneric(name = "EventSeries",
           def = function(object, processor, riskFactors){
             standardGeneric("EventSeries")
           })

setMethod(f = "EventSeries", signature = c(),
          definition = function(){
            return(new("EventSeries"))
          })

setMethod(f = "EventSeries", signature = c("ContractType", "character", "list"),
          definition = function(object, processor, riskFactors){
            # cast contract as list of list
            contracts <- list(contract)
            
            # prepare list in necessary structure to pass to JSON generator
            contractDefs <- lapply(contracts,preJcontract)
            riskFactors <-  preJSONrfxs(riskFactors)
            fin_list <- list(contracts = contractDefs,
                             riskFactors = riskFactors)
            
            # create final request body in json format
            request_body <- toJSON(fin_list, pretty = TRUE, auto_unbox = FALSE)
            
            # configure httr to NOT check certificTES on SSL / https connection
            httr::set_config(httr::config(ssl_verifypeer = 0L,
                                          ssl_verifyhost = 0L))
            
            # issue POST command to have server generate cashflows
            response_events <- POST(paste0(serverURL, "eventsBatch"),
                                    body = request_body,
                                    content_type_json())

            response_content <- content(response_events)
            if (response_events$status_code != 200) {
              print(request_body)
              print(response_events)
              print(response_content)
              stop("ErrorIn::ContractType::API response error; Check if all necessary contractTerms were set correctly!!!")
            }
            
            # Run the cashflow generation on this portfolio
            cshfl_rslt1 <- response_content[[1]]
            
            #first cashflow from single contract ptf
            stopifnot (cshfl_rslt1$status == "Success") # possible better info
            evs_list <- cshfl_rslt1$events
            # build the output EventSeries object
            evs <- EventSeries()
            evs$contractID <- contract$contractTerms$contractID
            evs$contractType <- contract$contractTerms$contractType
            evs$statusDate <-  contract$contractTerms$statusDate
            evs$riskFactors <- riskFactors

            # construct the 7 columns with event list data (no long loops please)
            # initialize the data.frame with a row index evid
            evid <- 1:length(evs_list)
            events_df <-data.frame(evid)
            Event_Field_Names <- c("type","time","payoff","currency",
                                   "nominalValue","nominalRate","nominalAccrued")
            for(evfield in Event_Field_Names) {
              events_df[evfield] <- unlist(sapply(evs_list,
                                                  function(ev){ev[evfield]}))
            }
            # now remove the evid column used to size events_df
            events_df <- subset(events_df, select = -evid) #drop starter column

            events_df$time <- sapply(events_df$time,
                                     function(t){substr(t,1,10)}) # format dates
            evs$events_df <- events_df
            return(evs)
          })


# EventSeries methods for Operations contract

setMethod(f = "EventSeries", signature = c("FEMSContract", "character", "missing"),
          definition = function(object, processor, riskFactors){
            EventSeries(object,timeDate(substring(processor,1,10)))
          })

setMethod(f = "EventSeries", signature = c("FEMSContract", "AD0", "missing"),
          definition = function(object, processor, riskFactors){
            EventSeries(object,as.character(processor))
          })

setMethod(f = "EventSeries", signature = c("OperationalCF", "timeDate", "missing"),
          definition = function(object, processor, riskFactors){
            
            # create event series object
            out <- new("EventSeries")
            out$contractID <- object$contractTerms$contractID
            out$contractType <- object$contractTerms$ContractType
            
            # AD0 event
            events <- data.frame(Date=as.character(processor),
                                 Value=0.0,
                                 Type="AD0",
                                 Level="P",
                                 Currency=object$contractTerms$currency,
                                 Time=0.0,
                                 NominalValue=0.0,
                                 NominalRate=0.0,
                                 NominalAccrued=0.0)
            
            ops <- do.call(object$pattern, object$args)

            if(!is.null(ops)) {
              vals <- as.numeric(series(ops))
              events <- rbind(events, data.frame(Date=as.character(time(ops)),
                                                 Value=c(vals[1],vals[2:length(vals)]),
                                                 Type="OPS", 
                                                 Level="P", 
                                                 Currency=object$contractTerms$currency,
                                                 Time=yearFraction(as.character(processor), as.character(time(ops)),convention = "30E360"),
                                                 NominalValue=0.0, 
                                                 NominalRate=0.0, 
                                                 NominalAccrued=0.0)
                              )
            }

            # convert to (sorted) timeSeries
            # Note: AD0 event needs to be after all other events of the same instant
            tms <- paste0(events$Date,"T00:00:00")
            tms[events$Type=="AD0"] <- paste0(substring(tms[events$Type=="AD0"],1,10),"T23:59:59")
            events <- events[order(tms),]
            evs.ts <- timeSeries(events,timeDate(events$Date))
            
            # compute nominal value
            evs.ts$NominalValue <- cumsum(evs.ts$NominalValue)
            
            # exclude pre-ad0 events
            # Note, its a sorted series so just look for AD0-event index
            evs.ts <- tail(evs.ts,nrow(evs.ts)-(which(evs.ts$Type=="AD0")-1))
            
            # convert back to data.frame
            events <- as.data.frame(series(evs.ts))
            events$Value <- as.numeric(events$Value)
            events$Time <- as.numeric(events$Time)
            events$NominalValue <- as.numeric(events$NominalValue)
            events$NominalRate <- as.numeric(events$NominalRate)
            events$NominalAccrued <- as.numeric(events$NominalAccrued)
            rownames(events) <- NULL
            
            # attach events to series
            out$events_df  <-  events
            
            return(out)
          })


setMethod(f = "EventSeries", signature = c("Investments", "timeDate", "missing"),
          definition = function(object, processor, riskFactors){
            
            # create event series object
            out <- new("EventSeries")
            out$contractID <- object$contractTerms$contractID
            out$contractType <- object$contractTerms$ContractType
            
            # AD0 event
            events <- data.frame(Date=as.character(processor),
                                 Value=0.0,
                                 Type="AD0",
                                 Level="P",
                                 Currency=object$contractTerns$currency,
                                 Time=0.0,
                                 NominalValue=0.0,
                                 NominalRate=0.0,
                                 NominalAccrued=0.0)
            
            ops <- do.call(object$pattern, object$args)

            if(!is.null(ops)) {
              if (length(ops)<2) stop("An investment pattern needs to have length>1!")
              vals <- c(ops[1,],diff(ops)[-1,])
              events <- rbind(events, data.frame(Date=as.character(time(ops)),
                                                 Value=c(-vals[1],vals[2:length(vals)]),
                                                 Type=c("IED",rep("DPR",length(ops)-1)),
                                                 Level="P", 
                                                 Currency=object$contractTerns$currency,
                                                 Time=yearFraction(as.character(processor), as.character(time(ops)),convention = "30E360"),
                                                 NominalValue=vals,
                                                 NominalRate=0.0,
                                                 NominalAccrued=0.0
                                                 )
                              )
            }
            # If there is a salvage value (write-off no till 0)
            # we add a last event of type MD and the remaining value
            if ( tail(ops,1) > 0 ) {
              tmp <- tail(ops,1)
              vals <- as.numeric(series(tmp))
              events <- rbind(events, 
                              data.frame(Date=as.character(time(tmp)),
                                         Value=vals,
                                         Type="MD",
                                         Level="P",
                                         Currency=object$Currency,
                                         Time=yearFraction(as.character(processor), as.character(time(tmp)), convention = "30E360"),
                                         NominalValue=-vals,
                                         NominalRate=0.0,
                                         NominalAccrued=0.0
                                         )
                              )
              object$contractTerms$maturityDate <- events[events$Type == "MD","Date"]
            }else{
              min_idx <- min(which(events$NominalValue==0))
              object$contractTerms$maturityDate <- events[min_idx, "Date"]
            }
            
            # convert to (sorted) timeSeries
            # Note: AD0 event needs to be after all other events of the same instant
            tms <- paste0(events$Date,"T00:00:00")
            tms[events$Type=="AD0"] <- paste0(substring(tms[events$Type=="AD0"],1,10),"T23:59:59")
            events <- events[order(tms),]
            evs.ts <- timeSeries(events,timeDate(events$Date))
            
            # compute nominal value
            evs.ts$NominalValue <- cumsum(evs.ts$NominalValue)
            
            # exclude pre-ad0 events
            # Note, its a sorted series so just look for AD0-event index
            evs.ts <- tail(evs.ts,nrow(evs.ts)-(which(evs.ts$Type=="AD0")-1))
            
            # convert back to data.frame
            events <- as.data.frame(series(evs.ts))
            events$Value <- as.numeric(events$Value)
            events$Time <- as.numeric(events$Time)
            events$NominalValue <- as.numeric(events$NominalValue)
            events$NominalRate <- as.numeric(events$NominalRate)
            events$NominalAccrued <- as.numeric(events$NominalAccrued)
            rownames(events) <- NULL
            
            # attach events to series
            out$events_df  <-  events
            
            return(out)
          })



## -----------------------------------------------------------------
## events methods for Operations contract
#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("FEMSContract", "character", "missing"),
          definition = function(object, processor, riskFactors){
            return(PAFEMS:::events(object,timeDate(substring(processor,1,10))))
          })

#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("FEMSContract", "AD0", "missing"),
          definition = function(object, processor, riskFactors){
            return(PAFEMS:::events(object, as.character(processor)))
          })

#' @export
#' @rdname ev-methods
setMethod(f = "events", signature = c("FEMSContract", "timeDate", "missing"),
          definition = function(object, processor, riskFactors){
            return(PAFEMS:::EventSeries(object,processor))
          })



#' generateEventSeries      Generic method definition
#'
#' Defines a generic method on S4 Class Eventseries. The instance is
#' generateEventSeries < contract riskFactors serverURL >
#'
#' @param  contract     the contract to simulate cashflows for
#' @param  riskFactors  list of riskFactors - scenario for contract simulation
#' @param  serverURL    locate the ACTUS server performing the cashflow analysis
#' @return              an EventSeries with cashflow events for the contract
#' 
setGeneric(name = "generateEventSeries",
           def = function(object, processor, riskFactors){
             standardGeneric("generateEventSeries")
           })

#' generateEventSeries        <contract>, <ACTUS-server-URL>, <risk-factor-list>
#'
#' exported function to simulate a contract cashflows and create an
#'   EventSeries using  "ContractType", "list", "character" method instance.
#'   constructs an EventSeries instance including as its events_df attribute a
#'   dataframe of cashflow events for the input ACTUS contract. This cashflow
#'   is generated with a callout to the ACTUS server located at ACTUS-server-URL
#'   using a risk scenario specified as the list of risk factors. THe method
#'   works by first creating a Portfolio with this ine contract and the supplied
#'   risk factor list, then calling generateEvents on this portfolio
#'
#'   This is an internal method on EventSeries, and not exported.
#'   generateEventSeries <contract>, risk-factors>, <serverURL> is the
#'   exported wrapper function which is exported from FEMSdevPkg and calls this.
#'
#' @param  contract    S4 ref      class= ContractType
#' @param  riskFactors list        list of S4 ref Class=RiskFactor
#' @param  serverURL   character   URL of ACTUS server to simulate the contract
#' @return              S4 ref     class=EventSeries
#' @export
#'
setMethod(f = "generateEventSeries", signature = c("ContractType", "character", "list"),
          definition = function(object, processor, riskFactors){
              evs <- EventSeries(object, processor, riskFactors)
              return(evs)
          })

library(utils)
library(timeSeries)

# ***************************************
# file2dataframe(filename)
#    reads named file; creates clean df
#    unify file2Contracts_df(filename)
#    and file2riskFactors_df(filename)
#       FNP 25th April 2022
#    for contracts we need to force dayCountConvention to be read
#    as character ( "30E360" would be read numeric )  BUT
#    field does not occur in riskData csv files
# ***************************************
contractFile2dataframe <- function(cdfn, sep = ",") {
  # read csv ignores and strips '"' and dayCountConvention has 30E360 value
  # which gets read as numeric = Inf.
  # we assume all contract csv files will have a dayCountConvention column
  df <- utils::read.csv(cdfn, colClasses = c(dayCountConvention = "character"))
  # convert all missing data into text null
  df[is.na(df)] <- "NULL"
  return(df)
}

# ************************************
# contracts_df2list(contracts_df)
#   build list of contracts from df
#   expanded cleaned version of df2contracts_list
#  Split df: terms, legs, descriptp (once)
#  for each row: createContract(terms, legs, irow)
#      append into returned list  fnp  10 Apr 2022
#    -- improved version df2contracts_list()
# ************************************************

contracts_df2list<- function(contracts_df){
  nonTermCols <- c("description","contrStrucObj.marketObjectCode",
                   "contrStruc.referenceType", "contrStruc.referenceRole")
  terms_df <-contracts_df[!names(contracts_df) %in% nonTermCols]
  legs_df <-data.frame(
    marketObjectCode = contracts_df["contrStrucObj.marketObjectCode"],
    referenceType = contracts_df["contrStruc.referenceType"],
    referenceRole = contracts_df["contrStruc.referenceRole"]
  )
  outlist <- list()
  for ( irow in 1:nrow(contracts_df)){
    outlist <- append (outlist, datarow2Contract(terms_df,legs_df,irow) )
  }
  return (outlist)
}

# ***********************************************
# datarow2Contract ( ) -  create contract object
#    inputs:  terms_df, legs_df, descr, irow :
#    contractType; object <- new("contractType)
#    constructors will set isStructured but not populate terms or legs
#    if isStructured: insertLegs
#    insertTerms ( both simple and structured cases )
# ************************************************
datarow2Contract<- function(terms_df, legs_df,irow){
  contractTypeName <- longName(tolower(terms_df$contractType[irow]))
  contract <- CT(contractTypeName)
  #FNP  avoid validity check for now 10Apr2022; test PAM,OPTNS
  if (contractTypeName == "Option"){
    contract$contractStructure<-list(
      CLeg(legs_df$contrStrucObj.marketObjectCode[irow])
    )
    contract$isCompound <- TRUE
  } else {
    contract$isCompound <- FALSE
  }
  # insert terms - skipping term validy checks for now FNP Apr 2022
  contractTerms <- as.list(t(terms_df[irow,]))
  names(contractTerms) <- colnames(terms_df)  # reattach column names
  
  ## drop all NULL elements. sapply operates on a list but returns a vector
  contractTerms <- contractTerms[sapply(contractTerms, function(x) x != "NULL")]
  
  set(object = contract, what = contractTerms)
  return(contract)
}

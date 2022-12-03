#*******************************************************************************
# ZHAW
# package: PA_FEMS
# Date: 08.11.2022
# Autor: Vigan Hamzai (hamzavig@students.zhaw.ch)
#*************************************************************

##############################################################
#' 
#' Class that contains the whole model of an enterprise or institution.
#' 
#' The class inherits from class \code{Node} in package \code{data.tree}
#' It contains a hierarchical model structure.
#' 
#' @import data.tree R6
#' @export
#' @rdname Institution
Institution <- R6Class("Institution",
                       inherit = Node)

# ************************************************************
# createInstitution(institutionName)
# ************************************************************
#' createInstitution
#' 
#' createInstitution(name) function of class Institution 
#' creates the whole hierarchy of the institution with name <name>.
#' 
#' @export
#' @rdname createInstitution
#' 
createInstitution <- function(name, ...) {
  
  institution <- Node$new(name)
  # Create top level nodes "Assets", "Liabilities" and "Operations"
  institution$AddChild("Assets")
  institution$AddChild("Liabilities")
  institution$AddChild("Operations")
  
  # Create underlying nodes for "Assets"
  institution$Assets$AddChild("ShortTerm")
  institution$Assets$AddChild("LongTerm")
  institution$Assets$AddChild("FixedAssets")
  
  institution$Assets$ShortTerm$AddChild("LiquidAssets")
  institution$Assets$LongTerm$AddChild("Loans")
  institution$Assets$LongTerm$AddChild("Mortgages")
  
  # Create underlying nodes for "Liabilities"
  institution$Liabilities$AddChild("ShortTerm")
  institution$Liabilities$AddChild("LongTerm")
  institution$Liabilities$AddChild("Equity")
  
  institution$Liabilities$ShortTerm$AddChild("Deposits")
  institution$Liabilities$LongTerm$AddChild("Loans")
  
  # Create underlying nodes for "Operations"
  institution$Operations$AddChild("Revenues")
  institution$Operations$AddChild("Expenses")
  
  institution$Operations$Revenues$AddChild("Interests")
  institution$Operations$Revenues$AddChild("Commissions")
  institution$Operations$Revenues$AddChild("Rent")
  institution$Operations$Revenues$AddChild("Other")
  
  institution$Operations$Expenses$AddChild("Interests")
  institution$Operations$Expenses$AddChild("Salaries")
  institution$Operations$Expenses$AddChild("Rent")
  institution$Operations$Expenses$AddChild("Other")
  
  return(institution)
}

# ************************************************************
# assignContracts2Tree(institution, ptf)
# ************************************************************
#' assignContracts2Tree
#' 
#' assignContracts2Tree(institution, ptf) assigns a given Portfolio <ptf>
#' to the respective leaf of the institution tree while converting the contracts
#' of the given Portfolio <ptf> into a data.frame first.
#' 
#' @include Portfolio.R
#' @include ContractType.R
#' @export
#' @rdname assignContracts2Tree

assignContracts2Tree <- function(institution, ptf, ...) {
  
  tree_dict <-   c("ASL" = institution$Assets$ShortTerm$LiquidAssets,
                   "ALL" = institution$Assets$LongTerm$Loans,
                   "ALM" = institution$Assets$LongTerm$Mortgages,
                   "AFA" = institution$Assets$FixedAssets,
                   "LSD" = institution$Liabilities$ShortTerm$Deposits,
                   "LLL" = institution$Liabilities$LongTerm$Loans,
                   "LEQ" = institution$Liabilities$Equity,
                   "ORI" = institution$Operations$Revenues$Interests,
                   "ORC" = institution$Operations$Revenues$Commissions,
                   "ORR" = institution$Operations$Revenues$Rent,
                   "ORO" = institution$Operations$Revenues$Other,
                   "OEI" = institution$Operations$Expenses$Interests,
                   "OES" = institution$Operations$Expenses$Salaries,
                   "OER" = institution$Operations$Expenses$Rent,
                   "OEO" = institution$Operations$Expenses$Other
                   )

  for(i in 1:length(ptf$contracts)){
    
    contractID <- getCIDfromContract(ptf$contracts[[i]])
    ct_leaf_key <- substr(contractID,1,3)
    leaf <- tree_dict[ct_leaf_key]
    
    stopifnot(leaf[[ct_leaf_key]]$isLeaf)
    leaf[[ct_leaf_key]]$contracts <- c(leaf[[ct_leaf_key]]$contracts, ptf$contracts[[i]])
  }

  return(institution)
}



# ************************************************************
# assignEvents2Tree(institution)
# ************************************************************
#' assignEvents2Tree
#' 
#' assignEvents2Tree(institution) assigns all corresponding events of the contracts
#' in the respective leaf of the institution tree
#' 
#' @include Portfolio.R
#' @include ContractType.R
#' @include EventSeries.R
#' @export
#' @rdname assignEvents2Tree

assignEvents2Tree <- function(institution, rf, ...) {
  
  for(i in 1:length(institution$Assets$leaves)){
    
    institution$Assets$leaves[[i]]$events <- list()
    
    for(j in 1:length(institution$Assets$leaves[[i]]$contracts)){
      
      if(institution$Assets$leaves[[i]]$contracts[[j]]$contractTerms$contractType %in% c("PAM","ANN")){
      
        contract <- institution$Assets$leaves[[i]]$contracts[[j]]
        serverURL <- "https://demo.actusfrf.org:8080/"
        riskFactors <- rf
        
        ctr_events <- EventSeries(contract, serverURL, riskFactors)
        leaf_events <- institution$Assets$leaves[[i]]$events
        leaf_events <- append(leaf_events, ctr_events)
                
      }else{
        
        contract <- institution$Assets$leaves[[i]]$contracts[[j]]
        ctr_start <- contract$contractTerms$initialExchangeDate
        riskFactors <- rf
        
        ctr_events <- EventSeries(contract, ctr_start)
        leaf_events <- institution$Assets$leaves[[i]]$events
        leaf_events <- append(leaf_events, ctr_events)
        
      }
    }
  }
  
  return(institution)
  
}


# ************************************************************
# getLeafsAsDataFrames(institution)
# ************************************************************
#' getLeafsAsDataFrames
#' 
#' getLeafsAsDataFrames(institution) converts each leaf and it's contracts
#' to a data.frame and returns a list of data.frames.
#' 
#' @include Portfolio.R
#' @include ContractType.R
#' @export
#' @rdname getLeafsAsDataFrames

getLeafsAsDataFrames <- function(institution, ...) {
  
  leaf_paths <- c("Assets$ShortTerm$LiquidAssets",
                  "Assets$LongTerm$Loans",
                  "Assets$LongTerm$Mortgages",
                  "Assets$FixedAssets",
                  "Liabilities$ShortTerm$Deposits",
                  "Liabilities$LongTerm$Loans",
                  "Liabilities$Equity",
                  "Operations$Revenues$Interests",
                  "Operations$Revenues$Commissions",
                  "Operations$Revenues$Rent",
                  "Operations$Revenues$Other",
                  "Operations$Expenses$Interests",
                  "Operations$Expenses$Salaries",
                  "Operations$Expenses$Rent",
                  "Operations$Expenses$Other"
  )
  
  leaf_dfs <- list()
  
  for(i in 1:length(institution$leaves)){
    
    if(!is.null(institution$leaves[[i]]$contracts)){
      leaf_ptf <- Portfolio()
      leaf_ptf$contracts <- institution$leaves[[i]]$contracts
      leaf_df <- getPortfolioAsDataFrame(leaf_ptf)
    }else{
      leaf_df <- data.frame()
    }
    
    leaf_dfs[[i]] <- list(leaf = leaf_paths[i],
                          contracts = leaf_df)
  }
  
  return(leaf_dfs)
}

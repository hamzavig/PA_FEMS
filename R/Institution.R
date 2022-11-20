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
  institution$Operations$Revenues$AddChild("Commisions")
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
#' @export
#' @rdname assignContracts2Tree

assignContracts2Tree <- function(institution, ptf, ...) {
  
  contracts_df <- getPortfolioAsDataFrame(ptf)
  
  return(institution)
}





  
#' Output of the data integration with block.splsda
#'
#' This dataset contains the output of the data integration with block.splsda (package mixOmics).
#'
#' @name res_data_integration
#' @aliases res_data_integration
#' @keywords data
#' 
#'
#'
#' @details
#' The correlation matrix Sigma indicates the correlation between block variables of the same block
#' or the block variables of different blocks. The samples (n=24) follow a multivariate normal distribution with
#' mean equal to 0 and covariance matrix Sigma. The three blocks are built thanks to theses samples : the
#' first block X1 has 300 variables, the second block X2 has 200 variables and the third block has 150 variables.
#' 
#' The response variable has categorical variable with two possible values. In the design matrix, all blocks are 
#' connected. The number of components is equal to 2. 20 variables are selected on each component.
#' 
#' res_data_integration is the output of the function block.splsda (package mixOmics).
#' 
#' 
#' @examples
#' load(system.file("extdata", "res_data_integration.rda", package="visCorVar"))
#' 
NULL

#' block_Y
#'
#'
#' 
#' @name block_Y
#' @aliases block_Y
#' @keywords data 
#' 
#' 
#' @details
#' block_Y is recorded as a dummy matrix that record the membership of each observation.
#' The response variable has categorical variable with two possible values. 
#' 
#' @examples 
#' load(system.file("extdata", "block_Y.rda", package="visCorVar"))
NULL

#' var_interest
#' 
#' @name var_interest
#' @aliases var_interest
#' @keywords data 
#' 
#' @details 
#' var_interest contains block variables that will be in the network.
#' 
#' @examples load(system.file("extdata", "var_interest.rda", package="visCorVar"))
#' 
NULL
#' @title Combinations
#' @description Calculates all possible orders and saves them.
#' @param x number of rows

permArrange <- function(x)
{
  permnArrange <- mclapply(x, permn)
  save(permnArrange, file = "data/permnArrange.RData") 
}

#' This is data to be included in my package
#'
#' @name permnArrange
#' @docType data
#' @keywords data created with permArrange
NULL
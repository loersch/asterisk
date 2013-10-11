#' @title Simple Arrange
#' @description Calculates a simple arrangement of the brackets.
#' @param xx positions of the brackets
#' @param dim distance > 1
#' @param ylim numeric vector of length 2, giving the y coordinates ranges.
#' @param pos t == top, b == bottom

simpleArrange <- function(xx, dim, ylim, pos)
{
  if (pos == "t")
    factor <- -1
  if (pos == "b")
    factor <- 1
  xxd <- xx[dim,]
  if (is.matrix(xxd)) {
    height <- matrix(ncol = nrow(xxd), nrow = nrow(xxd), NA)
    for (i in 1:nrow(xxd)) {
      j <- 1
      new <- T
      while (new | j > nrow(height)) {
        hrow <- height[j,]
        hrow <- sum(!is.na(hrow))
        if (hrow > 0) {
          new.row <- NULL
          for (k in 1:hrow) {
            test <- height[j, k]
            fitl <- xxd[i, 1] < xxd[test, 2] & xxd[i, 1] >= xxd[test, 1]
            fitr <- xxd[i, 2] >= xxd[test, 1] & xxd[i, 2] < xxd[test, 2]   
            new.row <- c(new.row, fitl, fitr)
          }
          new <- any(new.row)         
        } else {
          new <- F
        }
        if (new) j <- j + 1
      }
      hrow <- height[j,]
      hrow <- sum(!is.na(hrow))
      height[j, hrow + 1] <- i    
      xxd[i, 3:4] <- xxd[i, 3:4] + factor * (j-1) * .04 * sum(abs(ylim))
    }
  }
  xxo <- xx[!dim,]
  if (is.vector(xxd)) {
    if (pos == "t") {
      ybr1 <- min(xxd[3]) -  .04 * sum(abs(ylim)) 
      ybr2 <- min(xxd[4]) -  .04 * sum(abs(ylim)) 
    }
    if (pos == "b") {
      ybr1 <- max(xxd[3]) +  .04 * sum(abs(ylim)) 
      ybr2 <- max(xxd[4]) +  .04 * sum(abs(ylim)) 
    }
  } else {
    if (pos == "b") {
      ybr1 <- max(xxd[,3]) + .04 * sum(abs(ylim))
      ybr2 <- max(xxd[,4]) + .04 * sum(abs(ylim)) 
    } 
    if (pos == "t") {
      ybr1 <- min(xxd[,3]) - .04 * sum(abs(ylim))
      ybr2 <- min(xxd[,4]) - .04 * sum(abs(ylim)) 
    }
  }
  if (is.vector(xxo)) {
    xxo[3] <- ybr1 
    xxo[4] <- ybr2 
  } else {
    xxo[,3] <- ybr1  
    xxo[,4] <- ybr2  
  }

  xx[!dim,] <- xxo
  xx[dim,] <- xxd
  
  return(xx)
}

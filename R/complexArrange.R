#' @title Complex Arrange
#' @description Calculates the densest arrangement of the brackets.
#' @param xx positions of the brackets
#' @param ylim numeric vector of length 2, giving the y coordinates ranges.
#' @param factor +1,-1

complexArrange <- function(xx, ylim, factor)
{
  env <- environment()
  arrange.fun <- function(xx, height, unused = NULL)
  {
    for (i in 1:nrow(xx)) {
      if (i %in% unused | is.null(unused)) {
        j <- 1
        new <- T
        while (new | j > nrow(height)) {
          hrow <- height[j,]
          hrow <- sum(!is.na(hrow))
          if (hrow > 0) {
            new.row <- NULL
            for (k in 1:hrow) {
              test <- height[j, k]
              fitl <- xx[i, 1] < xx[test, 2] & xx[i, 1] >= xx[test, 1]
              fitr <- xx[i, 2] >= xx[test, 1] & xx[i, 2] < xx[test, 2]   
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
        if (hrow == 0 & is.null(unused)) height[j, hrow + 1] <- i    
        if (is.null(unused)) height[j, hrow + 1] <- i         
      }   
    }
    return(height)
  }
  xx <- cbind(xx, diff = xx[,2] - xx[,1])
  #xx <- xx[rev(order(xx[,5])),]
  xx <- cbind(xx, row = 1:nrow(xx))
  height <- matrix(ncol = nrow(xx), nrow = nrow(xx), NA)
  height <- arrange.fun(xx, height)
  # Einsortieren der restlichen cases
  used <- as.vector(height)
  unused <- !((xx[,6] %in% used) & xx[,5] != 1)
  if (any(unused)) {
    if(sum(unused) > 9 & max(xx[,5]) > 1) stop("too complex!!!")
    if(sum(unused) < 3 | max(xx[,5]) == 1) {
      N <- xx[,6]
      height <- arrange.fun(xx, height, N[unused])
    } else {
      data(permnArrange, envir = env)
      dat <- permnArrange[[sum(unused)]]
      dat.order <- Inf
      ic <- arrange.fun(xx, height, dat[[1]])
      ind <- apply(ic, 1, function(x) all(is.na(x)))
      ic <- ic[!ind, ]
      for (comb in dat[-1]) {
        tmp <- arrange.fun(xx, height, comb)
        ind <- apply(tmp, 1, function(x) all(is.na(x)))
        tmp <- tmp[!ind, ]
        if (ncol(tmp) < ncol(ic) ) ic <- tmp
      }
      height <- ic 
    }
  }
  # Einsortieren der restlichen cases (diff == 1)
  used <- as.vector(height)
  unused <- !(xx[,6] %in% used)
  if (any(unused)) {
    N <- xx[,6]
    height <- arrange.fun(xx, height, N[unused])
  }

  for (i in 1:nrow(height)) {
    var <- height[i,]
    var <- var[!is.na(var)]
    for (j in var) {
      xx[j, 3:4] <- xx[j, 3:4] + factor * (i-1) * .04 * sum(abs(ylim))
    } 
  }
  xx <- xx[,-(5:6)]
  return(xx)
}

#' @title Calc Asterisk
#' @description Calculates coords. .
#' @param var1 y-Value
#' @param var2 x-Value
#' @param ylim numeric vector of length 2, giving the y coordinates ranges.
#' @param coords x-Achsenticks
#' @param diff numeric vector of length 1, giving the maximum length of the brackets
#' @param pos 1 character code, giving the position of the brackets: "t" top, "b" bottom 
#' @param p.value numeric vector of length 1, giving the minimum p.value
#' @param pos.as 1 character code, giving the position of the asterisks: "h" between, "o" outer
#' @param densely boolean, try to calculate the densest arrangement


calcAsterisk <- function (var1, var2, ylim, coords = NULL, diff = Inf, pos = "b", p.value = 0.05, pos.as = "h", densely = F, extraspace = 0)
{
  raw <- pairwise.t.test(var1, as.factor(var2), p.adj = "holm")$p.value
  raw[raw >= p.value] <- NA
  cors <- raw
  cors[raw < 0.05] <- "*"
  cors[raw < 0.01] <- "**"
  cors[raw < 0.001] <- "***"
  for (i in 1:ncol(cors)) {
    cors[1:nrow(cors) > i + diff - 1, i] <- NA
  }
  
  if (all(is.na(cors) == TRUE)) {
    return(NA)
  } else {
    colnames(cors) <- 1:ncol(cors)
    rownames(cors) <- 1:nrow(cors) + 1
    
    del.c <- apply(cors, 2, function(x) all(is.na(x)))
    cors.del <- cors[, !del.c]
    
    if (is.vector(cors.del)) {
      del.r <- is.na(cors.del)
      cors.del <- cors.del[!del.r]
      cors.del <- as.matrix(cors.del)
      colnames(cors.del) <- colnames(cors)[!del.c]
      rownames(cors.del) <- rownames(cors)[!del.r]
      cors <- cors.del
    } else {
      cors <- cors.del
      del.r <- apply(cors, 1, function(x) all(is.na(x)))
      cors.del <- cors[!del.r,]
      
      if (is.vector(cors.del)) {
        cors.del <- t(as.matrix(cors.del))
        rownames(cors.del) <- rownames(cors)[!del.r]
      }
      cors <- cors.del    
    }
    
    
    cases <- !is.na(cors)
    space <- nchar(cors[cases]) * (0.05 + extraspace)
    x00 <- as.integer(rep(colnames(cors), colSums(cases)))
    
    xc <- as.integer(colnames(cases))
    xr <- as.integer(rownames(cases))
    len <- NULL
    for (i in 1:ncol(cases)){
      len <- c(len, (xr - xc[i])[cases[,i]])
    }
    
    if (pos == "b") {
      y0 <- ylim[1]
      y1 <- y0 + 0.02 * sum(abs(ylim))      
    }
    if (pos == "t") {
      y0 <- ylim[2]
      y1 <- y0 - 0.02 * sum(abs(ylim))
    }
    if (pos == "h") stop("missing")
    
    y0 <- rep(y0, length(x00))
    y1 <- rep(y1, length(x00))
    
    xx <- cbind(x0 = x00, x1 = x00 + len, y0 = y0, y1 = y1)
    
    if (diff > 1 & (anyDuplicated(x00) > 0 | anyDuplicated(x00 + len) > 0)) {
      dim <- len != 1
      if (densely) xx <- complexArrange(xx, ylim, pos)
      if (!densely) xx <- simpleArrange(xx, dim, ylim, pos)
    }
    
    #if (graph == "bar" & is.null(ggplot)) {
    #xx[,1] <- 0.7 + (xx[,1]-1)*1.2
    #xx[,2] <- 0.7 + (xx[,2]-1)*1.2
    #len <- len*1.2
    #}
    
    ## Ab hier wieder in addAsterisk
    
    if (!is.null(coords)) {
      if (is.matrix(coords)) {
        coords <- colMeans(coords)
      }
      dd <- unique(round(diff(coords), 8))
      if (length(dd) != 1) {
        stop("ungueltige Koordinaten")
      } else {
        xx[,1] <- coords[1] + (xx[,1]-1) * dd 
        xx[,2] <- coords[1] + (xx[,2]-1) * dd
        len <- len * dd
      }
    }
    
    if (pos.as == "o") {
      xy <- data.frame(
        x0 = c(xx[,1] + .1, xx[,1] + .1, xx[,2] - .1),
        x1 = c(xx[,1] + .1, xx[,2] - .1, xx[,2] - .1),
        y0 = rep(xx[,3], 3),
        y1 = c(xx[,4], xx[,3], xx[,4])
      )
    }
    if (pos.as == "h") {
      xy <- data.frame(
        x0 = c(xx[,1] + .1, xx[,1] + .1, xx[,1] + len/2 + space, xx[,2] - .1),
        x1 = c(xx[,1] + .1, xx[,1] + len/2 - space, xx[,2] - .1, xx[,2] - .1),
        y0 = rep(xx[,3], 4),
        y1 = c(xx[,4], xx[,3], xx[,3], xx[,4])
      )
    }
    
    xx[,1] <- xx[,1] + len/2
    xx <- data.frame(xx, labels = cors[cases])
    return(cbind(xx = xx, xy = xy))
  }
  }
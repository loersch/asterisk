#' @title Add Asterisk
#' @description Adds brackets with asterisks indicating significance levels to boxplots.
#' @param var1 y-Value
#' @param var2 x-Value
#' @param ylim numeric vector of length 2, giving the y coordinates ranges.
#' @param coords x-Achsenticks
#' @param diff numeric vector of length 1, giving the maximum length of the brackets
#' @param pos 1 character code, giving the position of the brackets: "t" top, "b" bottom
#' @param p.value numeric vector of length 1, giving the minimum p.value
#' @param cex.as asterisk expansion
#' @param pos.as 1 character code, giving the position of the asterisks: "h" between, "o" outer
#' @param densely boolean, try to calculate the densest arrangement
#' @param extraspace extra space for the asteriks
#' @param ggplot a plot created with ggplot
#' @param facet.vars Lay out panels in a grid (vars).
#' @param facet.names Lay out panels in a grid (names).
#' @export addAsterisks
#' @examples
#' \dontrun{
#' # Boxplot
#' boxplot(count ~ spray, data = InsectSprays, ylim = c(0,36))
#' addAsterisks(InsectSprays$count, InsectSprays$spray, ylim = c(0,36), pos = "t")
#' # Barplot
#' counts <- table(mtcars$vs, mtcars$gear)
#' r <- barplot(counts, main="Car Distribution by Gears and VS",
#'         xlab="Number of Gears", col=c("darkblue","red"),
#'         legend = rownames(counts), ylim = c(0,16))
#' addAsterisks(mtcars$vs, mtcars$gear, ylim = c(0,16),
#'              coords = r, pos = "t")
#' # Barplot - beside
#' counts <- table(mtcars$vs, mtcars$gear)
#' r <- barplot(counts, main="Car Distribution by Gears and VS",
#'              xlab="Number of Gears", col=c("darkblue","red"),
#'              legend = rownames(counts), ylim = c(0,17), beside = T)
#' addAsterisks(mtcars$vs, mtcars$gear, ylim = c(0,16), coords = r, pos = "t")
#' }

addAsterisks <- function(var1, var2, ylim, coords = NULL, diff = Inf, pos = "b",
                         p.value = 0.05, cex.as = 1.5, pos.as = "h", densely = F,
                         extraspace = 0, ggplot = NULL, facet.vars = NULL,
                         facet.names = NULL)
{

  if (!is.null(facet.vars) & !is.null(facet.names)) {
    tmp <- data.frame(var1, var2, facet.vars)
    res <- ddply(tmp, names(tmp)[3:ncol(tmp)],
                 function(x, ylim, coords, diff, pos, p.value, pos.as, densely, extraspace) {
                   calcAsterisk(x[,1], x[,2], ylim, coords, diff, pos, p.value, pos.as, densely, extraspace)},
                 ylim, coords, diff, pos, p.value, pos.as, densely, extraspace)
  } else {
    res <- calcAsterisk(var1, var2, ylim, coords, diff, pos, p.value, pos.as, densely, extraspace)
    #list2env(res, environment())
  }
  xx <- res[,!grepl("^xy.", names(res))]
  names(xx) <- sub("xx.", "", names(xx))
  xx <- unique(xx)
  xy <- res[,!grepl("xx", names(res))]
  names(xy) <- sub("xy.", "", names(xy))

  if (!is.null(facet.vars) & !is.null(facet.names)) {
    nfv <- length(facet.names)
    xx <- cbind(xx[,-(1:nfv)], xx[,1:nfv])
    names(xx)[6:ncol(xx)] <- facet.names
    xy <- cbind(xy[,-(1:nfv)], xy[,1:nfv])
    names(xy)[5:ncol(xy)] <- facet.names
  }

  if (pos == "b") {
    factor <- 1
    just <- 1.1
  }
  if (pos == "t") {
    factor <- -1
    just <- .4
  }

  if (!is.null(ggplot)) {
    if (pos.as == "h") {
      p <- ggplot +
        geom_segment(data = xy, aes(x  = x0, xend = x1, y = y0, yend = y1)) +
        geom_text(data = xx, aes(x = x0, y = y0, label = labels), vjust = .8, size = cex.as * 5)
      # size unit(c(0,0,0,0), "lines")
    }
    if (pos.as == "o") {
      p <- ggplot +
        geom_segment(data = xy, aes(x  = x0, xend = x1, y = y0, yend = y1)) +
        geom_text(data = xx, aes(x = x0, y = y0, label = labels), vjust = just, size = cex.as * 5)
    }
    return(p)
  } else {
    if (pos.as == "h") {
      segments(x0 = xy[,1], x1 = xy[,2], y0 = xy[,3], y1 = xy[,4])
      text(xx[,1], xx[,3], xx[,5], cex = cex.as)
    }
    if (pos.as == "o") {
      segments(x0 = xy[,1], x1 = xy[,2], y0 = xy[,3], y1 = xy[,4])
      text(xx[,1], xx[,3] + -1 * factor * .01 * sum(abs(ylim)), xx[,5], cex = cex.as)
    }
  }
}

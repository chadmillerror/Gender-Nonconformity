#' Summary Using Welch–Satterthwaite Equation Degrees of Freedom
#'
#' @param y A numeric vector.
#' @param x A binary vector.
#' @returns A positive number
#' @examples
#' \dontrun{
#' welch_df(y = c(0,1,2,3), x = c("A","B", "A", "B"))
#' welch_df(y = c(0,1,2,3,9), x = c(0,1,0,1,0))
#' }
#' @export
welch_df <- function(y,x) {
  if(length(levels(as.factor(x))) != 2) stop("x must only have two levels")
  if(!is.numeric(y)) stop("y must be numeric")
  a <- y[x==0]
  b <- y[x==1]
  na <- length(na.omit(a))
  nb <- length(na.omit(b))
  num <- (var(a)/na + var(b)/nb)^2
  den <- (sd(a)^4/na^2/(na-1))+(sd(b)^4/nb^2/(nb-1))
  if(num == 0 | den == 0) stop("y must have within-group variability")
  num/den
}

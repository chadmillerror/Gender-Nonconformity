#' @keywords internal
#' @export
csums <- function(x) {
  if(is.null(dim(x))) {
    return(sum(x))
  }
  if(dim(x)[1]==1 | dim(x)[2]==1 ) {
    return(x)
  }
  else {
    return(colSums(x))
  }
}

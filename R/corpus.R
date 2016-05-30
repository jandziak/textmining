#' Function to create new corpus
#'
#' @param x source
#' @return returns new corpus list
#'
#' @export
new_corpus <- function(x = NULL, ...) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  x
}

#' Function to access documents from corpus
#'
#' @param x corpus
#' @param i index
#'
#' @return returns i-th document of corpus x
#'
#' @export
getDoc <- function(x, i) {
  if (length(x) < i)
    stop("index \"i\" out of bands")
  x[i]
}




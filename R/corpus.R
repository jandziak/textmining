#' Function to create new corpus
#'
#' @param x source
#' @return returns new corpus list
#'
#' @export
new_corpus <- function(x = NULL) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  x <- structure(list(text = x, language = rep("en",
                                               length(x))), class = "new_corpus")
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
  if (length(x$text) < i)
    stop("index \"i\" out of bands")
  x$text[i]
}

#' Function to access metadata for documents from new corpus
#'
#' @param x corpus
#' @param i index
#' @param parameter name of metadata to be extracted
#'
#' @return returns i-th document of corpus x
#'
#' @export
getMeta <- function(x, i, parameter) {
  meta_vector <- try(get(parameter, x), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  if (length(meta_vector) < i)
    stop("index \"i\" out of bands")
  meta_vector[i]
}

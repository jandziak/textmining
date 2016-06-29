#' @export
content <- function(x) {
  UseMethod("content")
}

content.tmCorpus <- function(x) {
  lapply(x, getDoc)
}

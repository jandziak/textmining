#' @export
content <- function(x) {
  UseMethod("content")
}

content.tmCorpus <- function(x) {
  lapply(x, getDoc)
}

'content<-'<- function(x, value) {
  UseMethod("content<-")
}

'content<-.tmCorpus' <- function(x, value) {
  x <- lapply(seq_along(x) ,function(l) {x[[l]]$text <- value[[l]]; x[[l]]})
  structure(x, class = "tmCorpus")
}

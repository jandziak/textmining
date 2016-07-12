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

# content.character <- function(x) {
#   x
# }
#
# 'content<-.character' <- function(x, value) {
#   value
# }

'meta<-'<- function(x, tag, ..., value) {
  UseMethod("meta<-")
}

'meta<-.tmCorpus' <- function(x, tag, ..., value) {
  x <- lapply(seq_along(x) ,function(l) setMeta(x[[l]], tag, value[[l]]))
  structure(x, class = "tmCorpus")
}

tm_map <- function(x, FUN, ...)
  UseMethod("tm_map", x)

tm_map.tmCorpus <-function(x, FUN, ...) {
  content(x) <- lapply(content(x), FUN, ...)
  x
}

tm_filter <- function(x, FUN, ...) {
  UseMethod("tm_filter", x)
}

tm_filter.tmCorpus <- function(x, FUN, ...)
  x[tm_index(x, FUN, ...)]

tm_index <- function(x, FUN, ...) {
  UseMethod("tm_index", x)
}

tm_index.tmCorpus <- function(x, FUN, ...) {
  unlist(lapply(x, FUN, ...))
}


content.tmTextDocument <- function(x) {
  x$text
}

c.tmCorpus <- function(..., recursive = FALSE) {
  x <- list(...)
  x <- lapply(x, function(y) {class(y) <- "list"; y})
  x <- do.call("c", lapply(x, function(y) y))
  class(x) <- "tmCorpus"
  meta(x, "id") <-  1:length(x)
  x
}

tagdocument <- function(x, format = "obj", ...){
  tagged_text <- koRpus::treetag(x, ..., format = format)
  tagged_text
}

tagtmCorpus_helper <- function(x, ...) {
  x <- content(x)
  texts <- x %>%
    lapply(tagdocument, ...) %>%
    lapply(koRpus::taggedText)
  texts
}

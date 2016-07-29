# content <- function(x) {
#   UseMethod("content")
# }

#' @export
content.tmCorpus <- function(x) {
  lapply(x, getDoc)
}

#' @export
content.tmParsed <- function(x) {
  lapply(x, getDoc)
}

#' @export
content.tmWordCountsTable <- function(x) {
  lapply(x, getDoc)
}

# 'content<-'<- function(x, value) {
#   UseMethod("content<-")
# }

#' @export
'content<-.tmWordCountsTable' <- function(x, value) {
  x <- lapply(seq_along(x) ,function(l) {x[[l]]$text <- value[[l]]; x[[l]]})
  structure(x, class = "tmWordCountsTable")
}

#' @export
'content<-.tmCorpus' <- function(x, value) {
  x <- lapply(seq_along(x) ,function(l) {x[[l]]$text <- value[[l]]; x[[l]]})
  structure(x, class = "tmCorpus")
}

#' @export
'content<-.tmParsed' <- function(x, value) {
  x <- lapply(seq_along(x) ,function(l) {x[[l]]$text <- value[[l]]; x[[l]]})
  structure(x, class = "tmCorpus")
}

#' @export
content.character <- function(x) {
  x
}

#' @export
'content<-.character' <- function(x, value) {
  value
}

# meta <- function(x, tag, ...) {
#   UseMethod("meta")
# }

#' @export
meta.tmCorpus <- function(x, tag, ...) {
  lapply(x, function(x) getMeta(x, tag))
}

#' @export
meta.tmParsed <- function(x, tag, ...) {
  lapply(x, function(x) getMeta(x, tag))
}

#' @export
meta.tmTextDocument <- function(x, tag, ...) {
  getMeta(x, tag)
}

#' @export
'meta<-.tmTextDocument' <- function(x, tag, ..., value) {
  setMeta(x, tag, value)
}


# 'meta<-'<- function(x, tag, ..., value) {
#   UseMethod("meta<-")
# }

#' @export
'meta<-.tmCorpus' <- function(x, tag, ..., value) {
  x <- lapply(seq_along(x) ,function(l) setMeta(x[[l]], tag, value[[l]]))
  structure(x, class = "tmCorpus")
}

#' @export
'meta<-.tmParsed' <- function(x, tag, ..., value) {
  x <- lapply(seq_along(x) ,function(l) setMeta(x[[l]], tag, value[[l]]))
  structure(x, class = "tmParsed")
}

# tm_map <- function(x, FUN, ...)
#   UseMethod("tm_map", x)

#' @export
tm_map.tmCorpus <-function(x, FUN, ...) {
  content(x) <- lapply(content(x), FUN, ...)
  x
}

# tm_filter <- function(x, FUN, ...) {
#   UseMethod("tm_filter", x)
# }

#' @export
tm_filter.tmCorpus <- function(x, FUN, ...)
  x[tm_index(x, FUN, ...)]

# tm_index <- function(x, FUN, ...) {
#   UseMethod("tm_index", x)
# }

#' @export
tm_index.tmCorpus <- function(x, FUN, ...) {
  unlist(lapply(x, FUN, ...))
}

#' @export
content.tmTextDocument <- function(x) {
  x$text
}

#' @export
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

#' @export
terms.tmTopicModel <- function(x, ...) {
  if(class(x$model) == "jobjRef") {
    terms <- terms.jobjRef(x, ...)
  } else {
    terms <- as.data.frame(topicmodels::terms(x$model,  ...))
  }
  terms
}

sorted_topic_words <- function(topic_no = 1, k = 1, topic_table) {
  names(sort(topic_table$words[topic_no, ], decreasing = T)[1:k])
}

#' @export
terms.jobjRef <- function(x, ...) {
  no_topics <- dim(x$doc_topics)[2]
  topic_table_ <- topic_table(x)
  terms <- sapply(1:no_topics, function(x)
    sorted_topic_words(topic_no = x, topic_table = topic_table_,...))
  if(is.null(dim(terms)))
    terms <- matrix(terms, nrow = 1)
  terms <- as.data.frame(terms)
  names(terms) <- paste("Topic", 1:no_topics)
  terms
}

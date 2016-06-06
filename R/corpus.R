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
  x$text[[i]]
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
getMeta <- function(x, parameter, i=1) {
  if (class(x) == "TextDocument")
    return(get(parameter, x$meta))
  meta_vector <- try(get(parameter, x), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  if (length(meta_vector) < i)
    stop("index \"i\" out of bands")
  meta_vector[i]
}

#' Function to create new parsed corpus
#'
#' @param x source
#' @return returns new corpus list
#'
#' @export
new_parsed <- function(x = NULL) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  x <- structure(list(text = lapply(x, function(y) y), language = rep("en", length(x))),
                 class = "new_parsed")
  x
}

#' Function to parse new corpus
#'
#' @param x corpus
#' @return returns new parsed list
#'
#' @export
parse <- function(x) {
  parsed_doc_list <- sapply(x$text, function(y) strsplit(y, " "))
  names(parsed_doc_list) <- NULL
  x <- new_parsed(parsed_doc_list)
  x
}

#' Function to create new tabularised corpus
#'
#' @param x source
#' @return returns new corpus list
#'
#' @export
new_tabularised <- function(x = NULL) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  x <- structure(list(text = x, language = rep("en", length(x))),
                 class = "WordCountsTable")
  x
}

#' Helper function for tabelarising documents
#'
#' @param x parsed document
#' @return returns table of words and counts
#'
#' @export
tabler <- function(x) {
  x <- x %>%
    table %>%
    as.data.frame
  names(x) <- c("word", "count")
  x
}

#' Function to create tabelarised object from parsed
#'
#' @param x source
#' @return returns new tabelarised object
#'
#' @export
make_tabled <- function(x) {
  list_parsed <- x$text
  s <- lapply(list_parsed, tabler)
  s <- new_tabularised(s)
  s
}

#' Function to store single document with meta data
#'
#' @param x source
#' @return returns new document class
#'
#' @export
new_document <- function(x = NULL) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  x <- structure(list(text = x, meta = list(language = "en")), class = "TextDocument")
  x
}

#' Function to create new corpus
#'
#' @param x source
#' @return returns new corpus list
#'
#' @export
tmCorpus <- function(x = NULL) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  doc_list <- lapply(x, function(y) tmTextDocument(y, id = parent.frame()$i[]))
  x <- structure(doc_list, class = "tmCorpus")
  x
}

#' Function to create new parsed corpus
#'
#' @param x source
#' @return returns new corpus list
#'
#' @export
tmParsed <- function(x = NULL) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  doc_list <- lapply(x, tmTextDocument)
  x <- structure(doc_list, class = "tmParsed")
  x
}

#' Function to create new tabularised corpus
#'
#' @param x source
#' @return returns new corpus list
#'
#' @export
tmWordCountsTable <- function(x = NULL) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  doc_list <- lapply(x, tmTextDocument)
  x <- structure(doc_list, class = "tmWordCountsTable")
  x
}

#' Function to store single document with meta data
#'
#' @param x source
#' @return returns new document class
#'
#' @export
tmTextDocument <- function(x = NULL, ...) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  x <- structure(list(text = x, meta = tmMetaData(...)), class = "tmTextDocument")
  x
}

#' Constructor of metadata class
#'
#' @return returns tmMetaData object
#'
#' @export
tmMetaData <- function(id = 1, language = "en", author = character(0), date = Sys.Date(),
                       title = paste("Document_", id, sep = ""), ...) {
  structure(list(id = id, language = language, author = author, date = date, title = title,
                 ...), class = "tmMetaData")
}

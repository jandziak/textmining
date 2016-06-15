#' Function to create tmCorpus
#'
#' @param x source
#' @return returns tmCorpus object
#'
#' @export
tmCorpus <- function(x = NULL, source = NULL, method = "base") {
  if (!is.null(source)) {
    x <- tmReadDirCorpus(source, method)
  }
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  doc_list <- lapply(x, function(y) tmTextDocument(y, id = parent.frame()$i[]))
  x <- structure(doc_list, class = "tmCorpus")
  x
}

#' Function to create tmParsed
#'
#' @param x source
#' @return returns tmParsed object
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

#' Function to create tmWordCountsTable
#'
#' @param x source
#' @return returns tmWordCountsTable object
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

#' Function to create single tmTextDocument with meta data.
#' The object can store any from of documents: raw (string), parsed or table of words counts.
#'
#' @param x source
#' @return returns tmTextDocument
#'
#' @export
tmTextDocument <- function(x = NULL, ...) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  x <- structure(list(text = x, meta = tmMetaData(...)), class = "tmTextDocument")
  x
}

#' Function to create tmMetaData
#'
#' @param id id of document
#' @param language string language of document
#' @param author string authors name
#' @param date Date - date of reading in/date of publication
#' @param title string title of document
#'
#' @return returns tmMetaData object
#'
#' @export
tmMetaData <- function(id = 1, language = "en", author = character(0), date = Sys.Date(),
                       title = paste("Document_", id, sep = ""), ...) {
  structure(list(id = id, language = language, author = author, date = date, title = title,
                 ...), class = "tmMetaData")
}

#' Function to create tmCorpus
#'
#' @param source directory containing files to read
#' @param method method of data extraction to be used
#'
#' @return returns character vector
tmReadDirCorpus <- function(source, method) {
  if (method == "base") {
    files <- dir(path = source, pattern = "*.txt")
    x <- sapply(files, function(x) read.table(paste("tmp/", x, sep = ""), stringsAsFactors = FALSE))
    x <- as.character(x)
  } else if (method == "stylo") {
    x <- stylo::load.corpus(corpus.dir = source)
    x <- as.character(x)
  } else {
    x <- tm::VCorpus(tm::DirSource(directory = "tmp"))
    x <- sapply(x, NLP::content)
    x <- as.character(x)
  }
  x
}

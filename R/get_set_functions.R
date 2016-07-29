#' Function to access documents for textmining objects
#'
#' @param x object to retrive document fe tmCorpus or tmTextDocument
#' @param i index
#'
#' @return returns i-th document of object x
#' @examples
#' corp <- tmCorpus(c("This is first document", "This is second"))
#' getDoc(corp, 1)
#' getDoc(corp, 2)
#' content(corp)
#'
#' text <- tmTextDocument("Text document new")
#' getDoc(text)
#' content(text)
#'
#' parsed <- tmParsed(list(c("Parsed", "doc", "one"), c("Parsed", "two")))
#' getDoc(parsed, 2)
#' content(parsed)
#'
#' @export
getDoc <- function(x, i = 1) {
  UseMethod("getDoc")
}

#' @export
getDoc.default <- function(x, i = 1) {
  if (length(x) < i)
    stop("index \"i\" out of bands")
  x[[i]]$text
}

#' @export
getDoc.tmTextDocument <- function(x, i = 1) {
  x$text
}

#' Function to access meta data for textmining objects
#'
#' @param x textmining object tmCorpus or tmTextDocument
#' @param i index
#' @param parameter name of metadata to be extracted
#'
#' @return returns i-th document of corpus x
#' @examples
#' corp <- tmCorpus(c("This is first document", "This is second"))
#' getMeta(corp, parameter = "language", 1)
#' getMeta(corp, "title", 2)
#' meta(corp, "title")
#'
#' text <- tmTextDocument("Text document")
#' getMeta(text, "language")
#' meta(text, "title")
#'
#' parsed <- tmParsed(list(c("Parsed", "doc", "one"), c("Parsed", "two")))
#' getMeta(parsed, parameter = "title",  2)
#' meta(parsed, "title")
#'
#' @export
getMeta <- function(x, parameter, i = 1) {
  UseMethod("getMeta")
}

#' @export
getMeta.tmMetaData <- function(x, parameter, i = 1) {
  meta_vector <- try(get(parameter, x), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  meta_vector
}

#' @export
getMeta.tmTextDocument <- function(x, parameter, i = 1) {
  meta_vector <- try(get(parameter, x$meta), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  meta_vector
}

#' @export
getMeta.default <- function(x, parameter, i = 1) {
  if (length(x) < i)
    stop("index \"i\" out of bands")
  meta_vector <- try(get(parameter, x[[i]]$meta), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  meta_vector
}

#' Function to change documents for textmining objects
#'
#' @param x text mining object tmCorpus or tmTextDocument
#' @param doc element to be attached as content
#' @param i index
#'
#' @return x with changed i-th document
#' @examples
#' corp <- tmCorpus(c("This is first document", "This is second"))
#' corp <- setDoc(corp, "Changed doc", 1)
#' getDoc(corp, 1)
#' content(corp) <- c("Content 1", "Content 2")
#' content(corp)
#'
#' text <- tmTextDocument("Text document new")
#' text <- setDoc(text, "Content no 1")
#' setDoc(text)
#' content(text)
#'
#' parsed <- tmParsed(list(c("Parsed", "doc", "one"), c("Parsed", "two")))
#' parsed <- setDoc(parsed, c("Changed", "document"), 2)
#' getDoc(parsed, 2)
#' content(parsed) <- list(c("Changed", "document", "one"), c("Changed", "two"))
#' content(parsed)
#' @export
setDoc <- function(x, doc, i = 1) {
  UseMethod("setDoc")
}

#' @export
setDoc.default <- function(x, doc, i = 1) {
  if (length(x) < i)
    stop("index \"i\" out of bands")
  x[[i]]$text <- doc
  x
}

#' @export
setDoc.tmTextDocument <- function(x, doc, i = 1) {
  x$text <- doc
  x
}

#' Function to access meta data for textmining objects
#'
#' @param x object to retrive document fe tmCorpus or tmTextDocument
#' @param parameter name of metadata to be extracted
#' @param value value of meta data to set
#' @param i index
#'
#' @return returns i-th document of corpus x
#' @examples
#' corp <- tmCorpus(c("This is first document", "This is second"))
#' corp <- setMeta(corp, parameter = "language", "pl", 1)
#' getMeta(corp, parameter = "language", 1)
#' meta(corp, "title") <- c("T1","T2")
#' meta(corp, "title")
#'
#' text <- tmTextDocument("Text document")
#' text <- setMeta(text, "language", "de")
#' getMeta(text, "language")
#' meta(text, "title") <- "New"
#' meta(text, "title")
#'
#' parsed <- tmParsed(list(c("Parsed", "doc", "one"), c("Parsed", "two")))
#' parsed <- setMeta(parsed, parameter = "title",  "New1", 2)
#' getMeta(parsed, parameter = "title",  2)
#' meta(parsed, "title") <- c("T1", "T2")
#' meta(parsed, "title")
#' @export
setMeta <- function(x, parameter, value, i = 1) {
  UseMethod("setMeta")
}

#' @export
setMeta.tmMetaData <- function(x, parameter, value, i = 1) {
  meta_vector <- try(get(parameter, x), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  x[[parameter]] <- value
  x
}

#' @export
setMeta.tmTextDocument <- function(x, parameter, value, i = 1) {
  meta_vector <- try(get(parameter, x$meta), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  x$meta[[parameter]] <- value
  x
}

#' @export
setMeta.default <- function(x, parameter, value, i = 1) {
  if (length(x) < i)
    stop("index \"i\" out of bands")
  meta_vector <- try(get(parameter, x[[i]]$meta), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  x[[i]]$meta[[parameter]] <- value
  x
}

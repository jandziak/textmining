#' Function to access documents for textmining objects
#'
#' @param x object to retrive document fe tmCorpus or tmTextDocument
#' @param i index
#'
#' @return returns i-th document of object x
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
getDoc.tmTextDocument <- function(x) {
  x$text
}

#' Function to access meta data for textmining objects
#'
#' @param x object to retrive document fe tmCorpus or tmTextDocument
#' @param i index
#' @param parameter name of metadata to be extracted
#'
#' @return returns i-th document of corpus x
#'
#' @export
getMeta <- function(x, parameter, i = 1) {
  UseMethod("getMeta")
}

getMeta.tmMetaData <- function(x, parameter) {
  meta_vector <- try(get(parameter, x), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  meta_vector
}

getMeta.tmTextDocument <- function(x, parameter) {
  meta_vector <- try(get(parameter, x$meta), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  meta_vector
}

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
#' @param x object to retrive document fe tmCorpus or tmTextDocument
#' @param i index
#'
#' @return x with changed i-th document
#'
#' @export
setDoc <- function(rd, i = 1, doc) {
  if (length(rd) < i)
    stop("index \"i\" out of bands")
  rd[[i]]$text <- doc
  rd
}

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
getDoc.tmTextDocument <- function(x, i = 1) {
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
#' @param x object to retrive document fe tmCorpus or tmTextDocument
#' @param doc element to be attached as content
#' @param i index
#'
#' @return x with changed i-th document
#'
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
#'
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

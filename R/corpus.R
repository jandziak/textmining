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
  doc_list <- lapply(x, tmTextDocument)
  x <- structure(doc_list, class = "tmCorpus")
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
getDoc <- function(x, i = 1) {
  if (class(x) == "tmTextDocument") {
      x$text
  }
  else {
  if (length(x) < i)
    stop("index \"i\" out of bands")
  x[[i]]$text
  }
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
  if (class(x) != "tmTextDocument") {
    if (length(x) < i)
      stop("index \"i\" out of bands")
    x <- x[[i]]
  }
  meta_vector <- try(get(parameter, x$meta), silent = T)
  if (class(meta_vector) == "try-error")
    stop(paste("There is no metadata: \"", parameter, "\"", sep = ""))
  meta_vector
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
  x <- structure(doc_list,
                 class = "tmParsed")
  x
}


#' Function to parse new corpus
#'
#' @param x corpus
#' @return returns new parsed list
#'
#' @export
parse <- function(x) {
  parsed_doc_list <- sapply(x, function(y) strsplit(getDoc(y), " "))
  names(parsed_doc_list) <- NULL
  x <- tmParsed(parsed_doc_list)
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
  x <- structure(doc_list,
                 class = "tmWordCountsTable")
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
  list_parsed <- lapply(x, function(y) y$text)
  s <- lapply(list_parsed, tabler)
  s <- tmWordCountsTable(s)
  s
}

#' Function to store single document with meta data
#'
#' @param x source
#' @return returns new document class
#'
#' @export
tmTextDocument <- function(x = NULL) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  x <- structure(list(text = x, meta = structure(list(language = "en"))), class = "tmTextDocument")
  x
}

#' Transform function to lowercase
#'
#' @param x source
#' @param FUN transformation function
#' @return returns corpus object after transformation
#'
#' @export
transform <- function(x, FUN){
  x <- x %>% sapply(., function(y) FUN(getDoc(y)))
  tmCorpus(x)
}

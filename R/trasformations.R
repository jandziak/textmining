#' Function to parse tmCorpus. As an outpus we have tmParsed object
#'
#' @param x tmCorpus object
#' @return returns tmParsed object
#'
#' @export
parse <- function(x) {
  parsed_doc_list <- sapply(x, function(y) strsplit(getDoc(y), " "))
  names(parsed_doc_list) <- NULL
  x <- tmParsed(parsed_doc_list)
  x
}

#' Helper function for tabelarising documents
#'
#' @param x parsed tmTextDocument object.
#' @return returns tmTextDocument with table of words counts
#'
#' @export
tabler <- function(x) {
  x <- x %>% table %>% as.data.frame
  names(x) <- c("word", "count")
  x
}

#' Function to create tmWordCountsTable object from tmParsed
#'
#' @param x tmParsed source
#' @return returns tmWordCountsTable object
#'
#' @export
make_tabled <- function(x) {
  list_parsed <- lapply(x, function(y) y$text)
  s <- lapply(list_parsed, tabler)
  s <- tmWordCountsTable(s)
  s
}

#' #' Transform function to lowercase
#' #'
#' #' @param x source
#' #' @param FUN transformation function
#' #' @return returns corpus object after transformation
#' #'
#' #' @export
#' transform <- function(x, FUN) {
#'   x <- x %>% sapply(., function(y) FUN(getDoc(y)))
#'   tmCorpus(x)
#' }

#' Function to create ngram docs
#'
#' @param x tmCorpus object
#' @return returns tmParsed object of ngrams
#'
#' @export
ngram <- function(x, k = 1) {
  parsed_doc_list <- sapply(x, function(y) strsplit(getDoc(y), " "))
  names(parsed_doc_list) <- NULL
  ngram_list <- lapply(parsed_doc_list, function(y) stylo::make.ngrams(y, k))
  x <- tmParsed(ngram_list)
  x
}

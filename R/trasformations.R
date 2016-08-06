#' Function to parse tmCorpus. As an outpus we have tmParsed object.
#'
#' @param x tmCorpus object
#' @return returns tmParsed object
#'
#' @examples
#' corp <- tmCorpus(c("This is first document", "This is second"))
#' parsed <- parse(corp)
#' content(parsed)
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
tabler <- function(x) {
  x <- x %>% table %>% as.data.frame
  names(x) <- c("word", "count")
  x
}

#' Function to create tmWordCountsTable object from tmParsed
#'
#' @param x tmParsed source
#' @return returns tmWordCountsTable object
#' @examples
#' corp <- tmCorpus(c("This is first document", "This is second"))
#' parsed <- parse(corp)
#' tabled <- make_tabled(parsed)
#' content(tabled)
#'
#' @export
make_tabled <- function(x) {
  list_parsed <- lapply(x, function(y) y$text)
  s <- lapply(list_parsed, tabler)
  s <- tmWordCountsTable(s)
  s
}

# #' Transform function to lowercase
# #'
# #' @param x source
# #' @param FUN transformation function
# #' @return returns corpus object after transformation
# #'
# #' @export
# transform <- function(x, FUN) {
#   x <- x %>% sapply(., function(y) FUN(getDoc(y)))
#   tmCorpus(x)
# }

#' Function to create ngram docs
#'
#' @param x tmCorpus object
#' @return returns tmParsed object of ngrams
#' @examples
#' corp <- tmCorpus(c("This is first document", "This is second"))
#' parsed_ngrams <- ngram(corp, k = 2)
#' tabled <- make_tabled(parsed_ngrams)
#' content(tabled)
#'
#' @export
ngram <- function(x, k = 1) {
  parsed_doc_list <- sapply(x, function(y) strsplit(getDoc(y), " "))
  names(parsed_doc_list) <- NULL
  ngram_list <- lapply(parsed_doc_list, function(y) stylo::make.ngrams(y, k))
  x <- tmParsed(ngram_list)
  x
}

#' @export
tm_filter.tmTaggedCorpus <- function(x, FUN, ...)
  x[tm_index(x, FUN, ...)]

#' @export
tm_index.tmTaggedCorpus <- function(x, FUN, ...) {
  unlist(lapply(x, FUN, ...))
}


#' Function to filter tagged text
#'
#' @param x tmTaggedCorpus
#' @param column column name
#' @param value filtered value
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(textmining)
#' corp <- tmCorpus(c("This is corp corp", "Document 2 corp corp"))
#' rd <- tmTaggedCorpus(corp)
#' filtered_tmTaggedCorpus <- filter_documents(rd, "tag", "NP")
#' corpus <- tmCorpus(filtered_tmTaggedCorpus)
#' }
#'
#' @export
filter_documents <- function(x, column, value, ...) {
  z <- lapply(content(x), function(y) y[y[, column] == value, ])
  tmTaggedCorpus(z)
}





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

#' Function to create ngram docs
#'
#' @param x source
#' @return returns tmParsed document of ngrams
#'
#' @export
ngram <- function(x) {
  parsed_doc_list <- sapply(x, function(y) strsplit(getDoc(y), " "))
  names(parsed_doc_list) <- NULL
  k <- length(parsed_doc_list[[1]])
  parsed_doc_list <- sapply(1:(k-1),
                            function(i) paste(parsed_doc_list[[1]][i], parsed_doc_list[[1]][i+1]))
  x <- tmParsed(list(parsed_doc_list))
  x
}

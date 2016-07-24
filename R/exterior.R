TermDocumentMatrix_classes <- c("TermDocumentMatrix", "simple_triplet_matrix")

.TermDocumentMatrix <- function (x, weighting) {
  x <- slam::as.simple_triplet_matrix(x)
  if (!is.null(dimnames(x)))
    names(dimnames(x)) <- c("Terms", "Docs")
  class(x) <- TermDocumentMatrix_classes
  if (is.function(weighting))
    x <- weighting(x)
  else if (is.character(weighting) && (length(weighting) ==
                                       2L))
    attr(x, "weighting") <- weighting
  else stop("invalid weighting")
  x
}

words.PlainTextDocument <- function (x, ...)
  tm::scan_tokenizer(x)

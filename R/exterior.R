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

regexp_token <-
  paste("[A-Za-z",
        # Latin supplement (Western):
        "\U00C0-\U00FF",
        # Latin supplement (Eastern):
        "\U0100-\U01BF",
        # Latin extended (phonetic):
        "\U01C4-\U02AF",
        # modern Greek:
        "\U0386\U0388-\U03FF",
        # Cyrillic:
        "\U0400-\U0481\U048A-\U0527",
        # Hebrew:
        "\U05D0-\U05EA\U05F0-\U05F4",
        # Arabic:
        "\U0620-\U065F\U066E-\U06D3\U06D5\U06DC",
        # extended Latin:
        "\U1E00-\U1EFF",
        # ancient Greek:
        "\U1F00-\U1FBC\U1FC2-\U1FCC\U1FD0-\U1FDB\U1FE0-\U1FEC\U1FF2-\U1FFC",
        # Coptic:
        "\U03E2-\U03EF\U2C80-\U2CF3",
        # Georgian:
        "\U10A0-\U10FF",
        "]+",
        sep="")

#Function returns working directory and changes it temporarely
#onto temp directory and saves there stopwords file

stopwords_temp <- function() {
  wd <- getwd()
  setwd(tempdir())
  if (sum(dir() == rep("en.txt", length(dir()))) != 1) {
    writeLines(c("and,", "me,", "you"), "en.txt")
  }
  return(wd)
}

# Function returns name of stopwordlist file in appropriate temp directory
# As a side effect it changes the directory into tempdir()

stopwords_temp_mallet <- function(stoplist_file) {
  if (length(stoplist_file) != 1){
    rnd <- gsub(pattern = "\\.", x = rnorm(1), replacement = 0)
    name_stoplist_file <- paste("stopwords", rnd, ".txt", sep ="")
    while(sum(rep(name_stoplist_file, length(dir)) == dir()) != 0) {
      rnd <- gsub(pattern = "\\.", x = rnorm(1), replacement = 0)
      name_stoplist_file <- paste("stopwords", rnd, ".txt", sep ="")
    }
    setwd(tempdir())
    writeLines(stoplist_file, name_stoplist_file)
  } else {
    name_stoplist_file <- stoplist_file
  }
  return(name_stoplist_file)
}

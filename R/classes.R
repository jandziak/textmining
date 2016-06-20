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
tmParsed <- function(x = NULL, source = NULL, method = "stylo") {
  if (!is.null(source)) {
    x <- tmReadDirCorpus(source, method, parse = T)
  }
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
tmReadDirCorpus <- function(source, method, parse = F) {
  if (parse == T) {
    if (method == "stylo") {
      x <- stylo::load.corpus.and.parse(corpus.dir = source)
      names(x) <- NULL
    }
  }
  else {
    if (method == "base") {
      files <- dir(path = source, pattern = "*.txt")
      x <- sapply(files, function(x) read.table(paste(source, "/", x, sep = ""), stringsAsFactors = FALSE))
      x <- as.character(x)
    } else if (method == "stylo") {
      x <- stylo::load.corpus(corpus.dir = source)
      x <- as.character(x)
    } else {
      x <- tm::VCorpus(tm::DirSource(directory = source))
      x <- sapply(x, NLP::content)
      x <- as.character(x)
    }
  }
  x
}

#' Function to create tmTopicModel
#'
#' @param model LDA model
#'
#' @return returns object of a class tmTopicModel
#'
#' @export
tmTopicModel <- function(model) {
  class(model) <- "tmTopicModel"
  model
}

#' Helper function to use mallet topic modelling with tmCorpus
#'
#' @param doc single document
#'
#' @return returns named character vector
#'
mallet_prepare <- function(doc) {
  x <- getDoc(doc)
  names(x) <- getMeta(doc, parameter = "title")
  return(x)
}

#' Function to create train Topic Model
#'
#' @param x tmCorpus object
#' @param stoplist_file directory of file with stopwords
#' @param token_regexp regular expression patterns
#' @param no_of_topics number of topics
#' @param alpha_opt parameter of mallet model
#' @param burn_in parameter of mallet model
#' @param train parameter of mallet model
#' @param maximize parameter of mallet model
#'
#'
#' @return returns object of a class tmTopicModel
#'
#' @export
train <- function(x,
                  stoplist_file = "en.txt",
                  token_regexp = "[A-za-z]+",
                  no_of_topics = 20,
                  alpha_opt = 20,
                  burn_in = 50,
                  train = 200,
                  maximize = 10) {
    require(rJava)
  text_array <- sapply(x, mallet_prepare)
  text_array <- as.character(text_array)

  if(is.null(names(text_array)))
    names <- 1:length(text_array)
  else
    names <- names(text_array)
  mallet.instances = mallet::mallet.import(id.array = as.character(names),
                                   text.array = as.character(text_array),
                                   stoplist.file = stoplist_file,
                                   token.regexp = token_regexp)

  topic.model = mallet::MalletLDA(num.topics = no_of_topics)
  topic.model$loadDocuments(mallet.instances)

  vocabulary = topic.model$getVocabulary()
  word.freqs = mallet::mallet.word.freqs(topic.model)


  topic.model$setAlphaOptimization(alpha_opt, burn_in)

  topic.model$train(train)
  topic.model$maximize(maximize)

  tmTopicModel(topic.model)
}

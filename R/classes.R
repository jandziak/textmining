#' Function to create tmCorpus
#'
#' @param x source, for method stylo it supposed to be directory with Corpus files
#' @param ... other arguments dependent on package that we use
#' @param method method of reading the data
#'
#' @return returns tmCorpus object
#'
#' @export
tmCorpus <- function (x = NULL, ..., method = "base") {
  if (method != "base") {
    class(method) <- method
    x <- tmExternalCoprus(method, x, ...)
  } else {
    if (is.null(x)) {
      stop("argument \"x\" is missing")
    }
    doc_list <- lapply(x, function(y) tmTextDocument(y, id = parent.frame()$i[]))
    x <- structure(doc_list, class = "tmCorpus")
  }
  x
}

tmExternalCoprus <- function(method, x, ...) {
  UseMethod("tmExternalCoprus")
}

tmExternalCoprus.tm <- function(method, x, ...) {
  x <- tm::VCorpus(x)
  x <- as.tmCorpus(x)
  x
}

tmExternalCoprus.stylo <- function(method, x, ...) {
  x <- stylo::load.corpus(corpus.dir = x, ...)
  x <- as.tmCorpus(x)
  x
}

#' Function to create tmParsed
#'
#' @param x source
#' @param source directory to corpus on computer
#' @param method method of reading the data
#'
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
#' The object can store any from of documents: raw (string), parsed or table of
#' words counts.
#'
#' @param x source
#' @param ... metadata to set. Can be set as language = "pl" or newmeta = "random"
#'
#' @return returns tmTextDocument
#'
#' @export
tmTextDocument <- function(x = NULL, ...) {
  if (is.null(x)) {
    stop("argument \"x\" is missing")
  }
  x <- structure(list(text = x, meta = tmMetaData(...)),
                 class = "tmTextDocument")
  x
}

#' Function to create tmMetaData
#'
#' @param id id of document
#' @param language string language of document
#' @param author string authors name
#' @param date Date - date of reading in/date of publication
#' @param title string title of document
#' @param ... other metadata
#'
#' @return returns tmMetaData object
#'
#' @export
tmMetaData <- function(id = 1, language = "en", author = character(0),
                       date = Sys.Date(),
                       title = paste("Document_", id, sep = ""), ...) {
  structure(list(id = id, language = language, author = author, date = date,
                 title = title, ...), class = "tmMetaData")
}

#' Function to create tmCorpus
#'
#' @param source directory containing files to read
#' @param method method of data extraction to be used
#' @param parse bool decide if documents should be parsed
#'
#' @return returns character vector
#'
#' @export
tmReadDirCorpus <- function(source, method, parse = F) {
  if (parse == T) {
    if (method == "stylo") {
      x <- stylo::load.corpus.and.parse(corpus.dir = source)
      names(x) <- NULL
    }
  } else {
    if (method == "base") {
      files <- dir(path = source, pattern = "*.txt")
      x <- sapply(files, function(x) read.table(paste(source, "/", x, sep = ""),
                                                stringsAsFactors = FALSE))
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
#' @param ... settings for mallet.doc.topics and mallet.topic.words
#'
#'
#' @return returns object of a class tmTopicModel
#'
#' @export
train <- function(x, stoplist_file = "en.txt", token_regexp = "[A-Za-z]+",
                  no_of_topics = 20, alpha_opt = 20, burn_in = 50, train = 200,
                  maximize = 10, method = "mallet", ...) {
  if (method == "mallet") {
    require(rJava)
    text_array <- sapply(x, mallet_prepare)

    if (is.null(names(text_array)))
      names <- 1:length(text_array) else names <- names(text_array)

    mallet.instances <-
      mallet::mallet.import(id.array = as.character(names),
                            text.array = as.character(text_array),
                            stoplist.file = stoplist_file,
                            token.regexp = token_regexp)

    topic_model <- mallet::MalletLDA(num.topics = no_of_topics)
    topic_model$loadDocuments(mallet.instances)

    vocabulary <- topic_model$getVocabulary()
    word_freqs <- mallet::mallet.word.freqs(topic_model)

    topic_model$setAlphaOptimization(alpha_opt, burn_in)

    topic_model$train(train)
    topic_model$maximize(maximize)

    doc_topics <- mallet::mallet.doc.topics(topic_model, ...)
    topic_words <- mallet::mallet.topic.words(topic_model, ...)

    topic_model <- list(model = topic_model, vocabulary = vocabulary,
                        word_freqs = word_freqs, doc_topics = doc_topics,
                        topic_words = topic_words,
                        doc_names <- as.character(meta(x,"title")))
  } else {
    model <- topicmodels::LDA(x, k = no_of_topics, ...)
    topic_model <- list(model = model)
  }
  tmTopicModel(topic_model)
}

#' Function to predict topic model probabilities for existing topic model
#'
#' @param topic.model tmTopicModel obiect
#' @param x tmCorpus object
#' @param stoplist_file directory of file with stopwords
#' @param token_regexp regular expression patterns
#' @param burn_in parameter of mallet model
#' @param sampling_interval parameter of mallet model
#' @param n_iterations parameter of mallet model
#' @param random_seed parameter of mallet model
#'
#'
#' @return returns the table of topic probabilities
#'
#' @export
predict <- function(topic.model, x, stoplist_file = "en.txt",
                    token_regexp = "[A-Za-z]+", n_iterations = 100,
                    sampling_interval = 10, burn_in = 10, random_seed = NULL) {
  if (attr(class(topic.model$model),"package") == "topicmodels") {
    topicProbabilities <- topicmodels::posterior(topic.model$model,x)
    topicProbabilities <- as.data.frame(topicProbabilities$topics)
    return(as.data.frame(topicProbabilities))
  }

  require(rJava)
  new_texts <- sapply(x, mallet_prepare)

  mallet.instances <-
    mallet::mallet.import(id.array = as.character(names(new_texts)),
                          text.array = as.character(new_texts),
                          stoplist.file = stoplist_file,
                          token.regexp = token_regexp)

  comp_inst <- compatible_instances(as.character(names(new_texts)),
                                    as.character(new_texts),
                                    mallet.instances)

  inf <- inferencer(topic.model$model)
  inf_top <- infer_topics(inf, comp_inst, n_iterations = n_iterations,
                          sampling_interval = sampling_interval,
                          burn_in = burn_in, random_seed = random_seed)
  ml_inst <- as.data.frame(inf_top)

  ml_inst
}

#' Function to calculate topics and words arrays from the mallet model.
#'
#' @param model Mallet model.
#'
#' @return topics Array of the topics.
#' @return words Array of the most important words in topic.
#'
#' @export
topic_table <- function(model){
  doc_topics <- model$doc_topics
  topic_words <- model$topic_words

  colnames(topic_words) = model$vocabulary
  rownames(doc_topics) = model$doc_names
  colnames(doc_topics) = 1:length(doc_topics[1, ])

  list(topics = doc_topics,
       words = topic_words)
}

#' Simple wordcloud visualization of the topics.
#'
#' @param topic_table List returned from the topic_table function.
#' @param topic_id Id of the analised topic.
#' @param no_of_words Number of words to be ploted.
#' @param rot_per wordcloud param
#' @param random_order order of words
#'
#' @export
topic_wordcloud<- function(topic_table, topic_id = 1, no_of_words = 10,
                           rot_per = 0, random_order = FALSE){
  current_topic = sort(topic_table$words[topic_id, ], decreasing = T
                       )[1:no_of_words]
  wordcloud::wordcloud(names(current_topic), current_topic,
                 random.order = random_order,
                 rot.per = rot_per)
}

#' Function to plot topic network
#'
#' @param no_of_words Number of words from each topic to be included in graph
#' @param topic_words Words words extracted from the topic_table function
#'
#' @return network The graph visualising the network
#'
#' @export
gepi_network <- function(no_of_words, topic_words) {
  topic_names <- paste("Topic_", 1:dim(topic_words)[1], sep = "")
  row.names(topic_words) <- topic_names
  frequent_words <- sapply(topic_names, function(x)
    list(sort(topic_words[x,], decreasing = T)[1:no_of_words]))
  topic_words <- sapply(frequent_words, names)
  topic_word_values <- sapply(frequent_words, as.numeric)

  word_list <- unique(as.vector(topic_words))

  names <- c(topic_names, word_list)
  groups <- c(rep(20, length(topic_names)),
              rep(1, length(word_list)))
  size <- c(rep(3, length(topic_names)),
            rep(1, length(word_list)))

  Nodes <- data.frame(name = names,
                      group = groups,
                      size = size)

  from <- sapply(topic_words, function(x) which(Nodes == x)) - 1
  to <- rep(0:(length(topic_names) - 1), each = no_of_words)
  value <- as.vector(topic_word_values)
  value <- value * 20 / max(value)
  Links <- data.frame(source = from,
                      target = to,
                      value = value)

  network <- networkD3::forceNetwork(Links = Links, Nodes = Nodes,
                                     Source = "source", Target = "target",
                                     Value = "value", NodeID = "name",
                                     Group = "group",zoom = TRUE,
                                     Nodesize = "group", opacity = 0.9)

  network
}

as.tmCorpus <- function(x, ...) {
  UseMethod("as.tmCorpus")
}

as.tmCorpus.default <- function(x, ...) {
  tmCorpus(x)
}

as.tmCorpus.VCorpus <- function(x, ...) {
  x <- lapply(x, function(y) y$content)
  names(x) <- NULL
  tmCorpus(x)
}

as.tmCorpus.stylo.corpus <- function(x, ...) {
  x <- lapply(x, function(y) y)
  names(x) <- NULL
  tmCorpus(x)
}

print.tmCorpus <- function(x, ...) {
cat(format(x))
}

format.tmCorpus <-
  function(x, ...)
  {
    c(sprintf("<<%s>>", class(x)[1L]),
      sprintf("Content:  documents: %d", length(x)))
  }

meta <- function(x, tag, ...) {
  UseMethod("meta")
}

meta.tmCorpus <- function(x, tag, ...) {
  lapply(x, function(x) getMeta(x, tag))
}

termFreq_tm <-
  function (doc, control = list())
  {
    doc <- tm::PlainTextDocument(x = getDoc(doc),
                             language = getMeta(doc, "language"))
    stopifnot(is.list(control))
    .tokenize <- control$tokenize
    if (is.null(.tokenize) || identical(.tokenize, "words"))
      .tokenize <- tm:::words.PlainTextDocument
    else if (identical(.tokenize, "MC"))
      .tokenize <- tm::MC_tokenizer
    else if (identical(.tokenize, "scan"))
      .tokenize <- tm::scan_tokenizer
    else if (NLP::is.Span_Tokenizer(.tokenize))
      .tokenize <- NLP::as.Token_Tokenizer(.tokenize)
    if (is.function(.tokenize))
      txt <- .tokenize(doc)
    else stop("invalid tokenizer")
    .tolower <- control$tolower
    if (is.null(.tolower) || isTRUE(.tolower))
      .tolower <- tolower
    if (is.function(.tolower))
      txt <- .tolower(txt)
    .removePunctuation <- control$removePunctuation
    if (isTRUE(.removePunctuation))
      .removePunctuation <- tm::removePunctuation
    else if (is.list(.removePunctuation))
      .removePunctuation <- function(x) do.call(removePunctuation,
                                                c(list(x), control$removePunctuation))
    .removeNumbers <- control$removeNumbers
    if (isTRUE(.removeNumbers))
      .removeNumbers <- tm::removeNumbers
    .stopwords <- control$stopwords
    if (isTRUE(.stopwords))
      .stopwords <- function(x) x[is.na(match(x, tm::stopwords(meta(doc,
                                                                "language"))))]
    else if (is.character(.stopwords))
      .stopwords <- function(x) x[is.na(match(x, control$stopwords))]
    .stemming <- control$stemming
    if (isTRUE(.stemming))
      .stemming <- function(x) tm::stemDocument(x, meta(doc, "language"))
    or <- c("removePunctuation", "removeNumbers", "stopwords",
            "stemming")
    nc <- names(control)
    n <- nc[nc %in% or]
    for (name in sprintf(".%s", c(n, setdiff(or, n)))) {
      g <- get(name)
      if (is.function(g))
        txt <- g(txt)
    }
    if (is.null(txt))
      return(setNames(integer(0), character(0)))
    dictionary <- control$dictionary
    tab <- if (is.null(dictionary))
      table(txt)
    else table(factor(txt, levels = dictionary))
    bl <- control$bounds$local
    if (length(bl) == 2L && is.numeric(bl))
      tab <- tab[(tab >= bl[1]) & (tab <= bl[2])]
    nc <- nchar(names(tab), type = "chars")
    wl <- control$wordLengths
    lb <- if (is.numeric(wl[1]))
      wl[1]
    else 3
    ub <- if (is.numeric(wl[2]))
      wl[2]
    else Inf
    tab <- tab[(nc >= lb) & (nc <= ub)]
    storage.mode(tab) <- "integer"
    class(tab) <- c("term_frequency", class(tab))
    tab
  }

TermDocumentMatrix <- function(x, control = list()) {
  UseMethod("TermDocumentMatrix", x)
}

TermDocumentMatrix.tmCorpus <-
  function(x, control = list())
  {
    stopifnot(is.list(control))

    tflist <- lapply(x, termFreq_tm, control)
    tflist <- lapply(tflist, function(y) y[y > 0])

    v <- unlist(tflist)
    i <- names(v)
    allTerms <- sort(unique(as.character(if (is.null(control$dictionary)) i
                                         else control$dictionary)))
    i <- match(i, allTerms)
    j <- rep(seq_along(x), sapply(tflist, length))
    docs <- as.character(meta(x, "id"))
    if (length(docs) != length(x)) {
      warning("invalid document identifiers")
      docs <- NULL
    }

    m <- slam::simple_triplet_matrix(i = i, j = j, v = as.numeric(v),
                               nrow = length(allTerms),
                               ncol = length(x),
                               dimnames =
                                 list(Terms = allTerms,
                                      Docs = docs))

    bg <- control$bounds$global
    if (length(bg) == 2L && is.numeric(bg)) {
      rs <- row_sums(m > 0)
      m <- m[(rs >= bg[1]) & (rs <= bg[2]), ]
    }

    weighting <- control$weighting
    if (is.null(weighting))
      weighting <- tm::weightTf

    tm:::.TermDocumentMatrix(m, weighting)
  }


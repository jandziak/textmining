  #' Function to create tmCorpus
  #'
  #' @param x source, for package stylo it supposed to be directory with Corpus files
  #' @param ... other arguments dependent on package that we use
  #' @param package package of reading the data could be tm or stylo
  #'
  #' @return returns tmCorpus object
  #' @examples
  #' corp <- tmCorpus(c("This is corp", "Document 2"))
  #' corp <- tmCorpus(list("This is corp", "Document 2"))
  #' corp <- tmCorpus(VectorSource(c("This is corp", "Document 2")), package = "tm")
  #'\dontrun{
  #' corp <- tmCorpus(DirSource("directory"), package = "tm")
  #' corp <- tmCorpus("directory", package = "stylo")
  #'}
  #'
  #' @export
  tmCorpus <- function (x = NULL, ..., package = "base") {
    if (package != "base") {
      class(package) <- package
      x <- tmExternalCoprus(package, x, ...)
    } else {
      if (is.null(x)) {
        stop("argument \"x\" is missing")
      }
      doc_list <- lapply(x, function(y) tmTextDocument(y, id = parent.frame()$i[],...))
      x <- structure(doc_list, class = "tmCorpus")
    }
    x
  }

  tmExternalCoprus <- function(package, x, ...) {
    UseMethod("tmExternalCoprus")
  }

  tmExternalCoprus.tm <- function(package, x,
                                  readerControl = list(reader = reader(x),
                                                       language = "en"), ...) {
    x <- tm::VCorpus(x, readerControl)
    x <- as.tmCorpus(x, ...)
    x
  }

  tmExternalCoprus.stylo <- function(package, x, files = "all",
                                     encoding = "native.enc", ...) {
    x <- stylo::load.corpus(files, corpus.dir = x, encoding)
    x <- as.tmCorpus(x, ...)
    x
  }

  #' Function to create tmParsed
  #'
  #' @param x source
  #' @param source directory to corpus on computer
  #' @param package package of reading the data could be tm or stylo
  #'
  #' @return returns tmParsed object
  #' @examples
  #' corp <- tmCorpus(c("This is corp", "Document 2"))
  #' parsed <- parse(corp)
  #' parsed_ngram <- ngram(corp, k = 2)
  #' parsed <- tmParsed(list(c("This", "is", "corp"), c("Document", "2")))
  #'\dontrun{
  #' corp <- tmCorpus("directory", package = "stylo")
  #' parsed <- parse(corp)
  #' parsed_ngram <- ngram(corp, k = 2)
  #' parsed <- tmParsed(source = "directory", package = "stylo")
  #'}
  #'
  #' @export
  tmParsed <- function(x = NULL, source = NULL, package = "stylo") {
    if (!is.null(source)) {
      x <- tmReadDirCorpus(source, package, parse = T)
    }
    if (is.null(x)) {
      stop("argument \"x\" is missing")
    }
    doc_list <- lapply(x, function(y) tmTextDocument(y, id = parent.frame()$i[]))
    x <- structure(doc_list, class = "tmParsed")
    x
  }

  #' Function to create tmWordCountsTable
  #'
  #' @param x source tmParsed
  #' @return returns tmWordCountsTable object
  #' @examples
  #' corp <- tmCorpus(c("This is corp", "Document two is this"))
  #' parsed <- parse(corp)
  #' parsed_ngram <- ngram(corp, k = 2)
  #' tabled <- make_tabled(parsed)
  #' tabled_ngram <- make_tabled(parsed_ngram)
  #'\dontrun{
  #' corp <- tmCorpus("directory", package = "stylo")
  #' parsed <- parse(corp)
  #' parsed_ngram <- ngram(corp, k = 2)
  #' parsed <- tmParsed(source = "directory", package = "stylo")
  #'}
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
  #' @examples
  #' text <-  tmTextDocument("This is text")
  #' text2 <- tmTextDocument("This is text", language = "en", title = "My test")
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
  #' @examples
  #' tmMetaData(id = 1, author = "Author", newmetadata = "random")
  #' tmMetaData(title = "New title")
  #'
  #' @export
  tmMetaData <- function(id = 1, language = "en", author = character(0),
                         date = Sys.Date(),
                         title = paste("Document_", id, sep = ""), ...) {
    structure(list(id = id, language = language, author = author, date = date,
                   title = title, ...), class = "tmMetaData")
  }

  tmReadDirCorpus <- function(source, package, parse = F, files = "all",
                              encoding = "UTF-8",
                              readerControl = list(reader = reader(x),
                                                   language = "en")) {
    if (parse == T) {
      if (package == "stylo") {
        x <- stylo::load.corpus.and.parse(corpus.dir = source)
        names(x) <- NULL
      }
    } else {
      if (package == "base") {
        files <- dir(path = source, pattern = "*.txt")
        x <- sapply(files, function(x) read.table(paste(source, "/", x, sep = ""),
                                                  stringsAsFactors = FALSE))
        x <- as.character(x)
      } else if (package == "stylo") {
        x <- stylo::load.corpus(files, corpus.dir = source, encoding)
        x <- as.character(x)
      } else {
        x <- tm::VCorpus(tm::DirSource(directory = source), readerControl)
        x <- sapply(x, NLP::content)
        x <- as.character(x)
      }
    }
    x
  }

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
  #' @param k number of topics
  #' @param alpha_opt parameter of mallet model
  #' @param burn_in parameter of mallet model
  #' @param train parameter of mallet model
  #' @param maximize parameter of mallet model
  #' @param ... settings for mallet.doc.topics and mallet.topic.words
  #'
  #' @return returns object of a class tmTopicModel
  #'
  #' @export
  train <- function(x, ...) {
    UseMethod("train")
  }

  #' @export
  train.tmCorpus <- function(x, k = 20,
                             stoplist_file = "en.txt",
                             token_regexp = regexp_token,
                             alpha_opt = 20,
                             burn_in = 50, train = 200,
                             maximize = 10, package = "mallet", ...) {

      trained <- train_mallet_helper(x, k, stoplist_file,
                                     token_regexp,  alpha_opt,
                          burn_in, train, maximize)

    tmTopicModel(trained)
  }

  # DocumentTermMatrix <-
  #   function(x, control = list())
  #     t(TermDocumentMatrix(x, control))

  #' @export
  train.DocumentTermMatrix <- function(x, k = 20, ...) {

    trained <- train_topicmodels_helper(x, k, ...)
    tmTopicModel(trained)
  }

  train_mallet_helper <- function(x, k = 20,
                                  stoplist_file = "en.txt",
                                  token_regexp = regexp_token,
                                  alpha_opt = 20,
                                  burn_in = 50, train = 200,
                                  maximize = 10, ...) {
    wd <- getwd()
    if (length(stoplist_file) != 1){
      rnd <- gsub(pattern = "\\.", x = rnorm(1), replacement = 0)
      name_stoplist_file <- paste("stopwords", rnd, ".txt", sep ="")
      print(name_stoplist_file)
      while(sum(rep(name_stoplist_file, length(dir)) == dir()) != 0) {
        rnd <- gsub(pattern = "\\.", x = rnorm(1), replacement = 0)
        name_stoplist_file <- paste("stopwords", rnd, ".txt", sep ="")
        print(name_stoplist_file)
      }
      setwd(tempdir())
      writeLines(stoplist_file, name_stoplist_file)
    } else {
      name_stoplist_file <- stoplist_file
    }

    text_array <- sapply(x, mallet_prepare)
    if (is.null(names(text_array)))
      names <- 1:length(text_array) else names <- names(text_array)

    mallet.instances <-
      mallet::mallet.import(id.array = as.character(names),
                            text.array = as.character(text_array),
                            stoplist.file = name_stoplist_file,
                            token.regexp = token_regexp)

    topic_model <- mallet::MalletLDA(num.topics = k)
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
                        doc_names = as.character(meta(x,"title")),
                        name_stoplist_file = name_stoplist_file,
                        stoplist_file = stoplist_file)
    setwd(wd)
    topic_model
  }


  train_topicmodels_helper <- function(x, k = 20, ...) {
      model <- topicmodels::LDA(x, k = k, ...)
      vocabulary <- topicmodels::terms(model)
      word_topic_log_prob <- model@gamma
      topic_model <- list(model = model, vocabulary = vocabulary,
                          word_topic_log_prob = word_topic_log_prob)
      topic_model
  }

  #' #' Function to predict topic model probabilities for existing topic model
  #' #'
  #' #' @param topic.model tmTopicModel obiect
  #' #' @param x tmCorpus object
  #' #' @param stoplist_file directory of file with stopwords
  #' #' @param token_regexp regular expression patterns
  #' #' @param burn_in parameter of mallet model
  #' #' @param sampling_interval parameter of mallet model
  #' #' @param n_iterations parameter of mallet model
  #' #' @param random_seed parameter of mallet model
  #' #' @param change predict structure so it fits normal
  #' #'
  #' #' @return returns the table of topic probabilities
  #' #'
  #' #' @export
  #' predict <- function(topic.model, x, stoplist_file = "en.txt",
  #'                     token_regexp = regexp_token, n_iterations = 100,
  #'                     sampling_interval = 10, burn_in = 10, random_seed = NULL) {
  #'   UseMethod("predict")
  #' }

  #' @export
  predict.tmTopicModel <- function(object, x, stoplist_file = "en.txt",
                      token_regexp = regexp_token, n_iterations = 100,
                      sampling_interval = 10, burn_in = 10, random_seed = NULL) {
    predict(object$model, x, object$stoplist_file, token_regexp, n_iterations,
            sampling_interval, burn_in, random_seed)
  }

  #' @export
  predict.LDA  <- function(object, x, stoplist_file = "en.txt",
                              token_regexp = regexp_token, n_iterations = 100,
                              sampling_interval = 10, burn_in = 10,
                              random_seed = NULL) {
    topicProbabilities <- topicmodels::posterior(object,x)
    topicProbabilities <- as.data.frame(topicProbabilities$topics)
    as.data.frame(topicProbabilities)
  }

  #' @export
  predict.jobjRef <- function(object, x, stoplist_file = "en.txt",
                              token_regexp = regexp_token, n_iterations = 100,
                              sampling_interval = 10, burn_in = 10,
                              random_seed = NULL) {

    predict_mallet_helper(object, x, stoplist_file, token_regexp, n_iterations,
                          sampling_interval, burn_in, random_seed)
  }

  predict_mallet_helper <- function(model, x, stoplist_file = "en.txt",
                                    token_regexp = regexp_token, n_iterations = 100,
                                    sampling_interval = 10, burn_in = 10,
                                    random_seed = NULL) {
    wd <- getwd()
    if (length(stoplist_file) != 1){
      rnd <- gsub(pattern = "\\.", x = rnorm(1), replacement = 0)
      name_stoplist_file <- paste("stopwords", rnd, ".txt", sep ="")
      print(name_stoplist_file)
      while(sum(rep(name_stoplist_file, length(dir)) == dir()) != 0) {
        rnd <- gsub(pattern = "\\.", x = rnorm(1), replacement = 0)
        name_stoplist_file <- paste("stopwords", rnd, ".txt", sep ="")
        print(name_stoplist_file)
      }
      setwd(tempdir())
      writeLines(stoplist_file, name_stoplist_file)
    } else {
      name_stoplist_file <- stoplist_file
    }

    new_texts <- sapply(x, mallet_prepare)

    mallet.instances <-
      mallet::mallet.import(id.array = as.character(names(new_texts)),
                            text.array = as.character(new_texts),
                            stoplist.file = name_stoplist_file,
                            token.regexp = token_regexp)

    comp_inst <- compatible_instances(as.character(names(new_texts)),
                                      as.character(new_texts),
                                      mallet.instances)

    inf <- inferencer(model)
    inf_top <- infer_topics(inf, comp_inst, n_iterations = n_iterations,
                            sampling_interval = sampling_interval,
                            burn_in = burn_in, random_seed = random_seed)
    ml_inst <- as.data.frame(inf_top)
    setwd(wd)
    ml_inst
  }

  #' Function to calculate topics and words arrays from the mallet model.
  #'
  #' @param model tmTopicModel mallet type model.
  #'
  #' @return topics Array of the topics.
  #' @return words Array of the most important words in topic.
  #' @examples
  #' \dontrun{
  #' library(rJava)
  #' x <- tmCorpus(lapply(1:100, function(x) paste(sample(LETTERS, 11),
  #'                                               collapse = "")))
  #'
  #' model <- train(x)
  #' new_x <- tmCorpus(lapply(1:100, function(x) paste(sample(LETTERS, 11),
  #'                                                   collapse = "")))
  #'
  #'
  #' topic_table(model)
  #'
  #' y <- DocumentTermMatrix(x)
  #' rownames(y) <- meta(x, "title")
  #' jss_TM <-
  #'   list(VEM = train(y, k = k, control = list(seed = SEED)),
  #'        VEM_fixed = train(y, k = k,
  #'                          control = list(estimate.alpha = FALSE, seed = SEED)),
  #'        Gibbs = train(y, k = k, method = "Gibbs",
  #'                      control = list(seed = SEED, burnin = 1000,
  #'                                     thin = 100, iter = 1000)))
  #' pred_VEM <- predict(jss_TM$VEM, new_x)
  #'}
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
  #' @param model tmTopicModel object
  #' @param topic_id Id of the analised topic.
  #' @param k number of words to be ploted.
  #' @param rot_per wordcloud param
  #' @param random_order order of words
  #' @examples
  #' \dontrun{
  #' library(rJava)
  #' x <- tmCorpus(lapply(1:100, function(x) paste(sample(LETTERS, 11),
  #'                                               collapse = "")))
  #'
  #' model <- train(x)
  #' topic_wordcloud(model, topic_id = 2, k = 11)
  #'}
  #' @export
  topic_wordcloud<- function(model, topic_id = 1, k = 10,
                             rot_per = 0, random_order = FALSE){
    if(class(model$model) == "jobjRef")
    {
      topic_table <- topic_table(model)
      current_topic = sort(topic_table$words[topic_id, ], decreasing = T
      )[1:k]
    } else {
      topic_table <- posterior(model$model)
      current_topic = sort(topic_table$terms[topic_id, ], decreasing = T
      )[1:k]
    }
    wordcloud::wordcloud(names(current_topic), current_topic,
                         random.order = random_order,
                         rot.per = rot_per)
  }

  #' Function to plot topic network
  #'
  #' @param k Number of words from each topic to be included in graph
  #' @param topic_words Words words extracted from the topic_table function
  #'
  #' @return network The graph visualising the network
  #' @examples
  #' \dontrun{
  #' x <- tmCorpus(rep("as, a , a ,s  l k l l k k j h g f f hg j aaa", 100))
  #' require(rJava)
  #' model <- suppressMessages(train(x))
  #' table_topic <- topic_table(model)
  #' network <- gepi_network(10 ,table_topic$words)
  #'}
  #' @export
  gepi_network <- function(k, topic_words) {
    topic_names <- paste("Topic_", 1:dim(topic_words)[1], sep = "")
    row.names(topic_words) <- topic_names
    frequent_words <- sapply(topic_names, function(x)
      list(sort(topic_words[x,], decreasing = T)[1:k]))
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
    to <- rep(0:(length(topic_names) - 1), each = k)
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

  #' @export
  as.tmCorpus <- function(x, ...) {
    UseMethod("as.tmCorpus")
  }

  #' @export
  as.tmCorpus.default <- function(x, ...) {
    tmCorpus(x, ...)
  }

  #' @export
  as.tmCorpus.VCorpus <- function(x, ...) {
    x <- lapply(x, function(y) y$content)
    names(x) <- NULL
    tmCorpus(x, ...)
  }

  #' @export
  as.tmCorpus.stylo.corpus <- function(x, ...) {
    x <- lapply(seq_along(x), function(i) paste(x[[i]], collapse = " "))
    names(x) <- NULL
    tmCorpus(x, ...)
  }

  #' @export
  print.tmCorpus <- function(x, ...) {
  cat(format(x))
  }

  format.tmCorpus <-
    function(x, ...)
    {
      c(sprintf("<<%s>>", class(x)[1L]),
        sprintf("Content:  documents: %d", length(x)))
    }

  termFreq_tm <-
    function (doc, control = list())
    {
      doc <- tm::PlainTextDocument(x = getDoc(doc),
                               language = getMeta(doc, "language"))
      stopifnot(is.list(control))
      .tokenize <- control$tokenize
      if (is.null(.tokenize) || identical(.tokenize, "words"))
        .tokenize <- words.PlainTextDocument
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

  # TermDocumentMatrix <- function(x, control = list()) {
  #   UseMethod("TermDocumentMatrix", x)
  # }

  #' @export
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
        rs <- slam::row_sums(m > 0)
        m <- m[(rs >= bg[1]) & (rs <= bg[2]), ]
      }

      weighting <- control$weighting
      if (is.null(weighting))
        weighting <- tm::weightTf

      .TermDocumentMatrix(m, weighting)
    }


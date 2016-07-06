context("Exterior packages corpus read")
test_that("Reading corpus from other files works", {
  dir.create("tmp")
  writeLines("This is written file", "tmp/tmp1.txt")
  writeLines("This is written file 2", "tmp/tmp2.txt")
  rd <- try(tmCorpus(x = "tmp", method = "stylo"))
  unlink("tmp", recursive = TRUE)
  expect_equal(rd, tmCorpus(c("This is written file",
                              "This is written file 2")))
})

test_that("Reading corpus from other files works", {
  dir.create("tmp")
  writeLines("This is written file", "tmp/tmp1.txt")
  writeLines("This is written file 2", "tmp/tmp2.txt")
  rd <- try(tmCorpus(x = tm::DirSource("tmp"), method = "tm"))
  unlink("tmp", recursive = TRUE)
  expect_equal(rd, tmCorpus(c("This is written file",
                              "This is written file 2")))
})

test_that("Reading corpus from tm VectorSource", {
  rd <- try(tmCorpus(x = tm::VectorSource(c("This is written file",
                                        "This is written file 2")),
                     method = "tm"))
  expect_equal(rd, tmCorpus(c("This is written file",
                              "This is written file 2")))
})

# context("Read and parse document")
# test_that("Read and parse using stylo", {
#   dir.create("tmp")
#   writeLines("this is written file", "tmp/tmp1.txt")
#   rd <- try(tmParsed(source = "tmp", method = "stylo"))
#   unlink("tmp", recursive = TRUE)
#   expect_equal(rd, tmParsed(list(c("this", "is", "written", "file"))))
# })
#
# context("Train function attached to the corpus")
# test_that("Class of the model is tmTopicModel", {
#   x <- tmCorpus(rep("as, a , a ,s  l k l l k k j h g f f hg j aaa", 100))
#   model <- suppressMessages(train(x))
#   expect_equal(class(model), "tmTopicModel")
# })
#
# context("Predict function attached to the corpus")
# test_that("Predict class for topic model tmTopicModel", {
#   x <- tmCorpus(rep("as, a , a ,s  l k l l k k j h g f f hg j aaa", 100))
#   model <- suppressMessages(train(x))
#   y <- tmCorpus(rep("as, aa a a a a ada s a a da d as a", 100))
#   pred <- predict(model, y)
#   expect_equal(class(pred), "data.frame")
# })
#
# test_that("Topic table function", {
#   x <- tmCorpus(rep("as, a , a ,s  l k l l k k j h g f f hg j aaa", 100))
#   model <- suppressMessages(train(x))
#   n1 <- topic_table(model, x)
#   expect_equal(names(n1), c("topics", "words"))
# })
#
# test_that("Gepi graphics", {
#   x <- tmCorpus(rep("as, a , a ,s  l k l l k k j h g f f hg j aaa", 100))
#   model <- suppressMessages(train(x))
#   table_topic <- topic_table(model, x)
#   network <- gepi_network(10 ,table_topic$words)
#   expect_equal(class(network), c("forceNetwork", "htmlwidget"))
# })
#
# test_that("Content returns the list content of a tmCorpus", {
#   x <- tmCorpus(c("Nothing is here", "Just list content"))
#   content_x <- content(x)
#   expect_equal(content_x, list("Nothing is here", "Just list content"))
# })
#
# test_that("Content settr for the list content of a tmCorpus", {
#   x <- tmCorpus(c("Nothing is here", "Just list content"))
#   content(x) <- list("a", "b")
#   expect_equal(content(x), list("a", "b"))
# })
#
# test_that("tm_map works for tmCorpus", {
#   x <- tmCorpus(c("Nothing is here", "Just list content"))
#   content(x) <- list("a", "b")
#   x <- tm_map(x, tm::removeWords, tm::stopwords("english"))
#   expect_equal(content(x), list("", "b"))
# })

test_that("Create tmCorpus from vector with as.tmCorpus", {
  x <- c("sa", "As")
  x <- as.tmCorpus(x)
  expect_equal(x, tmCorpus(c("sa", "As")))
})

test_that("Create tmCorpus from VCorpus with as.tmCorpus", {
  x <- c("sa", "As")
  V_x <- tm::VCorpus(tm::VectorSource(x))
  x<- as.tmCorpus(V_x)
  expect_equal(x, tmCorpus(c("sa", "As")))
})

test_that("Create tmCorpus from stylo.corpus with as.tmCorpus", {
  dir.create("tmp")
  writeLines("This is written file", "tmp/tmp1.txt")
  writeLines("This is written file 2", "tmp/tmp2.txt")
  x <- stylo::load.corpus(corpus.dir = "tmp")
  unlink("tmp", recursive = TRUE)
  x <- as.tmCorpus(x)
  expect_equal(x, tmCorpus(c("This is written file", "This is written file 2")))
})

test_that("Content settr for the list content of a tmCorpus", {
  x <- tmCorpus(c("Nothing is here", "Just list content"))
  meta(x, "language")
  expect_equal(meta(x, "language"), list("en", "en"))
})

test_that("TermDocumentMatrix from tmCorpus", {
  x <- tmCorpus(c("doc1", "doc2"))
  y <- TermDocumentMatrix(x)
  expect_equal(class(TermDocumentMatrix(x))[1], "TermDocumentMatrix")
})

test_that("Set meta data for tmCorpus with meta function", {
  x <- tmCorpus(c("doc1", "doc2"))
  meta(x, tag = "language") <- c("pl", "pl")
  expect_equal(getMeta(x, "language", 2), "pl")
})

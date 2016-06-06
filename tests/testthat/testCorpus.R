context("Test new corpus")
test_that("No argument in constructor", {
  expect_error(new_corpus(), "argument \"x\" is missing")
})

test_that("One argument constructor", {
  rd <- new_corpus("doc_1")
  expect_equal(getDoc(rd, 1), "doc_1")
})

test_that("Simple vector constructor", {
  rd <- new_corpus(c("doc_1", "doc_2"))
  expect_equal(getDoc(rd, 2), "doc_2")
})

test_that("Complex vector constructor", {
  rd <- new_corpus(paste("doc_", 1:100, sep = ""))
  expect_equal(getDoc(rd, 14), "doc_14")
})

test_that("Getter index out of bands", {
  rd <- new_corpus(c("doc_1", "doc_2"))
  expect_error(getDoc(rd, 3), "index \"i\" out of bands")
})

test_that("Meta data one article", {
  rd <- new_corpus(c("doc_1"))
  expect_equal(getMeta(rd, "language"), "en")
})

test_that("Meta data two articles", {
  rd <- new_corpus(c("doc_1", "doc_2"))
  expect_equal(getMeta(rd, "language"), "en")
})

test_that("Meta data many articles", {
  rd <- new_corpus(paste("doc_", 1:100, sep = ""))
  expect_equal(getMeta(rd, "language", 15), "en")
})

test_that("Getter for meta data out of bands", {
  rd <- new_corpus(c("doc_1", "doc_2"))
  expect_error(getMeta(rd, "language", 3), "index \"i\" out of bands")
})

test_that("Getter for meta data, no metadata with a given name", {
  rd <- new_corpus(c("doc_1", "doc_2"))
  expect_error(getMeta(rd, "author", 3), "There is no metadata: \"author\"")
})

test_that("Class value for new corpus is appropriate", {
  rd <- new_corpus("doc_1")
  expect_equal(class(rd), "new_corpus")
})

context("Test parsed text")
test_that("No argument in constructor of parsed text", {
  expect_error(new_parsed(), "argument \"x\" is missing")
})

test_that("One argument constructor", {
  rd <- new_parsed(list(c("parsed", "doc_1")))
  expect_equal(getDoc(rd, 1), c("parsed", "doc_1"))
})

test_that("Simple list constructor of new parsed text", {
  rd <- new_parsed(list(c("parsed", "doc_1"), c("parsed", "doc_2")))
  expect_equal(getDoc(rd, 2), c("parsed", "doc_2"))
})

test_that("Complex vector constructor", {
  doc_list <- lapply(1:100, function(y) c("parsed", paste("doc_", y, sep = "")))
  rd <- new_parsed(doc_list)
  expect_equal(getDoc(rd, 14), c("parsed", "doc_14"))
})

test_that("Getter index out of bands new parsed ", {
  rd <- new_parsed(list(c("parsed", "doc_1"), c("parsed", "doc_2")))
  expect_error(getDoc(rd, 3), "index \"i\" out of bands")
})

test_that("Meta data one article parsed text", {
  rd <- new_parsed(list(c("parsed", "doc_1")))
  expect_equal(getMeta(rd, "language"), "en")
})

test_that("Meta data two articles", {
  rd <- new_parsed(list(c("parsed", "doc_1"), c("parsed", "doc_2")))
  expect_equal(getMeta(rd, "language"), "en")
})

test_that("Meta data many articles", {
  doc_list <- lapply(1:100, function(y) c("parsed", paste("doc_", y, sep = "")))
  rd <- new_parsed(doc_list)
  expect_equal(getMeta(rd, "language", 15), "en")
})

test_that("Getter for meta data out of bands", {
  rd <- new_parsed(list(c("parsed", "doc_1"), c("parsed", "doc_2")))
  expect_error(getMeta(rd, "language", 3), "index \"i\" out of bands")
})

test_that("Getter for meta data, no metadata with a given name for new parsed", {
  rd <- new_parsed(list(c("parsed", "doc_1"), c("parsed", "doc_2")))
  expect_error(getMeta(rd, "author", 2), "There is no metadata: \"author\"")
})

test_that("Getter for meta data, no metadata with a given name for new parsed", {
  rd <- new_parsed(list(c("parsed", "doc_1"), c("parsed", "doc_2")))
  expect_error(getMeta(rd, "author", 2), "There is no metadata: \"author\"")
})

context("Parse corpus")
test_that("Parse single simple article",{
  rd <- new_corpus("Not parsed doc_1")
  rd <- parse(rd)
  expect_equal(rd, new_parsed(list(c("Not", "parsed", "doc_1"))))
})

test_that("Parse two simple articles",{
  rd <- new_corpus(c("Not parsed doc_1", "Not parsed doc_2"))
  rd <- parse(rd)
  test <- new_parsed(list(c("Not", "parsed", "doc_1"), c("Not", "parsed", "doc_2")))
  expect_equal(getDoc(rd,1), getDoc(test, 1))
  expect_equal(getDoc(rd,2), getDoc(test, 2))
  expect_equal(rd, test)
})

context("Test tabelarised text")
test_that("No argument in constructor of tabularised", {
  expect_error(new_tabularised(), "argument \"x\" is missing")
})

test_that("Tabelarise one doc", {
  df <- data.frame(word = c("be", "am"), count = c(1, 2))
  rd <- new_tabularised(list(df))
  expect_equal(getDoc(rd, 1), df)
})

test_that("Tabelarised two doc", {
  df_1 <- data.frame(word = c("be", "am"), count = c(1, 2))
  df_2 <- data.frame(word = c("have", "has"), count = c(2, 1))
  rd <- new_tabularised(list(df_1, df_2))
  expect_equal(getDoc(rd, 1), df_1)
  expect_equal(getDoc(rd, 2), df_2)
})

test_that("Tabelarised many doc", {
  df <- data.frame(word = c("be", "am"), count = c(1, 2))
  list_df <- lapply(1:100, function(x) df)
  rd <- new_tabularised(list_df)
  expect_equal(getDoc(rd, 1), df)
  expect_equal(getDoc(rd, 50), df)
})

test_that("Tabelarised meta data for one doc exists", {
  df <- data.frame(word = c("be", "am"), count = c(1, 2))
  rd <- new_tabularised(list(df))
  expect_equal(getMeta(rd, "language"), "en")
})

test_that("Tabelarised  meta data for two doc exists", {
  df_1 <- data.frame(word = c("be", "am"), count = c(1, 2))
  df_2 <- data.frame(word = c("have", "has"), count = c(2, 1))
  rd <- new_tabularised(list(df_1, df_2))
  expect_equal(getMeta(rd, "language"), "en")
  expect_equal(getMeta(rd, "language", 2), "en")
})

test_that("Tabelarised  meta data for many doc exists", {
  df <- data.frame(word = c("be", "am"), count = c(1, 2))
  list_df <- lapply(1:100, function(x) df)
  rd <- new_tabularised(list_df)
  expect_equal(getMeta(rd, "language"), "en")
  expect_equal(getMeta(rd, "language", 50), "en")
})

test_that("Getter for meta data out of bands", {
  df_1 <- data.frame(word = c("be", "am"), count = c(1, 2))
  df_2 <- data.frame(word = c("have", "has"), count = c(2, 1))
  rd <- new_tabularised(list(df_1, df_2))
  expect_error(getMeta(rd, "language", 3), "index \"i\" out of bands")
})

test_that("Getter for meta data, no metadata with a given name for new parsed", {
  df_1 <- data.frame(word = c("be", "am"), count = c(1, 2))
  df_2 <- data.frame(word = c("have", "has"), count = c(2, 1))
  rd <- new_tabularised(list(df_1, df_2))
  expect_error(getMeta(rd, "author", 2), "There is no metadata: \"author\"")
})

test_that("Document test for class", {
  df_1 <- data.frame(word = c("be", "am"), count = c(1, 2))
  rd <- new_tabularised(list(df_1))
  expect_equal(class(rd), "WordCountsTable")
})


context("Table function")
test_that("Parsed one document tabelarises", {
  rd <- new_parsed(list(c("doc_1", "parsed")))
  rd <- make_tabled(rd)
  df <- data.frame(word = c("doc_1", "parsed"), count = c(1, 1))
  expect_equal(rd, new_tabularised(list(df)))
})

context("Document class")
test_that("Document class created with single text", {
  rd <- new_document("doc_1")
  expect_equal(rd$text, "doc_1")
})

test_that("Document class no argument in constructor", {
  expect_error(new_document(), "argument \"x\" is missing")
})

test_that("Document test for class", {
  rd <- new_document("doc")
  expect_equal(class(rd), "TextDocument")
})

test_that("Meta data for document class", {
  rd <- new_document("doc_1")
  expect_equal(getMeta(rd,"language"), "en")
})

test_that("Getter for meta data, no metadata with a given name for TextDocument", {
  rd <- new_document("doc_1")
  expect_error(getMeta(rd, "randomMeta"), "There is no metadata: \"randomMeta\"")
})

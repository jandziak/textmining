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
  expect_equal(getMeta(rd, 1, "language"), "en")
})

test_that("Meta data two articles", {
  rd <- new_corpus(c("doc_1", "doc_2"))
  expect_equal(getMeta(rd, 1, "language"), "en")
})

test_that("Meta data many articles", {
  rd <- new_corpus(paste("doc_", 1:100, sep = ""))
  expect_equal(getMeta(rd, 15, "language"), "en")
})

test_that("Getter for meta data out of bands", {
  rd <- new_corpus(c("doc_1", "doc_2"))
  expect_error(getMeta(rd, 3, "language"), "index \"i\" out of bands")
})

test_that("Getter for meta data, no metadata with a given name", {
  rd <- new_corpus(c("doc_1", "doc_2"))
  expect_error(getMeta(rd, 3, "author"), "There is no metadata: \"author\"")
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



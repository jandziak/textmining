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


test_that("No argument in constructor", {
  expect_error(new_corpus(), "argument \"x\" is missing")
})

test_that("One argument constructor", {
  rd <- new_corpus("ala")
  expect_equal(getDoc(rd, 1), "ala")
})

test_that("Simple vector constructor", {
  rd <- new_corpus(c("doc_1", "doc_2"))
  expect_equal(getDoc(rd, 2), "doc_2")
})

test_that("Complex vector constructor", {
  rd <- new_corpus(paste("doc_", 1:100, sep = ""))
  expect_equal(getDoc(rd, 14), "doc_14")
})

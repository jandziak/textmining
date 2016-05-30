test_that("No argument in constructor", {
  expect_error(new_corpus(), 'argument "x" is missing')
})

test_that("One argument constructor", {
  rd <- new_corpus("ala")
  expect_equal(getDoc(rd, 1), "ala")
})

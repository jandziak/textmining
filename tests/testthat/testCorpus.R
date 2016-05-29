test_that("No argument in constructor", {
  expect_error(new_corpus(), 'argument "x" is missing')
})

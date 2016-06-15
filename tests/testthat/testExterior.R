context("Exterior packages corpus read")
test_that("Reading corpus from other files works", {
  dir.create("tmp")
  write.table("This is written file", "tmp/tmp1.txt")
  rd <- tmCorpus(source = "tmp")
  unlink("tmp", recursive = TRUE)
  expect_equal(rd, tmCorpus("This is written file"))
})

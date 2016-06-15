context("Exterior packages corpus read")
test_that("Reading corpus from other files works", {
  dir.create("tmp")
  write.table("This is written file", "tmp/tmp1.txt")
  rd <- try(tmCorpus(source = "tmp"))
  unlink("tmp", recursive = TRUE)
  expect_equal(rd, tmCorpus("This is written file"))
})
test_that("Reading corpus from other files works", {
  dir.create("tmp")
  write.table("This is written file", "tmp/tmp1.txt")
  write.table("This is written file 2", "tmp/tmp2.txt")
  rd <- try(tmCorpus(source = "tmp"))
  unlink("tmp", recursive = TRUE)
  expect_equal(rd, tmCorpus(c("This is written file","This is written file 2")))
})

test_that("Reading corpus from other files works", {
  dir.create("tmp")
  writeLines("This is written file", "tmp/tmp1.txt")
  writeLines("This is written file 2", "tmp/tmp2.txt")
  rd <- try(tmCorpus(source = "tmp", method = "stylo"))
  unlink("tmp", recursive = TRUE)
  expect_equal(rd, tmCorpus(c("This is written file","This is written file 2")))
})

test_that("Reading corpus from other files works", {
  dir.create("tmp")
  writeLines("This is written file", "tmp/tmp1.txt")
  writeLines("This is written file 2", "tmp/tmp2.txt")
  rd <- try(tmCorpus(source = "tmp", method = "tm"))
  unlink("tmp", recursive = TRUE)
  expect_equal(rd, tmCorpus(c("This is written file","This is written file 2")))
})

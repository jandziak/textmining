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

context("Read and parse document")
test_that("Read and parse using stylo", {
  dir.create("tmp")
  writeLines("this is written file", "tmp/tmp1.txt")
  rd <- try(tmParsed(source = "tmp", method = "stylo"))
  unlink("tmp", recursive = TRUE)
  expect_equal(rd, tmParsed(list(c("this", "is", "written", "file"))))
})

context("Train function attached to the corpus")
test_that("Class of the model is tmTopicModel", {
  x <- tmCorpus(rep("as, a , a ,s  l k l l k k j h g f f hg j aaa", 100))
  model <- suppressMessages(train(x))
  expect_equal(class(model), "tmTopicModel")
})

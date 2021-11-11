context("test-corplingr_freqlist_leipzig_all.R")

test_that("freqlist of two or more corpus produces message", {
  expect_message(freqlist_leipzig_all(leipzig_path = leipzig_corpus_path[2:3]), regexp = "^You chose to generate frequency list for all words")
})

flist <- freqlist_leipzig_all(leipzig_path = leipzig_corpus_path[2:3])
test_that("output of freqlist_leipzig_all of two or more corpus files is a list", {
  expect_output(str(flist), regexp = "^List of ")
})

flist <- freqlist_leipzig_all(leipzig_path = leipzig_corpus_path[2])
test_that("output of freqlist_leipzig_all of one corpus file is a tibble data frame", {
  expect_output(str(flist), regexp = "(tibble|tbl(_df)?|data\\.frame)")
})

context("test-corplingr_freqlist_leipzig_all.R")

test_that("throwing error when the leipzig file path does not exist in the current working directory", {
  expect_error(freqlist_leipzig_all(leipzig_path = c("path/number/1.txt", "path/number/2.txt")), regexp = "'path/number/1.txt' does not exist in current working directory")
})

test_that("freqlist of two or more corpus produces message", {
  expect_message(freqlist_leipzig_all(leipzig_path = c("mini_leipzig.txt", "mini_leipzig_01.txt")), regexp = "^You chose to generate frequency list for all words")
})

data("flist_mini")
test_that("output of freqlist_leipzig_all of two or more corpus files is a list", {
  expect_output(str(flist_mini), regexp = "^List of ")
})

flist <- flist_mini[[1]]
test_that("output of freqlist_leipzig_all of one corpus file is a tibble data frame", {
  expect_output(str(flist), regexp = "(tibble|tbl(_df)?|data\\.frame)")
})

test_that("output of freqlist_leipzig_all of one corpus file is a tibble data frame", {
  expect_output(str(freqlist_leipzig_all(leipzig_path = c("mini_leipzig.txt"))), regexp = "(tibble|tbl(_df)?|data\\.frame)")
})

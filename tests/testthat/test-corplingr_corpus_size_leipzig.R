context("test-corplingr_corpus_size_leipzig.R")

test_that("output corpus_size_leipzig is a tibble/data frame", {
  expect_output(str(corpus_size_leipzig("mini_leipzig_01.txt")), "(tbl_df|data\\.frame)")
})

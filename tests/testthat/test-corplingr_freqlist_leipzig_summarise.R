context("test-corplingr_freqlist_leipzig_summarise.R")

# create mini tibble for input data
df <- tibble::tibble(match = c("memberi", "memberikan", "memberi", "memberikan"), corpus_id = c("corpus1", "corpus1", "corpus2", "corpus2"), n = as.integer(c(6394, 11710, 2214, 5213)))

test_that("output of freqlist_leipzig_summarise is a data frame", {
  expect_output(str(freqlist_leipzig_summarise(df = df, group_var = match)), regexp = "(tbl_df|tbl|data\\.frame|tibble)")
})

test_that("output of freqlist_leipzig_summarise is a data frame", {
  expect_output(str(freqlist_leipzig_summarise(df = df, group_var = match, descending = FALSE)), regexp = "(tbl_df|tbl|data\\.frame|tibble)")
})

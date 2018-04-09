context("test-corplingr_concord_others.R")

data("demo_corpus_leipzig")
corp <- demo_corpus_leipzig$ind_mixed_2012_1M

test_that("non-matched pattern produced message", {
  expect_message(concord_others(corp, "\\bgjdanjadnalkd\\b"), "^Sorry; no match found")
})

df_out <- concord_others(corp, "\\bjalan\\b")
test_that("output of concord_others is a tibble", {
  expect_output(str(df_out), "^Classes.+?tbl_df")
})

context("test-corplingr_concord_leipzig.R")

data("demo_corpus_leipzig")
corpora <- demo_corpus_leipzig$ind_mixed_2012_1M
case_insensitive <- TRUE
r <- 1
pattern <- "\\bhjgnvjdsk\\b"
match_id <- stringr::str_which(corpora, stringr::regex(pattern[r], ignore_case = case_insensitive))

test_that("unfound match return integer zero", {
  expect_equal(length(match_id), 0L)
})

# rgx <- "\\bterkalahkan\\b"
# concord_df <- concord_leipzig(leipzig_path = leipzig_corpus_path[3],
#                               pattern = rgx,
#                               case_insensitive = TRUE)
#
# test_that("output of concord_leipzig is a data frame", {
#   expect_output(str(concord_df), "Classes ‘tbl_df’, ‘tbl’ and 'data.frame'")
# })

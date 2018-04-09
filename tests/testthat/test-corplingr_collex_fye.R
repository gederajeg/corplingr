context("test-corplingr_collex_fye.R")

# test data
dfid <- colloc_default(corpus_list = demo_corpus_id,
                       pattern = "jalan",
                       tokenise_corpus_to_sentence = TRUE)
collex_tb <- collex_prepare(dfid, "r1")
collex_tb <- dplyr::mutate(collex_tb, collstr = collex_fye(a, corpus_size, n_w_in_corp, n_pattern))

test_that("output of collex_fye is double", {
  expect_type(collex_tb[["collstr"]], "double")
})

collex_tb[3,2] <- NA
test_that("collex_fye produce error when the value of 'a' is NA", {
  expect_error(collex_fye(collex_tb$a,
                          collex_tb$corpus_size,
                          collex_tb$n_w_in_corp,
                          collex_tb$n_pattern),
               regexp = "missing\\svalue\\swhere\\sTRUE/FALSE")
})

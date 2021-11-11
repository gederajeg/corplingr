context("test-corplingr_collex_fye.R")

# test data
dfid <- corplingr::colloc_default(corpus_list = demo_corpus_id,
                       pattern = "\\bjalan\\b",
                       tokenise_corpus_to_sentence = TRUE)
collex_tb <- corplingr::collex_prepare(dfid, "r1")
collex_tb <- dplyr::mutate(collex_tb, collstr = purrr::pmap_dbl(list(a, a_exp, n_w_in_corp, corpus_size, n_pattern), corplingr::collex_fye,
                                                     two_sided = FALSE, collstr_res = TRUE))

test_that("output of collex_fye is double", {
  expect_type(collex_tb[["collstr"]], "double")
})

collex_tb <- collex_prepare(dfid, "r1")
collex_tb <- dplyr::mutate(collex_tb, collstr = purrr::pmap_dbl(list(a, a_exp, n_w_in_corp, corpus_size, n_pattern), collex_fye, two_sided = TRUE, collstr_res = FALSE))

test_that("output of collex_fye is double", {
  expect_type(collex_tb[["collstr"]], "double")
})

collex_tb[3,2] <- NA
test_that("collex_fye produce error when the value of 'a' is NA", {
  expect_error(collex_fye(collex_tb$a[3],
                          collex_tb$a_exp[3],
                          collex_tb$corpus_size[3],
                          collex_tb$n_w_in_corp[3],
                          collex_tb$n_pattern[3]),
               regexp = "missing\\svalue\\swhere\\sTRUE/FALSE")
})

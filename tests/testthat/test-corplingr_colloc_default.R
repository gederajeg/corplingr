context("test-corplingr_colloc_default.R")

data("demo_corpus_bali")
output <- corplingr::colloc_default(corpus_list = demo_corpus_bali,
                     pattern = "^nuju$",
                     window = "b",
                     span = 3)
test_that("output colloc_default is a list of three elements", {
  expect_output(str(output), "List of 3")
})


# test input list of corpus without names
demo_corpus_bali1 <- unname(demo_corpus_bali)
output <- corplingr::colloc_default(corpus_list = demo_corpus_bali1,
                                    pattern = "^nuju$",
                                    window = "b",
                                    span = 3)
test_that("output colloc_default is a list of three elements", {
  expect_output(str(output), "List of 3")
})


data("obali_colloc_output_test")
test_that("output colloc_default (now with filepath) is a list of three elements", {
  expect_output(str(output), "List of 3")
})

test_that("non-matching pattern returns error", {
  expect_error(corplingr::colloc_default(corpus_path = c("mini_leipzig.txt", "mini_leipzig_01.txt"), pattern = "^sdhsajk$", window = "b", span = 3), regexp = "^No match is found\\!$")
})

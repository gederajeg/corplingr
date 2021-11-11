context("test-corplingr_colloc_default.R")

output <- corplingr::colloc_default(corpus_list = demo_corpus_bali,
                     pattern = "^nuju$",
                     window = "b",
                     span = 3)
test_that("output colloc_default is a list of three elements", {
  expect_output(str(output), "List of 3")
})

output <- corplingr::colloc_default(corpus_path = orti_bali_path[1:200],
                                    pattern = "^nuju$",
                                    window = "b",
                                    span = 3)
test_that("output colloc_default (now with filepath) is a list of three elements", {
  expect_output(str(output), "List of 3")
})

test_that("non-matching pattern returns error", {
  expect_error(corplingr::colloc_default(corpus_list = demo_corpus_bali,
                                         pattern = "^sdhsajk$",
                                         window = "b",
                                         span = 3), regexp = "^No match is found\\!$")
})

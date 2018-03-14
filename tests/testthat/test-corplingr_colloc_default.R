context("test-corplingr_colloc_default.R")

output <- colloc_default(corpus_list = demo_corpus_bali,
                     pattern = "^xhfgl$",
                     window = "b",
                     span = 3)
test_that("output colloc_default is a list of three elements", {
  expect_output(str(output), "List of 3")
})

test_that("non-matching pattern return zero tibble for collocates", {
  expect_identical(dim(output$colloc_tb)[1], 0L)
})

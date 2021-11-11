context("test-corplingr_colloc_leipzig.R")

colloc <- colloc_leipzig(leipzig_path = "mini_leipzig.txt",
                         pattern = "\\byang\\b",
                         window = "b",
                         span = 3,
                         save_results = FALSE,
                         to_lower_colloc = TRUE)
test_that("output of colloc_leipzig is a list of two elements", {
  expect_output(str(colloc), "List of 2")
})

colloc <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                         pattern = "\\bfdhfghkjlvhj\\b",
                         window = "r",
                         span = 3,
                         save_results = FALSE,
                         to_lower_colloc = TRUE)

test_that("output of non-match of colloc_leipzig is null", {
  expect_null(colloc, info = NULL)
})

test_that("output of non-match produce message", {
  expect_message(colloc_leipzig(leipzig_path = "mini_leipzig.txt",
                                pattern = "\\bfdhfghkjlvhj\\b",
                                window = "b",
                                span = 3,
                                save_results = FALSE,
                                to_lower_colloc = TRUE))
})

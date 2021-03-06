context("test-corplingr_colloc_leipzig.R")

colloc <- colloc_leipzig(leipzig_path = leipzig_corpus_path[2],
                         pattern = "\\bterelakkan\\b",
                         window = "b",
                         span = 3,
                         save_results = FALSE,
                         to_lower_colloc = TRUE)
test_that("output of colloc_leipzig is a list of two elements", {
  expect_output(str(colloc), "List of 2")
})

colloc <- colloc_leipzig(leipzig_path = leipzig_corpus_path[2],
                         pattern = "\\bfdhfghkjlvhj\\b",
                         window = "b",
                         span = 3,
                         save_results = FALSE,
                         to_lower_colloc = TRUE)

test_that("output of non-match of colloc_leipzig is null", {
  expect_null(colloc, info = NULL)
})

test_that("output of non-match produce message", {
  expect_message(colloc_leipzig(leipzig_path = leipzig_corpus_path[2],
                                pattern = "\\bfdhfghkjlvhj\\b",
                                window = "b",
                                span = 3,
                                save_results = FALSE,
                                to_lower_colloc = TRUE))
})

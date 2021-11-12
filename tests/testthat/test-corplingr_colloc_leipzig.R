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

# test the code for saving output file
test_that("saving output file produces message", {expect_message(colloc_leipzig(leipzig_path = "mini_leipzig.txt",
               pattern = "\\byang\\b",
               window = "b",
               span = 3,
               save_results = TRUE,
               to_lower_colloc = FALSE, coll_output_name = "colloc_tibble_leipzig.txt", sent_output_name = "colloc_sent_match_leipzig.txt"), regexp = "been saved")})


colloc <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                         pattern = "\\bfdhfghkjlvhj\\b",
                         window = "r",
                         span = 3,
                         save_results = FALSE,
                         to_lower_colloc = TRUE)

test_that("output of non-match of colloc_leipzig is null", {
  expect_null(colloc, info = NULL)
})

# test input list of corpus without names
demo_corpus_leipzig1 <- unname(demo_corpus_leipzig)
colloc <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig1,
                         pattern = "\\bfdhfghkjlvhj\\b",
                         window = "b",
                         span = 3,
                         save_results = FALSE,
                         to_lower_colloc = FALSE)

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


colloc <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                         pattern = "\\bmengenai\\b",
                         window = "b",
                         span = 1,
                         save_results = FALSE,
                         to_lower_colloc = TRUE)

test_that("output of colloc_leipzig is a list of two elements", {
  expect_output(str(colloc), "List of 2")
})

colloc2 <- colloc_leipzig(leipzig_corpus_list = demo_corpus_leipzig,
                         pattern = "\\bdepan\\b",
                         window = "l",
                         span = 1,
                         save_results = FALSE,
                         to_lower_colloc = TRUE)

test_that("output of colloc_leipzig is a list of two elements", {
  expect_output(str(colloc2), "List of 2")
})

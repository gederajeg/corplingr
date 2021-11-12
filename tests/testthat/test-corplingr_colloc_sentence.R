context("test-corplingr_colloc_sentence.R")

# test data from the output of colloc_sentence
df <- colloc_sentence(corpus_path = "mini_leipzig.txt",
                      leipzig_input = TRUE,
                      pattern = "\\byang\\b",
                      window = "l",
                      span = 1,
                      case_insensitive = TRUE,
                      to_lower_colloc = TRUE,
                      save_interim_results = FALSE)

test_that("unidentified pattern produces message", {
  expect_message(colloc_sentence(corpus_path = "mini_leipzig.txt",
                                 leipzig_input = TRUE,
                                 pattern = "\\bxhfjanfkamfkda\\b",
                                 window = "l",
                                 span = 1,
                                 case_insensitive = TRUE,
                                 to_lower_colloc = TRUE,
                                 save_interim_results = FALSE),
                 regexp = "^SORRY! No match")
})


test_that("the output of colloc_sentence is a tibble with six columns", {
  expect_identical(dim(df)[2], 6L)
})


test_that("output of colloc_sentence is a tibble", {
  expect_output(str(df), regexp = "(tibble|tbl(_df)?|data\\.frame)")
})


df <- colloc_sentence(corpus_path = "mini_leipzig.txt",
                      leipzig_input = TRUE,
                      pattern = "\\bhgjdanjnvdkrjeijkama\\b",
                      window = "l",
                      span = 1,
                      case_insensitive = TRUE,
                      to_lower_colloc = TRUE,
                      save_interim_results = FALSE)

test_that("the output of colloc_sentence without match is a tibble with zero observations and variables", {
  expect_identical(dim(df), c(0L, 0L))
})


test_that("saving the interim result produces message", {expect_message(colloc_sentence(corpus_path = "mini_leipzig.txt",
                               leipzig_input = TRUE,
                               pattern = "\\byang\\b",
                               window = "l",
                               span = 1,
                               case_insensitive = TRUE,
                               to_lower_colloc = TRUE,
                               save_interim_results = TRUE,
                               coll_output_name = "colloc_tibble_out.txt"), regexp = "Saving the interim|All done")})

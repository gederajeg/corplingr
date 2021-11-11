context("test-corplingr_freqlist_leipzig_citation.R")

filepath <- "mini_leipzig.txt"
pattern <- "\\bmengasia\\b"


test_that("no match return message", {
  expect_message(freqlist_leipzig_citation(pattern, filepath),
                 "No match found at all!")
})

pattern <- "\\bakan\\b"
outmatch <- freqlist_leipzig_citation(pattern, filepath)
test_that("row number is greater than zero when match found", {
  expect_true(nrow(outmatch) > 0)
})

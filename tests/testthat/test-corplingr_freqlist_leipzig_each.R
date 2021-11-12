context("test-corplingr_freqlist_leipzig_each.R")

# prepare the input
regex <- "\\bakan\\b"
corpus.path <- c("mini_leipzig_01.txt", "mini_leipzig.txt")

# generate the frequency count
wlist <- freqlist_leipzig_each(pattern = regex, leipzig_path = corpus.path, case_insensitive = TRUE)

wlist1 <- freqlist_leipzig_each(pattern = regex, leipzig_path = corpus.path, case_insensitive = FALSE)

wlist2 <- freqlist_leipzig_each(pattern = "\\bmemberi(kan)\\b", leipzig_path = corpus.path, case_insensitive = FALSE)

test_that("output freqlist_leipzig_each is tibble", {
  expect_output(str(wlist), "(tibble|tbl_df|data\\.frame)")
})

test_that("output freqlist_leipzig_each is tibble", {
  expect_output(str(wlist1), "(tibble|tbl_df|data\\.frame)")
})

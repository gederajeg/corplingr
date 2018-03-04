# function to check the corpus file in which a word is found
# PARAMETER
# @df = a tibble data frame containing wordlist derived via the 'create_wordlist()' function
# @regex = regular expressions/strings for the searched words/expressions
check_wordlist_corpus <- function(df = NULL, regex = NULL) {
  out1 <- df %>%
    filter(str_detect(match, regex))
  out_corpus <- out1 %>%
    .$corpus_id %>%
    str_c(collapse="|")
  outall <- list(df=out1, corpus=out_corpus)
  return(outall)
}

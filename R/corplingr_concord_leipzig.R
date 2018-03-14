#' Generate tidyverse-style concordances for the Leipzig Corpora
#'
#' @description The function produces tibble-output concordances for Leipzig Corpora files.
#' @param leipzig_path character stringrs of (i) file names of the Leipzig corpus if they are already in the working directory, or (ii) the complete filepath to each of the Leipzig corpus files to be processed.
#' @param pattern regular expressions/exact patterns for the target pattern.
#' @param case_insensitive whether the search ignores case (TRUE -- the default) or not (FALSE).
#' @return A concordance-tibble consisting of (i) \code{start} and \code{end} character position of the \code{pattern} in the corpus; (ii) \code{corpus} file names and \code{sentence IDs} in which the \code{pattern} is found; (iii) \code{left}, \code{node}, and \code{right} concordance-style view.
#' @importFrom purrr map
#' @importFrom purrr map_df
#' @importFrom purrr pmap
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr quo
#' @importFrom dplyr quo_name
#' @importFrom stringr str_which
#' @importFrom stringr str_length
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_count
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_trim
#' @importFrom stringr str_sub<-
#' @importFrom rlang sym
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' # load the required packages
#' library(tidyverse)
#' library(corplingr)
#'
#' # 1. Generate concordance of a pattern from multiple corpus files
#' leipzig_corpus_path <- c("/Your/Path/to/Leipzig/corpora_1.txt",
#' "/Your/Path/to/Leipzig/corpora_2.txt")
#'
#' concord <- concord_leipzig(leipzig_path = leipzig_corpus_path, pattern = "menjalani")
#' str(concord)
#'
#'
#' # 2. Combine with pipe "%>%" and other tidyverse suits!
#'
#' concord_leipzig(leipzig_corpus_path, "menjalani") %>%
#'
#' # retain only the concordance, corpus name and sentence id
#' select(-start, -end) %>%
#'
#' write_delim(path = "my_concordance.txt", delim = "\t")
#' }
#' @export



concord_leipzig <- function(leipzig_path = "file path to leipzig corpora", pattern = "regular expressions", case_insensitive = TRUE) {

  # a tibble-dataframe for storing output from all corpus files
  full_concordance <- tibble()

  for (i in seq_along(leipzig_path)) {

    # read in the corpus text
    #if (any(str_detect(leipzig_path, "corpus_sent_vector")) == TRUE) {
     # cat("Using cleaned vector corpus!\n")
      #load(leipzig_path[i])
      #corpora <- sentence_cleaned#; rm(corpus.sent.size, corpus.total.size)
      #corpus_id <- str_replace(str_replace(basename(leipzig_path[i]), "corpus.+?__(?=ind)", ""),
      #                         "\\.RData", "")
      #cat('"', corpus_id, '" ', "has been loaded!\n", sep = "")
    #} else {
      corpora <- read_lines(file = leipzig_path[i])
      cat('"', basename(leipzig_path[i]), '" ', "has been loaded!\n", sep = "")

      # retrieve the corpus names
      corpus_id <- basename(leipzig_path[i])
      corpus_id <- stringr::str_replace(corpus_id, '-sentences.*$', '')
    #}

    for (r in seq_along(pattern)) {
      # progress report
      cat("Searching pattern no. ", r, " in corpus no. ", i, "!\n", sep = "")

      # detect the search pattern and retrieve the citation with the match
      match_id <- stringr::str_which(corpora, stringr::regex(pattern[r], ignore_case = case_insensitive))
      sub_corpus <- corpora[match_id]

      # remove sentence number at the beginning of the line
      sub_corpus <- stringr::str_replace(sub_corpus, "^\\d+?\\s", "")

      # store the sentence number in which the match is found
      sent_id <- match_id

      # detect if any matches found
      if (length(sub_corpus) == 0) {

        cat("NO MATCH(ES) for the pattern you are searching for!\nTRY another corpus!\n\n")
        next

      } else {

        cat("At least one match for the search pattern is detected in the corpus!\n\n")
        match_length <- stringr::str_count(sub_corpus,
                                           stringr::regex(pattern[r],
                                                          ignore_case = case_insensitive)) # get the number of matches of the search word found in the corpus
        sent_with_match <- rep(sub_corpus, match_length) # replicate the sentences/string based on the number of matches found in the string
        sent_id <- rep(sent_id, match_length) # replicate the sentence numbers/IDs based on the number of matches found in the string
        corpus_id <- rep(corpus_id, sum(match_length)) # replicate the corpus file names based on the number of matches found in the string
        position <- stringr::str_locate_all(sub_corpus,
                                            stringr::regex(pattern[r],
                                                           ignore_case = case_insensitive)) # get the starting and end position of the pattern
        sent_quo <- rlang::sym(sprintf("sentences"))
        position_tidy <- position %>% # get all starting and end position into a tidy 'tibble'
          purrr::map(as_tibble) %>% # change the matrix in each position list into a tibble
          purrr::map_df(bind_rows) %>% # return the listed tibble into a single tibble
          dplyr::mutate(!!sent_quo := sent_with_match) # add the matched sentences into the tibble of the starting and end position

        left <- rlang::sym(sprintf("left"))
        right <- rlang::sym(sprintf("right"))
        node_sent_quo <- rlang::sym(sprintf("node_sentences"))
        # generate a concordance table
        concordance <- position_tidy %>%
          mutate(corpus = corpus_id,
                 sent_id = sent_id,
                 !!left := stringr::str_sub(!!sent_quo, start = 1, end = start-1),
                 !!left := stringr::str_trim(!!left),
                 !!left := replace(!!left, nchar(!!left) <= 0, "~"),
                 node = stringr::str_trim(stringr::str_sub(!!sent_quo, start = start, end = end)),
                 !!right := stringr::str_trim(stringr::str_sub(!!sent_quo, start = end+1, end = stringr::str_length(!!sent_quo))),
                 !!right := replace(!!right, nchar(!!right)<=0, "~"),
                 !!node_sent_quo := stringr::`str_sub<-`(!!sent_quo, start = start, end = end, value = "nodeword")) %>%
          select(-!!sent_quo, -!!node_sent_quo)

        full_concordance <- bind_rows(full_concordance, concordance)

        rm(corpora)
      }
    }
  }
  cat("Done!\n")
  if (dim(full_concordance)[1] == 0) {
    message("No match found at all!")
  } else {
    return(full_concordance)
  }
}





# create sorting index based on r1 and l1 only
#l1_sort_index <- concordance %>%
 # select(left) %>%
  #str_extract_all('[^ ]+$') %>%
  #str_extract_all('^.', simplify = T) %>%
  #map_df(~ tibble(left_sort = .))

#r1_sort_index <- concordance %>%
 # select(right) %>%
  #str_extract_all('^.', simplify = T) %>%
  #map_df(~ tibble(right_sort = .))

#concordance <- concordance %>%
 # mutate(l1_sort_index, r1_sort_index)

#' Generate tidyverse-style concordances for the Leipzig Corpora
#'
#' @description The function produces tibble-output concordances for Leipzig Corpora files.
#' @param leipzig_path character stringrs of (i) file names of the Leipzig corpus if they are already in the working directory, or (ii) the complete filepath to each of the Leipzig corpus files to be processed.
#' @param pattern regular expressions/exact patterns for the target pattern.
#' @param case_insensitive whether the search ignores case (TRUE -- the default) or not (FALSE).
#' @return A concordance-tibble consisting of (i) \code{start} and \code{end} character position of the \code{pattern} in the corpus; (ii) \code{corpus} file names and \code{sentence IDs} in which the \code{pattern} is found; (iii) \code{left}, \code{node}, and \code{right} concordance-style view; and (iv) \code{node_sentences} containing the full sentences with the search pattern replaced with \code{"nodeword"} string.
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
#' @importFrom rlang .data
#' @importFrom assertthat assert_that
#' @importFrom assertthat not_empty
#' @examples
#' \dontrun{
#' # load the required packages
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
#' library(dplyr)
#' library(readr)
#' concord_leipzig(leipzig_corpus_path, "menjalani") %>%
#'
#' # retain only the concordance, corpus name and sentence id
#' select(-start, -end) %>%
#'
#' write_delim(path = "my_concordance.txt", delim = "\t")
#' }
#' @export



concord_leipzig <- function(leipzig_path = NULL,
                            pattern = NULL,
                            case_insensitive = TRUE) {

  # check if the path and patterns are not null
  assertthat::assert_that(assertthat::not_empty(leipzig_path))
  assertthat::assert_that(assertthat::not_empty(pattern))

  # a tibble-dataframe for storing output from all corpus files
  full_concordance <- tibble()

  for (i in seq_along(leipzig_path)) {

      corpora <- read_lines(file = leipzig_path[i])
      message(paste('"', basename(leipzig_path[i]), '" ', "has been loaded!\n", sep = ""))

      # retrieve the corpus names
      corpus_id <- basename(leipzig_path[i])
      corpus_id <- stringr::str_replace(corpus_id, '-sentences.*$', '')

    for (r in seq_along(pattern)) {
      # progress report
      message(paste("Searching pattern no. ", r, " in corpus no. ", i, "!\n", sep = ""))

      # detect the search pattern and retrieve the citation with the match
      match_id <- stringr::str_which(corpora, stringr::regex(pattern[r], ignore_case = case_insensitive))
      sub_corpus <- corpora[match_id]

      # remove sentence number at the beginning of the line
      sub_corpus <- stringr::str_replace(sub_corpus, "^\\d+?\\s", "")

      # store the sentence number in which the match is found
      sent_id <- match_id

      # detect if any matches found
      if (length(sub_corpus) == 0) {

        message("NO MATCH(ES) for the pattern you are searching for!\nTRY another corpus!\n\n")
        next

      } else {

        message("At least one match for the search pattern is detected in the corpus!\n\n")
        match_length <- stringr::str_count(sub_corpus,
                                           stringr::regex(pattern[r],
                                                          ignore_case = case_insensitive)) # get the number of matches of the search word found in the corpus
        sent_with_match <- rep(sub_corpus, match_length) # replicate the sentences/string based on the number of matches found in the string
        sent_id <- rep(sent_id, match_length) # replicate the sentence numbers/IDs based on the number of matches found in the string
        corpus_id <- rep(corpus_id, sum(match_length)) # replicate the corpus file names based on the number of matches found in the string
        position <- stringr::str_locate_all(sub_corpus,
                                            stringr::regex(pattern[r],
                                                           ignore_case = case_insensitive)) # get the starting and end position of the pattern

        position_tidy <- position %>% # get all starting and end position into a tidy 'tibble'
          purrr::map(as_tibble) %>% # change the matrix in each position list into a tibble
          purrr::map_df(bind_rows) %>% # return the listed tibble into a single tibble
          dplyr::mutate(sentences = sent_with_match) # add the matched sentences into the tibble of the starting and end position

        # prepare data for generating a concordance table
        concordance <- position_tidy %>%
          mutate(corpus = corpus_id,
                 sent_id = sent_id,
                 left = stringr::str_sub(.data$sentences, start = 1, end = start - 1),
                 left = stringr::str_trim(.data$left),
                 left = replace(.data$left, nchar(.data$left) <= 0, "~"),
                 node = stringr::str_trim(stringr::str_sub(.data$sentences, start = start, end = end)),
                 right = stringr::str_trim(stringr::str_sub(.data$sentences, start = end + 1, end = stringr::str_length(.data$sentences))),
                 right = replace(.data$right, nchar(.data$right) <= 0, "~"),
                 node_sentences = stringr::`str_sub<-`(.data$sentences, start = start, end = end, value = paste("<m>", .data$node, "</m>", sep = ""))) %>%
          select(-.data$sentences)

        full_concordance <- bind_rows(full_concordance, concordance)

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

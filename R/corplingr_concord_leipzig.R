#' Generate tidyverse-style concordances for the Leipzig Corpora
#'
#' @description The function produces tibble-output concordances for Leipzig Corpora files.
#' @param pattern regular expressions/exact patterns for the target pattern.
#' @param corpus_file_names (i) file names of the corpus if they are already in the working directory, or (ii) the complete filepath to each of the Leipzig corpus files to be processed.
#' @param case_insensitive whether the search ignores case (TRUE -- the default) or not (FALSE).
#' @return A concordance-tibble consisting of (i) \code{start} and \code{end} character position of the \code{pattern} in the corpus; (ii) \code{corpus} file names and \code{sentence IDs} in which the \code{pattern} is found; (iii) \code{left}, \code{node}, and \code{right} concordance-style view; (iv) and full \code{sentences} of the matches, with the \code{pattern} being replaced with "nodeword".
#' @importFrom purrr map
#' @importFrom purrr map_df
#' @importFrom purrr pmap
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_which
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_count
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_sub
#' @importFrom stringr str_trim
#' @importFrom stringr str_sub<-
#' @examples
#' \dontrun{
#' # load the required packages
#' library(tidyverse)
#' library(corplingr)
#'
#' # 1. Generate concordance of a pattern from multiple corpus files
#' corpus_files_path <- c("/Your/Path/to/Leipzig/corpora_1.txt",
#' "/Your/Path/to/Leipzig/corpora_2.txt")
#'
#' concord <- concord_leipzig_tidy(pattern = "menjalani", corpus_file_names = corpus_files_path)
#' str(concord)
#'
#'
#' # 2. Combine with pipe "%>%" and other tidyverse suits!
#'
#' concord_leipzig_tidy("menjalani", corpus_files_path) %>%
#'
#' # retain only the concordance, corpus name and sentence id
#' select(-start, -end, -node_sentences) %>%
#'
#' write_delim(path = "my_concordance.txt", delim = "\t")
#' }



concord_leipzig_tidy <- function(pattern = NULL, corpus_file_names = NULL, case_insensitive = TRUE) {

  # a tibble-dataframe for storing output from all corpus files
  full_concordance <- tibble()

  for (i in seq_along(corpus_file_names)) {

    # read in the corpus text
    if (any(str_detect(corpus_file_names, "corpus_sent_vector")) == TRUE) {
      cat("Using cleaned vector corpus!\n")
      load(corpus_file_names[i])
      corpora <- sentence_cleaned; rm(corpus.sent.size, corpus.total.size)
      corpus_id <- str_replace(str_replace(basename(corpus_file_names[i]), "corpus.+?__(?=ind)", ""),
                               "\\.RData", "")
      cat('"', corpus_id, '" ', "has been loaded!\n", sep = "")
    } else {
      corpora <- read_lines(file = corpus_file_names[i])
      cat('"', basename(corpus_file_names[i]), '" ', "has been loaded!\n", sep = "")

      # retrieve the corpus names
      corpus_id <- basename(corpus_file_names[i]) %>%
      str_replace('-sentences.*$', '')
    }

    for (r in seq_along(pattern)) {
      # progress report
      cat("Searching pattern no. ", r, " in corpus no. ", i, "!\n", sep = "")

      # detect the search pattern and retrieve the citation with the match
      match.id <- str_which(corpora, regex(pattern[r], ignore_case = case_insensitive))
      subcorpus <- corpora[match.id]

      # store the sentence number in which the match is found
      sent_id <- match.id

      # detect if any matches found
      if (length(subcorpus) == 0) {

        cat("NO MATCH(ES) for the pattern you are searching for!\nTRY another corpus!\n\n")

      } else {

        cat("At least one match for the search pattern is detected in the corpus!\n\n")

        match_length <- str_count(subcorpus, regex(pattern[r], ignore_case = case_insensitive)) # get the number of matches of the search word found in the corpus

        sent.with.match <- rep(subcorpus, match_length) # replicate the sentences/string based on the number of matches found in the string

        sent_id <- rep(sent_id, match_length) # replicate the sentence numbers/IDs based on the number of matches found in the string

        corpus_id <- rep(corpus_id, sum(match_length)) # replicate the corpus file names based on the number of matches found in the string

        position <- str_locate_all(subcorpus, regex(pattern[r], ignore_case = case_insensitive)) # get the starting and end position of the pattern

        position_tidy <- position %>% # get all starting and end position into a tidy 'tibble'
          map(as_tibble) %>% # change the matrix in each position list into a tibble
          map_df(bind_rows) %>% # return the listed tibble into a single tibble
          mutate(sentences = sent.with.match) # add the matched sentences into the tibble of the starting and end position

        # generate a concordance table
        concordance <- position_tidy %>%

          mutate(corpus = corpus_id,

                 sent_id = sent_id,

                 left = str_sub(sentences, start = 1, end = start-1), # get the left context

                 left = left %>% # remove the sentence id from the left context
                   str_trim(), # remove leading whitespace

                 left = replace(left, nchar(left)<=0, "~"),

                 node = str_sub(sentences, start = start, end = end) %>%  # get the node
                   str_trim(), # remove the leading and trailing whitespace

                 right = str_sub(sentences, start = end+1, end = str_length(sentences)) %>% # get the right context
                   str_trim(), # remove the trailing whitespace

                 right = replace(right, nchar(right)<=0, "~"),

                 node_sentences = stringr::`str_sub<-`(sentences, start = start, end = end, value = "nodeword")) %>%

          select(-sentences) # set the search word into <NODE>

        full_concordance <- bind_rows(full_concordance, concordance)
        rm(corpora)
      }
    }
  }
  return(full_concordance)
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

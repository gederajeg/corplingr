#' Generate word-/regex-specific frequency list from Leipzig Corpora
#'
#' @description The function generates a tibble of word-count for particular words/regexes for each supplied Leipzig corpus file
#'
#' @param pattern the regular expressions/exact patterns for the target pattern/word
#' @param corpus_file_names gives the (i) file names of the corpus if they are in the working directory, or (ii) the complete file path to each of the Leipzig
#' @param case_insensitive whether case differences should be ignored (\code{TRUE} -- the default) or not (\code{FALSE})
#' @return a tibble with three columns (i) \code{match}, (ii) \code{corpus_id}, and (iii) \code{n}, which is the count/token
#' @importFrom tibble tibble
#' @importFrom readr read_lines
#' @importFrom stringr str_subset
#' @importFrom stringr regex
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_extract_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr count
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @examples
#'
#' # prepare the input
#' regex <- "\\bmemberi(kan)?\\b"
#' corpus.path <- corpus_files_path[1:2]
#'
#' # generate the frequency count
#' create_wordlist(pattern = regex, corpus_file_names = corpus.path, case_insensitive = TRUE)
#'
create_wordlist <- function(pattern=NULL, corpus_file_names="corpus-file path", case_insensitive=TRUE) {

  # create an empty tibble to store the results
  regexp <- pattern
  wordlist_table <- tibble()
  for (i in seq_along(corpus_file_names)) {

    corpora <- read_lines(file = corpus_file_names[i])
    cat('"', corpus_file_names[i], '" ', "has been loaded!\n", sep = "")

    for (r in seq_along(regexp)) {
      # progress report
      cat("Searching pattern no. ", r, " in corpus no. ", i, "!\n", sep = "")

      # detect the search pattern
      subcorpus <- str_subset(corpora, regex(pattern = regexp[r], ignore_case = case_insensitive)) %>%
        str_replace_all("^\\d+?\\s", "") # replace sent. number

      # retrieve the corpus names
      corpus_id <- basename(corpus_file_names[i]) %>%
        str_replace('-sentences.*$', '')

      # detect if any matches found
      if (length(subcorpus) == 0) {

        cat("NO MATCH(ES) for the pattern you are searching for!\nTRY another corpus!\n\n")

      } else {

        cat("At least one match for the search pattern is detected in the corpus!\n\n")

        # extract the pattern
        match <- str_extract_all(subcorpus, regex(pattern = regexp[r], ignore_case = case_insensitive)) %>%
          unlist() %>%
          tolower()

        # replicate corpus name as many as the match
        corpus_id <- rep(corpus_id, length(match))

        # store the word and corpus names in tibble
        wordlist_counter <- tibble(match, corpus_id) %>%
          count(match, corpus_id) %>%
          ungroup()

        wordlist_table <- bind_rows(wordlist_table, wordlist_counter)
        #if (i==length(corpus_file_names) & r==length(regex)) {
        #}
      }
    }
  }
  return(wordlist_table)
}

#' Read in the wordlist files of the Leipzig Corpora
#'
#' @description The function generates a tibble of wordlist read from the Leipzig Corpora Word List files
#'
#' @param wlist_path the file paths of the wordlist. Use \code{wordlist_files_path}
#' @param file_pattern the regular expressions/ID labels for the corpus files
#' @param all whether to read all available wordlist files from all corpus (\code{TRUE}) or not (\code{FALSE} -- the default)
#' @param lower_case whether the words are lowercased (\code{TRUE} -- the default) or not (\code{FALSE})
#' @return a tibble with three columns (i) \code{corpus_id} (i.e., the corpus file), (ii) \code{word} (i.e., word forms), and (iii) \code{n} (i.e., token frequency)
#' @importFrom readr read_table
#' @importFrom stringr str_count
#' @importFrom tidyr extract
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom stringr str_to_lower

wordlist_leipzig_read <- function(wlist_path = wordlist_corpus_path, file_pattern = NULL, all = FALSE, lower_case = TRUE) {
  if (all == TRUE) {
    wlist_corpus_id <- basename(wlist_path)
    wlist_df <- tibble()
    for (i in seq_along(wlist_path)) {
      temp_wlist <- read_table(file = wlist_path[i], col_names = FALSE)
      if (str_count(temp_wlist$X1[1], "\\t") == 3) {
        temp_wlist <- temp_wlist %>%
          extract(col = X1,
                  into = c("w_id", "word", "w2", "n"),
                  regex = "^(.+?)\\t(.+?)\\t(.+?)\\t(.+?)$") %>%
          mutate(n = as.numeric(n)) %>%
          select(-w2) %>%
          mutate(corpus_id = wlist_corpus_id[i])
      } else if (str_count(temp_wlist$X1[1], "\\t") == 2) {
        temp_wlist <- temp_wlist %>%
          extract(col = X1,
                  into = c("w_id", "word", "n"),
                  regex = "^(.+?)\\t(.+?)\\t(.+?)$") %>%
          mutate(n = as.numeric(n)) %>%
          mutate(corpus_id = wlist_corpus_id[i])
      }
      if (lower_case == TRUE) {
        temp_wlist <- temp_wlist %>%
          mutate(word = str_to_lower(word)) %>%
          group_by(word, corpus_id) %>%
          summarise(n=sum(n)) %>%
          ungroup() %>%
          select(corpus_id, word, n)
      } else {
        cat("\nNot lower-casing the wordlist!\n")
      }
      wlist_df <- bind_rows(wlist_df, temp_wlist)
    }
  } else {
    if (is.null(file_pattern)) {
      cat("\nSpecify the name/pattern for the corpus id!\n")
    } else {
      wlist_files <- str_subset(wlist_path, file_pattern)
      wlist_corpus_id <- str_subset(basename(wlist_path), file_pattern)
      wlist_df <- tibble()
      for (i in seq_along(wlist_files)) {
        temp_wlist <- read_table(file = wlist_files[i], col_names = FALSE)
        if (str_count(temp_wlist$X1[1], "\\t") == 3) {
          temp_wlist <- temp_wlist %>%
            extract(col = X1,
                    into = c("w_id", "word", "w2", "n"),
                    regex = "^(.+?)\\t(.+?)\\t(.+?)\\t(.+?)$") %>%
            mutate(n = as.numeric(n)) %>%
            select(-w2) %>%
            mutate(corpus_id = wlist_corpus_id[i])
        } else if (str_count(temp_wlist$X1[1], "\\t") == 2) {
          temp_wlist <- temp_wlist %>%
            extract(col = X1,
                    into = c("w_id", "word", "n"),
                    regex = "^(.+?)\\t(.+?)\\t(.+?)$") %>%
            mutate(n = as.numeric(n)) %>%
            mutate(corpus_id = wlist_corpus_id[i])
        }
        if (lower_case == TRUE) {
          temp_wlist <- temp_wlist %>%
            mutate(word = str_to_lower(word)) %>%
            group_by(word, corpus_id) %>%
            summarise(n=sum(n)) %>%
            ungroup() %>%
            select(corpus_id, word, n)
        } else {
          cat("\nNot lower-casing the wordlist!\n")
        }
        wlist_df <- bind_rows(wlist_df, temp_wlist)
      }
    }
  }
  return(wlist_df)
}

#' Summarise wordlist count across corpus
#' @description function to summarise the wordlist by removing the \code{corpus} variable derived via \code{\link{create_wordlist()}}.
#' @param df a tibble data frame containing wordlist derived via the \code{\link{create_wordlist()}}.
#' @param descending whether the list is ordered increasingly and vice versa according to the token frequency
#' @importFrom dplyr enquo
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @return a tibble
wordlist_summarise <- function(df, group_var = "variable to group_by", descending = TRUE) {
  group_var <- enquo(group_var)
  out <- df %>%
    group_by(!!group_var) %>%
    summarise(n = sum(n))
  if (descending == TRUE) {
    return(arrange(out, desc(n)))
  } else if (descending == FALSE) {
    return(out)
  }
}



# function to check the corpus file in which a word is found
# PARAMETER
# @df = a tibble data frame containing wordlist derived via the 'create_wordlist()' function
# @regex = regular expressions/strings for the searched words/expressions
check_wordlist_corpus <- function(df=NULL, regex=NULL) {
  out1 <- df %>%
    filter(str_detect(match, regex))
  out_corpus <- out1 %>%
    .$corpus_id %>%
    str_c(collapse="|")
  outall <- list(df=out1, corpus=out_corpus)
  return(outall)
}


#' Check full sentence-citation for a pattern

#' @description function to get a sentence citation for a match. This function can be used to check the full sentence in which a word/pattern occur in a corpus. This can be a useful follow-up upon inspecting the wordlist results derived via \code{\link{create_wordlist()}} function.
#' @param pattern regular expressions/strings of the target words/patterns
#' @param corpus_file_names complete file path of the Leipzig corpora to search for full citation of the words/patterns of interest.
#' @param case_insensitive whether to search case-insensitive pattern (TRUE) or not (FALSE).
#' @return character vectors containing the info for 'corpus names', 'sentence number', and 'sentence match'.

get_citation_match <- function(pattern = NULL, corpus_file_names = corpus_files_path, case_insensitive = TRUE) {
  sent <- vector()
  out <- concord_leipzig_tidy(pattern = pattern, corpus_file_names, case_insensitive = case_insensitive)
  cat("Collecting full-citation(s) for the search word(s) done!\nOutput format: CORPUS - SENTENCE_NUMBER - CITATION(S)\n-------------------------------------------------------\n")
  collector <- str_c(out$corpus, " | ", out$sent_id, " | ", out$node_sentences, sep="")
  collector <- str_replace_all(collector, 'nodeword', str_c("<node>", toupper(out$node), "</node>", sep = ""))
  sent <- c(sent, collector)
}


#' Generate Leipzig corpus-size
#
#' @description function to get a total word-token count of a given leipzig corpus file
#' @param word_regex regular expressions defining what "a word" is
#' @param corpus_file_names file path to the directory folder in which the Leipzig corpus files are stored
#' @return tibble containing \code{corpus_id}, \code{size}, and \code{size_print} (for text-printing)

leipzig_corpus_size <- function(word_regex = "\\b(?i)([-a-z0-9]+)\\b", corpus_file_names = corpus_files_path) {

  corpus_id <- str_replace_all(basename(corpus_file_names), "-sentences.txt", "")
  corpus_path <- corpus_file_names

  total_word <- vector()

  for (i in seq_along(corpus_path)) {

    # read in the corpus text
    corpora <- read_lines(file = corpus_path[i])
    cat('"', corpus_id[i], '" ', "has been loaded!\n", sep = "")

    # total word count
    corp_count <- corpora %>%
      str_replace("^\\d+?\\s", "") %>%
      str_count(word_regex) %>%
      sum()
    rm(corpora)
    cat('Done counting total words for "', corpus_id[i], '"... (', format(corp_count, big.mark = ","), ' word-tokens)\n\n', sep = "")
    total_word <- c(total_word, corp_count)
  }
  out <- tibble(corpus_id, size=total_word, size_print=format(total_word, big.mark = ","))
  cat("Total size of the corpus files (in word-tokens).\n")
  return(out)
}

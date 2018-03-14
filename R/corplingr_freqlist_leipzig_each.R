#' Generate word-/regex-specific frequency list from the Leipzig Corpora
#'
#' @description The function generates a tibble of token-count for a particular word(s)/regex(es) for each supplied Leipzig corpus file.
#'
#' @param pattern the regular expressions/exact patterns for the target pattern/word whose frequency in a (set of) Leipzig Corpus file(s) you want to generate.
#' @param leipzig_path gives the (i) file names of the corpus if they are in the working directory, or (ii) the complete file path to each of the Leipzig.
#' @param case_insensitive logical; whether case differences should be ignored (\code{TRUE} -- the default) or not (\code{FALSE}).
#' @return a tibble with three columns (i) \code{match}, (ii) \code{corpus_id}, and (iii) \code{n}, which is the count/token.
#' @importFrom tibble tibble
#' @importFrom readr read_lines
#' @importFrom stringr str_subset
#' @importFrom stringr regex
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_replace
#' @importFrom stringr str_extract_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr count
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{
#' # prepare the input
#' regex <- "\\bmemberi(kan)?\\b"
#' corpus.path <- leipzig_file_path[1:2]
#'
#' # generate the frequency count
#' freqlist_leipzig_each(pattern = regex,
#'                 leipzig_path = corpus.path,
#'                 case_insensitive = TRUE)
#' }
#' @export

freqlist_leipzig_each <- function(pattern = NULL, leipzig_path = "(full) filepath to a (set of) Leipzig corpus files", case_insensitive = TRUE) {

  # create an empty tibble to store the results
  regexp <- pattern
  wordlist_table <- tibble::tibble()
  for (i in seq_along(leipzig_path)) {

    corpora <- readr::read_lines(file = leipzig_path[i])
    cat('"', leipzig_path[i], '" ', "has been loaded!\n", sep = "")

    for (r in seq_along(regexp)) {
      # progress report
      cat("Searching pattern no. ", r, " in corpus no. ", i, "!\n", sep = "")

      # detect the search pattern
      sub_corpus <- stringr::str_subset(corpora,
                                        stringr::regex(pattern = regexp[r],
                                                       ignore_case = case_insensitive))

      # retrieve the corpus names
      corpus_id <- basename(leipzig_path[i])
      corpus_id <- stringr::str_replace(corpus_id, "-sentences.*$", "")

      # detect if any matches found
      if (length(sub_corpus) == 0) {

        cat("NO MATCH(ES) for the pattern you are searching for!\nTRY another corpus!\n\n")

      } else {

        cat("At least one match for the search pattern is detected in the corpus!\n\n")

        # extract the pattern
        match <- stringr::str_extract_all(sub_corpus,
                                          stringr::regex(pattern = regexp[r],
                                                         ignore_case = case_insensitive))
        match <- unlist(match)
        match <- stringr::str_to_lower(match)

        # replicate corpus name as many as the match
        corpus_id <- rep(corpus_id, length(match))

        # store the word and corpus names in tibble
        wordlist_counter <- tibble::tibble(match, corpus_id)
        wordlist_counter <- dplyr::count(wordlist_counter, match, corpus_id, sort = TRUE)
        wordlist_counter <- dplyr::ungroup(wordlist_counter)

        wordlist_table <- dplyr::bind_rows(wordlist_table, wordlist_counter)
      }
    }
  }
  return(wordlist_table)
}

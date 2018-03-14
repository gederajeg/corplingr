#' Generate Leipzig corpus-size
#
#' @description function to get a total word-token count of a given leipzig corpus file.
#'     It is built on top of \code{\link[stringr]{str_count}}.
#' @param leipzig_path file path to the directory folder in which the Leipzig corpus files are stored
#' @param word_regex regular expressions defining what "a word" is
#' @return tibble containing \code{corpus_id}, \code{size}, and \code{size_print} (for text-printing)

corpus_size_leipzig <- function(leipzig_path = "(full) filepath to Leipzig corpus files", word_regex = "\\b(?i)([-a-zA-Z0-9]+)\\b") {

  corpus_id <- stringr::str_replace_all(basename(leipzig_path), "-sentences.txt", "")
  corpus_path <- leipzig_path

  size <- vector()

  for (i in seq_along(corpus_path)) {

    # read in the corpus text
    corpora <- readr::read_lines(file = corpus_path[i])
    cat('"', corpus_id[i], '" ', "has been loaded!\n", sep = "")

    # total word count
    corp_count <- corpora %>%
      stringr::str_replace("^\\d+?\\s", "") %>%
      stringr::str_count(stringr::regex(pattern = word_regex)) %>%
      sum()
    rm(corpora)
    cat('Done counting total words for "', corpus_id[i], '"... (', format(corp_count, big.mark = ","), ' word-tokens)\n\n', sep = "")
    size <- c(size, corp_count)
  }
  size_print <- format(size, big.mark = ",")
  out <- tibble::tibble(corpus_id, size, size_print)
  cat("Total size of the corpus files (in word-tokens).\n")
  return(out)
}

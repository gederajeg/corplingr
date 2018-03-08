#' Read in the wordlist files of the Leipzig Corpora
#'
#' @description The function generates a tibble of frequency list of words read from the Leipzig Corpora frequency list files.
#'     When one downloads the zip file of a Leipzig corpus, the readily available frequency lists will have the "-words.txt" label.
#'     For instance, for Indonesian, the filename looks like this: \code{"ind_newscrawl_2015_300K-words.txt"}.
#'
#' @param wlist_path the file paths of the Leipzig wordlist files.
#' @param file_pattern the regular expressions for the wordlist files if particular files are to be read. The default is \code{NULL}.
#'     When all available frequency list files want to be be read-in, leave this argument to \code{NULL} and set the \code{all} argument (cf. below) to \code{TRUE}.
#' @param all whether to read all available wordlist files from all corpus (\code{TRUE}) or not (\code{FALSE} -- the default)
#' @param lower_case whether the words are lowercased (\code{TRUE} -- the default) or not (\code{FALSE})
#' @return a tibble with three columns (i) \code{corpus_id} (i.e., the corpus file), (ii) \code{word} (i.e., word forms), and (iii) \code{n} (i.e., token frequency)
#' @importFrom readr read_table
#' @importFrom stringr str_count
#' @importFrom tidyr extract
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom stringr str_to_lower
#' @importFrom rlang :=
#' @importFrom rlang sym
#' @importFrom stats end
#' @importFrom stats start
#' @export
#' @examples
#' \dontrun{
#' # read in a file containing filepaths of all Leipzig Corpora frequency list files
#' wordlist_path <- readr::read_lines(file = "~/Documents/Corpora/leipzig_wordlist_path.txt")
#'
#' # read in the web and wikipedia sections
#' tb <- freqlist_leipzig_read(wordlist_corpus_path,
#'                             file_pattern = "(web_2012|wikipedia_2016)",
#'                             all = FALSE,
#'                             lower_case = TRUE)
#'
#'  # subset only on word that consists of letters and hypen (to retrieve a reduplication, for instance)
#'  dplyr::filter(tb, stringr::str_detect(word, "^([-a-z]+)$"))
#' }


freqlist_leipzig_read <- function(wlist_path = wordlist_corpus_path, file_pattern = NULL, all = FALSE, lower_case = TRUE) {
  corpus_id_q <- rlang::sym(sprintf("corpus_id"))
  n_q <- rlang::sym(sprintf("n"))
  word_q <- rlang::sym(sprintf("word"))
  x1_q <- rlang::sym(sprintf("X1"))
  w2_q <- rlang::sym(sprintf("w2"))
  if (all == TRUE) {
    wlist_corpus_id <- basename(wlist_path)
    wlist_df <- tibble::tibble()
    for (i in seq_along(wlist_path)) {
      temp_wlist <- readr::read_table(file = wlist_path[i], col_names = FALSE)
      if (stringr::str_count(temp_wlist$X1[1], "\\t") == 3) {
        temp_wlist <- temp_wlist %>%
          tidyr::extract(col = !!x1_q,
                         into = c("w_id", "word", "w2", "n"),
                         regex = "^(.+?)\\t(.+?)\\t(.+?)\\t(.+?)$") %>%
          dplyr::mutate(!!n_q := as.numeric(!!n_q)) %>%
          dplyr::select(-!!w2_q) %>%
          dplyr::mutate(!!corpus_id_q := wlist_corpus_id[i])
      } else if (stringr::str_count(temp_wlist$X1[1], "\\t") == 2) {
        temp_wlist <- temp_wlist %>%
          tidyr::extract(col = !!x1_q,
                         into = c("w_id", "word", "n"),
                         regex = "^(.+?)\\t(.+?)\\t(.+?)$") %>%
          mutate(!!n_q := as.numeric(!!n_q)) %>%
          mutate(!!corpus_id_q := wlist_corpus_id[i])
      }
      if (lower_case == TRUE) {
        temp_wlist <- temp_wlist %>%
          dplyr::mutate(!!word_q := stringr::str_to_lower(!!word_q)) %>%
          dplyr::group_by(!!word_q, !!corpus_id_q) %>%
          dplyr::summarise(!!n_q := sum(!!n_q)) %>%
          dplyr::ungroup() %>%
          dplyr::select(!!corpus_id_q, !!word_q, !!n_q)
      } else {
        cat("\nNot lower-casing the wordlist!\n")
      }
      wlist_df <- dplyr::bind_rows(wlist_df, temp_wlist)
    }
  } else {
    if (is.null(file_pattern)) {
      cat("\nSpecify the name/pattern for the corpus id!\n")
    } else {
      wlist_files <- stringr::str_subset(wlist_path, file_pattern)
      wlist_corpus_id <- stringr::str_subset(basename(wlist_path), file_pattern)
      wlist_df <- tibble::tibble()
      for (i in seq_along(wlist_files)) {
        temp_wlist <- readr::read_table(file = wlist_files[i], col_names = FALSE)
        if (stringr::str_count(temp_wlist$X1[1], "\\t") == 3) {
          temp_wlist <- temp_wlist %>%
            tidyr::extract(col = !!x1_q,
                           into = c("w_id", "word", "w2", "n"),
                           regex = "^(.+?)\\t(.+?)\\t(.+?)\\t(.+?)$") %>%
            dplyr::mutate(!!n_q := as.numeric(!!n_q)) %>%
            dplyr::select(-!!w2_q) %>%
            dplyr::mutate(!!corpus_id_q := wlist_corpus_id[i])
        } else if (stringr::str_count(temp_wlist$X1[1], "\\t") == 2) {
          temp_wlist <- temp_wlist %>%
            tidyr::extract(col = !!x1_q,
                           into = c("w_id", "word", "n"),
                           regex = "^(.+?)\\t(.+?)\\t(.+?)$") %>%
            dplyr::mutate(!!n_q := as.numeric(!!n_q)) %>%
            dplyr::mutate(!!corpus_id_q := wlist_corpus_id[i])
        }
        if (lower_case == TRUE) {
          temp_wlist <- temp_wlist %>%
            dplyr::mutate(!!word_q := stringr::str_to_lower(!!word_q)) %>%
            dplyr::group_by(!!word_q, !!corpus_id_q) %>%
            dplyr::summarise(!!n_q := sum(!!n_q)) %>%
            dplyr::ungroup() %>%
            dplyr::select(!!corpus_id_q, !!word_q, !!n_q)
        } else {
          cat("\nNot lower-casing the wordlist!\n")
        }
        wlist_df <- dplyr::bind_rows(wlist_df, temp_wlist)
      }
    }
  }
  return(wlist_df)
}

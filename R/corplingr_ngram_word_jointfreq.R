#' Combine the ngram output with the frequency list of the elements of the ngram
#'
#' @description Left join the frequency list for each element making up the ngram
#' @param df_ngram ngram table output by \code{\link{ngram_word_leipzig}}.
#' @param df_wlist wordlist table output by \code{\link{ngram_word_leipzig}}.
#' @param ... capture the word elements of the ngram (e.g., \code{w1, w2, w3, etc.}).
#' @return a joint tibble for the ngram and frequency list for each component word of the ngram
#' @importFrom dplyr funs
#' @importFrom dplyr quo
#' @importFrom dplyr quos
#' @importFrom dplyr quo_name
#' @importFrom dplyr left_join
#' @importFrom dplyr rename_if
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' # load the corplingr package
#' devtools::load_all("~/Documents/_my_r_packages/corplingr")
#'
#' # load the .RData for the cleaned sentence corpora
#' # 'cleaned' -> all tokens have been separated by whitespace
#' load(leipzig_cleaned_path[1])
#'
#' # if it is a sentence-per-line input-vector, mark the element with "SENT"
#' sentence_cleaned <- paste("SENT", sentence_cleaned, "SENT", sep = " ")
#'
#' # generate the ngram
#' ngram_out <- ngram_word_leipzig(sentence_cleaned, sample_n = 10000)
#' ngram <- ngram_out$ngram
#' wlist <- ngram_out$wlist
#'
#' # joint the frequency list
#' df <- ngram_word_jointfreq(ngram, wlist, w1, w2)
#' df
#' }

ngram_word_jointfreq <- function(df_ngram, df_wlist, ...) {

  quo_by <- dplyr::quos(...)
  for (i in seq_along(quo_by)) {
    quo_by_name <- dplyr::quo_name(quo_by[[i]])
    df_wlist <- dplyr::rename_if(df_wlist, is.character, funs(function(.) !!quo_by_name))
    df_wlist <- dplyr::rename_if(df_wlist, is.numeric, funs(function(.) paste("n_", !!quo_by_name, sep = "")))
    df_ngram <- df_ngram %>% dplyr::left_join(df_wlist, by = quo_by_name)
  }
  return(df_ngram)
}

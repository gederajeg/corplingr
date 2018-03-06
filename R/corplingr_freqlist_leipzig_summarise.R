#' Summarise wordlist count across corpus
#'
#' @description function to summarise the wordlist by removing the \code{corpus} variable derived via \code{\link{freqlist_leipzig_each}} or \code{\link{freqlist_leipzig_all}}.
#' @param df a tibble data frame containing wordlist derived via the \code{\link{freqlist_leipzig_each}} or \code{\link{freqlist_leipzig_all}}.
#' @param group_var a bare (i.e. unquoted) variable name to group by.
#' @param descending whether the list is ordered increasingly and vice versa according to the token frequency.
#' @importFrom dplyr enquo
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom rlang sym
#' @return a tibble
#' @export
#' @examples
#' \dontrun{
#' regex <- "\\bmemberi(kan)?\\b"
#' corpus.path <- corpus_files_path[1:2]
#'
#' # Generate the freqlist of the pattern
#' wlist <- freqlist_create(pattern = regex,
#'                          corpus_file_names = corpus.path,
#'                          case_insensitive = TRUE)
#'
#' wlist
#' A tibble: 4 x 3
#' match      corpus_id              n
#' <chr>      <chr>              <int>
#'  1 memberi    ind_mixed_2012_1M   6394
#'  2 memberikan ind_mixed_2012_1M  11710
#'  3 memberi    ind_news_2008_300K  2214
#'  4 memberikan ind_news_2008_300K  5213
#'
#' # Summarise the match
#' freqlist_summarise(df = wlist, group_var = match, descending = TRUE)
#' }

freqlist_leipzig_summarise <- function(df, group_var = "variable to group by", descending = TRUE) {
  group.var <- enquo(group_var)
  n_q <- rlang::sym(sprintf("n"))
  df <- dplyr::group_by(df, !!group.var)
  df <- dplyr::summarise(df, !!n_q := sum(!!n_q))
  if (descending == TRUE) {
    return(dplyr::arrange(df, dplyr::desc(!!n_q)))
  } else if (descending == FALSE) {
    return(df)
  }
}

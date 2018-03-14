#' Prepare data for collexeme/collocates analysis
#'
#' @description This function is designed to handle the output of \code{colloc_default} to generate a tidy data required as input of \code{collex_fye} for performing collexeme/collocate analysis.
#' @param list_output The list output of \code{colloc_default}.
#' @param span character; the context-window span user wants to focus on for the collexeme/collocates analysis.
#'     For instance \code{"r1"}, \code{"l1"}, or a set of span windows as \code{c("r1", "l1")}.
#'     The default is \code{NULL}, indicating that all span defined from running \code{colloc_default} will be considered.
#'
#' @return A tbl_df (tibble data frame)
#' @export
#'
#' @examples
#' \dontrun{
#' # do the collocate search
#' df <- colloc_default(corpus_path = orti_bali_path,
#'                      pattern = "^nuju$",
#'                      window = "b", # focusing on both left and right context window
#'                      span = 3) # retrieve 3 collocates to the left and right of the node
#'
#' # prepare the collexeme analysis input tibble
#' # and select to focus on R1 and R2 collocates.
#' collex_tb <- collex_prepare(df, span = c("r1", "r2"))
#' }
#'
#' @importFrom dplyr filter
#' @importFrom dplyr quo
#' @importFrom dplyr quo_name
#' @importFrom dplyr count
#' @importFrom dplyr rename
#' @importFrom dplyr pull
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom stringr regex
#' @importFrom stringr str_detect
#' @importFrom purrr is_null
collex_prepare <- function(list_output = "list output of 'colloc_default()'", span = NULL) {
  pattern_rgx <- list_output[["pattern_regex"]]
  all_colloc <- list_output[["colloc_tb"]]
  all_word_vectors <- list_output[["wordlist_tb"]]

  a_q <- dplyr::quo(a)
  n_q <- dplyr::quo(n)
  n_pattern_q <- dplyr::quo(n_pattern)
  n_w_in_corp_q <- dplyr::quo(n_w_in_corp)
  w_q <- dplyr::quo(w)
  corpus_size_q <- dplyr::quo(corpus_size)

  if (!purrr::is_null(span)) {
    all_colloc <- all_colloc[all_colloc$w_span %in% span, ]
  }

  # preparing another output tibble for conducting collocates/collexemes analysis
  # it takes all collocates within the defined span as indicated in the function
  # 1. collocates frequency
  colloc_freq <- dplyr::count(all_colloc, !!w_q, sort = TRUE)
  colloc_freq <- dplyr::rename(colloc_freq, !!dplyr::quo_name(a_q) := !!n_q)

  # 2. all words in the corpus
  word_freq <- dplyr::count(all_word_vectors, !!w_q, sort = TRUE)
  word_freq <- dplyr::rename(word_freq, !!dplyr::quo_name(n_w_in_corp_q) := !!n_q)

  # 3. corpus size in total of word tokens
  corpus_size <- dim(all_word_vectors)[1]

  # 4. node word/construction total tokens in the corpus
  pattern_size <- dplyr::filter(word_freq, stringr::str_detect(!!w_q, pattern_rgx))
  pattern_size <- dplyr::pull(pattern_size, !!n_w_in_corp_q)
  pattern_size <- sum(pattern_size)

  # generate the tibble for the required input for collostructionr::coll.collex()
  collex_tb <- dplyr::left_join(colloc_freq, word_freq, by = "w")
  collex_tb <- dplyr::mutate(collex_tb,
                             !!dplyr::quo_name(corpus_size_q) := corpus_size,
                             !!dplyr::quo_name(n_pattern_q) := pattern_size)
  return(collex_tb)

}


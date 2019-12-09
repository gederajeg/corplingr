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
#' df <- colloc_default(corpus_path = orti_bali_path[1:50],
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
#' @importFrom rlang .data
collex_prepare <- function(list_output = "list output of 'colloc_default()'", span = NULL) {
  pattern_rgx <- list_output[["pattern_regex"]]
  all_colloc <- list_output[["colloc_tb"]]
  all_word_vectors <- list_output[["wordlist_tb"]]

  if (!purrr::is_null(span)) {
    all_colloc <- all_colloc[all_colloc$span %in% span, ]
  }

  # preparing another output tibble for conducting collocates/collexemes analysis
  # it takes all collocates within the defined span as indicated in the function
  # 1. collocates frequency
  colloc_freq <- dplyr::count(all_colloc, .data$w, sort = TRUE)
  colloc_freq <- dplyr::rename(colloc_freq, a = .data$n)

  # 2. all words in the corpus
  word_freq <- dplyr::count(all_word_vectors, .data$w, sort = TRUE)
  word_freq <- dplyr::rename(word_freq, n_w_in_corp = .data$n)

  # 3. corpus size in total of word tokens
  corpus_size <- dim(all_word_vectors)[1]

  # 4. node word/construction total tokens in the corpus
  pattern_size <- dplyr::filter(word_freq, stringr::str_detect(.data$w, pattern_rgx))
  pattern_size <- dplyr::pull(pattern_size, .data$n_w_in_corp)
  pattern_size <- sum(pattern_size)

  # generate the tibble for the required input for collostructionr::coll.collex()
  collex_tb <- dplyr::left_join(colloc_freq, word_freq, by = "w")
  collex_tb <- dplyr::mutate(collex_tb,
                             corpus_size = corpus_size,
                             n_pattern = pattern_size)
  return(collex_tb)

}


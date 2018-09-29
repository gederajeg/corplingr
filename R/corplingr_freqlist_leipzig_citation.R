#' Check full sentence-citation for a pattern

#' @description A function to retrieve a full-sentence citation for a match/pattern. This function can be used to check the full sentence in which a word/pattern occur in a corpus.
#'     This can be a useful follow-up upon inspecting the wordlist results derived via \code{\link{freqlist_leipzig_each}} function.
#' @param pattern regular expressions/strings of the target words/patterns.
#' @param leipzig_path complete file path of the Leipzig corpora to search for full citation of the words/patterns of interest.
#' @param case_insensitive whether to search case-insensitive pattern (\code{TRUE} -- the default) or not (\code{FALSE}).
#' @return A tibble containing the info for 'corpus names', 'sentence number', 'node word', and 'sentence match'.
#' @importFrom stringr str_c
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_upper
#' @importFrom dplyr bind_rows
#' @importFrom dplyr quo_name
#' @importFrom dplyr quo
#' @importFrom tibble tibble
#' @importFrom rlang :=
#' @export

freqlist_leipzig_citation <- function(pattern = NULL, leipzig_path = "(full) filepath to Leipzig corpus files", case_insensitive = TRUE) {
  sent <- tibble::tibble()
  corpus <- dplyr::quo(corpus)
  sent_id <- dplyr::quo(sent_id)
  node <- dplyr::quo(node)
  node_sentences <- dplyr::quo(node_sentences)

  out <- concord_leipzig(pattern = pattern, leipzig_path, case_insensitive = case_insensitive)

  if (any(dim(out)[1] > 0)) {

    collector <- dplyr::select(out,
                               !!corpus,
                               !!sent_id,
                               !!node,
                               !!node_sentences)
    collector <- dplyr::mutate(collector,
                               !!dplyr::quo_name(node_sentences) := stringr::str_replace(!!node_sentences,
                                                                                         "nodeword",
                                                                                         str_c("<node>",
                                                                                               !!node,
                                                                                               "</node>",
                                                                                               sep = "")
                                                                                         )
                               )
    sent <- dplyr::bind_rows(sent, collector)
    return(sent)
  }
}

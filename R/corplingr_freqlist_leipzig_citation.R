#' Check full sentence-citation for a pattern

#' @description A function to retrieve a full-sentence citation for a match/pattern. This function can be used to check the full sentence in which a word/pattern occur in a corpus.
#'     This can be a useful follow-up upon inspecting the wordlist results derived via \code{\link{freqlist_leipzig_each}} function.
#' @param pattern regular expressions/strings of the target words/patterns.
#' @param leipzig_path complete file path of the Leipzig corpora to search for full citation of the words/patterns of interest.
#' @param case_insensitive whether to search case-insensitive pattern (\code{TRUE} -- the default) or not (\code{FALSE}).
#' @return character vectors containing the info for 'corpus names', 'sentence number', and 'sentence match'.
#' @importFrom stringr str_c
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_upper
#' @export

freqlist_leipzig_citation <- function(pattern = NULL, leipzig_path = "(full) filepath to Leipzig corpus files", case_insensitive = TRUE) {
  sent <- vector()
  out <- concord_leipzig(pattern = pattern, leipzig_path, case_insensitive = case_insensitive)
  cat("Collecting full-citation(s) for the search word(s) done!\nOutput format: CORPUS - SENTENCE_NUMBER - CITATION(S)\n-------------------------------------------------------\n")
  collector <- stringr::str_c(out$corpus, " | ", out$sent_id, " | ", out$node_sentences, sep="")
  collector <- stringr::str_replace_all(collector, 'nodeword', stringr::str_c("<node>", stringr::str_to_upper(out$node), "</node>", sep = ""))
  sent <- c(sent, collector)
  return(sent)
}

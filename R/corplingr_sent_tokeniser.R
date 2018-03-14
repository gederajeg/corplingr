#' Split a corpus by sentence-boundary
#'
#' @description The embedded function in the collocational framework to split input corpus into vector of sentences using \code{\link[stringi]{stri_split_boundaries}} from \code{stringi} package.
#'     Each sentence line will be appended, at the beginning and at the end, with \code{"ZSENTENCEZ"} marker as many as the number of collocational window-span is required.
#'     This marker will help identify if collocates of a word cross the boundary of the sentence in which the word occurs.
#'     The function automatically detects and removes if \code{"ZSENTENCEZ"} is part of the identified collocate.
#' @param strings character vector of a corpus text.
#' @param to_lower logical; turn the corpus into lowercase when \code{TRUE} (the default).
#' @param window_span integer; it is supplied from the value of the \code{span} argument in the higher-level collocational function call.
#'     It will determine the number of times the \code{"ZSENTENCEZ"} marker will be appended at the beginning and at the end of each sentence.
#' @return A character vector with as many sentences as there are in the input corpus as identified by \code{\link[stringi]{stri_split_boundaries}}.
#' @examples
#' txt <- c("It is one sentence. It is another sentence! There are TWO sentences.",
#'          "The second sentence and the second element of 'txt' corpus.",
#'          "Can we add another one (sentence) here as the third element?",
#'          "Of course!")
#' sent <- tokenise_sentence(strings = txt,
#'                           to_lower = TRUE,
#'                           window_span = 3)
#' sent
#' @export
#' @importFrom stringi stri_split_boundaries
tokenise_sentence <- function(strings = NULL, to_lower = TRUE, window_span = NULL) {
  sent <- unlist(stringi::stri_split_boundaries(str = strings, type = "sentence"))
  sent <- stringr::str_trim(string = sent[nzchar(sent)])
  if (to_lower == TRUE) {
    sent <- stringr::str_to_lower(sent)
  }
  sent_marker <- stringr::str_c(rep("ZSENTENCEZ", window_span), collapse = " ")
  sent <- stringr::str_c(sent_marker, sent, sent_marker, sep = " ")
  return(sent)
}

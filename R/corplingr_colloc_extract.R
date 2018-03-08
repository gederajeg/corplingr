#' Generate tidyverse-style sequential, ngram-like collocates
#'
#' @description The function produces window-span collocates in the following scheme: ...\\scolloc\\scolloc\\sNODE\\scolloc\\scolloc\\s... in which \code{"\\\s"} stands for 'white space'.
#' @param corpus_vector a raw corpus vector loaded via \code{scan} or \code{\link[readr]{read_lines}}.
#' @param pattern regular expressions/strings for node-word to search in the corpus.
#' @param window window-span direction of the collocates: \code{"r"} ('right of the node'), \code{"l"} ('left of the node'), or the DEFAULT is \code{"b"} ('both window').
#' @param span numeric vector indicating the span of the collocate scope.
#' @param gathered whether the output table should be in tidy format (\code{TRUE}) or not (\code{FALSE} -- default). This makes use of \code{tidyr}'s \code{\link[tidyr]{gather}}.
#' @param case_insensitive whether the search ignore case (\code{TRUE} -- default) or not (\code{FALSE})
#' @return A tbl_df of collocates consisting of the \code{patterns}, \code{node} words, and the collocates as values of each of the specified \code{span} as individual columns (when \code{gathered} is \code{FALSE}).
#'     When \code{gathered} is set to \code{TRUE}, the results consist of four columns: (i) \code{patterns}, (ii) \code{node}, (iii) \code{collocates}, and (iv) \code{span}.
#' @importFrom stringr str_c
#' @importFrom stringr regex
#' @importFrom stringr str_to_lower
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom tidyr extract
#' @importFrom tidyr spread
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # load a Leipzig corpus file
#' corpus <- readr::read_lines(corpus_files_path[2])
#'
#' # search right-side collocates within two words from the node
#' colloc <- colloc_extract(corpus_vector = corpus, pattern = "\\bmemberi\\b", window = "r", span = 2)
#'
#' # explore the output
#' colloc %>%
#' filter(!r2 %in% c("kepada", "bagi", "untuk", "terhadap", "ke"))
#' }
#' @export


colloc_extract <- function(corpus_vector = "corpus vector",
                           pattern = "regex",
                           window = "b",
                           span = "numeric",
                           gathered = FALSE,
                           case_insensitive = TRUE) {
  node_regex <- stringr::regex(pattern, ignore_case = case_insensitive)
  line_match <- stringr::str_subset(corpus_vector, node_regex)
  if (window == "l") {
    #span_regex <- stringr::str_c(seq(span), collapse = ", ")
    colloc_regex <- stringr::str_c("\\b([a-zA-Z-]+\\s){", span, "}", sep = "")
    patterns_regex <- stringr::str_c(colloc_regex, node_regex, sep = "")
  } else if (window == "r") {
    #span_regex <- stringr::str_c(seq(span), collapse = ", ")
    colloc_regex <- stringr::str_c("(\\s([a-zA-Z-]+)){", span,"}\\b", sep = "")
    patterns_regex <- stringr::str_c(node_regex, colloc_regex, sep = "")
  } else {
    colloc_regex_left <- stringr::str_c("\\b([a-zA-Z-]+\\s){", span, "}", sep = "")
    colloc_regex_right <- stringr::str_c("(\\s([a-zA-Z-]+)){", span,"}\\b", sep = "")
    patterns_regex <- stringr::str_c(colloc_regex_left, node_regex, colloc_regex_right, sep = "")
  }

  patterns_regex <- stringr::regex(patterns_regex, ignore_case = case_insensitive)
  patterns <- stringr::str_extract_all(line_match, patterns_regex)
  patterns <- unlist(patterns)
  patterns <- patterns[nzchar(patterns)]
  patterns <- stringr::str_to_lower(patterns)
  pattern_q <- rlang::sym(sprintf("patterns"))
  patterns <- tibble::tibble(!!pattern_q := patterns)

  if (window == "l") {
    extract_regex <- stringr::str_c("^", stringr::str_c(rep("(.+?)\\s", span), collapse = ""), "(.+)$", sep="")
    into_cols <- c(stringr::str_c(window, rev(seq(span))), "node")
    out <- patterns %>%
      extract(!!pattern_q, into = into_cols, regex = extract_regex, remove = FALSE)
  } else if (window == "r") {
    extract_regex <- stringr::str_c("^([^ ]+?)", stringr::str_c(rep("\\s(.+?)", span), collapse = ""), "$", sep="")
    into_cols <- c("node", stringr::str_c(window, seq(span)))
    out <- patterns %>%
      extract(!!pattern_q, into = into_cols, regex = extract_regex, remove = FALSE)
  } else {
    colloc_regex_left <- stringr::str_c("^", stringr::str_c(rep("(.+?)\\s", span), collapse = ""), sep="")
    colloc_regex_right <- stringr::str_c(stringr::str_c(rep("\\s(.+?)", span), collapse = ""), "$", sep="")
    node_regex <- "([^ ]+?)"
    extract_regex <- stringr::str_c(colloc_regex_left, node_regex, colloc_regex_right)
    into_cols <- c(stringr::str_c("l", rev(seq(span))), "node", stringr::str_c("r", seq(span)))
    out <- patterns %>%
      extract(!!pattern_q, into = into_cols, regex = extract_regex, remove = FALSE)
  }

  node_q <- rlang::sym(sprintf("node"))
  colloc_q <- rlang::sym(sprintf("collocates"))
  span_q <- rlang::sym(sprintf("span"))
  if (gathered == TRUE) {
    out <- out %>%
      gather(key = !!span_q, value = !!colloc_q, -!!pattern_q, -!!node_q)
    return(out %>%
             arrange(!!pattern_q) %>%
             select(!!pattern_q, !!node_q, !!colloc_q, !!span_q))
  } else {
    return(out %>%
             arrange(!!pattern_q))
  }
}



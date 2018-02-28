#' Generate tidyverse-style sequential, ngram-like collocates
#'
#' @description The function produces window-span collocates in the following scheme: ...\\scolloc\\scolloc\\sNODE\\scolloc\\scolloc\\s...
#' @param corpus a raw corpus vector loaded via \code{scan()} or \code{readr::read_lines()}
#' @param patterns regular expressions/strings for node-word to search in the corpus
#' @param window window-span direction of the collocates: \code{"r"} ('right of the node'), \code{"l"} ('left of the node'), or the DEFAULT is \code{"b"} ('both window')
#' @param span numeric vector indicating the span of the collocate scope
#' @param gathered whether the output table should be in tidy format (\code{TRUE}) or not (\code{FALSE} -- default). This makes use of \code{tidyr}'s \code{gather()}.
#' @param case_insensitive whether the search ignore case (\code{TRUE} -- default) or not (\code{FALSE})
#' @return collocates-tibble consisting of the \code{patterns} and collocates per \code{span} as individual columns
#' @importFrom stringr str_c
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom dplyr %>%
#'
#' @examples
#' Do not run!
#'
#' # load a Leipzig corpus file
#' corpus <- readr::read_lines(corpus_files_path[2])
#'
#' # search right-side collocates within two words from the node
#' colloc <- colloc_extract(corpus, "\\bmemberi\\b", "r", 2)
#'
#' # explore the output
#' colloc %>%
#' filter(!r2 %in% c("kepada", "bagi", "untuk", "terhadap", "ke"))

colloc_extract <- function(corpus = "corpus vector", pattern = "regex", window = "b", span = "numeric", gathered = FALSE, case_insensitive = TRUE) {
  node_regex <- regex(pattern, ignore_case = case_insensitive)
  line_match <- corpus %>% str_subset(node_regex)
  if (window == "l") {
    #span_regex <- str_c(seq(span), collapse = ", ")
    colloc_regex <- str_c("\\b([a-zA-Z-]+\\s){", span, "}", sep = "")
    patterns_regex <- str_c(colloc_regex, node_regex, sep = "")
  } else if (window == "r") {
    #span_regex <- str_c(seq(span), collapse = ", ")
    colloc_regex <- str_c("(\\s([a-zA-Z-]+)){", span,"}\\b", sep = "")
    patterns_regex <- str_c(node_regex, colloc_regex, sep = "")
  } else {
    colloc_regex_left <- str_c("\\b([a-zA-Z-]+\\s){", span, "}", sep = "")
    colloc_regex_right <- str_c("(\\s([a-zA-Z-]+)){", span,"}\\b", sep = "")
    patterns_regex <- str_c(colloc_regex_left, node_regex, colloc_regex_right, sep = "")
  }

  patterns_regex <- regex(patterns_regex, ignore_case = case_insensitive)
  patterns <- str_extract_all(line_match, patterns_regex) %>%
    unlist() %>%
    tolower() %>%
    tibble(patterns = .)

  if (window == "l") {
    extract_regex <- str_c("^", str_c(rep("(.+?)\\s", span), collapse = ""), "(.+)$", sep="")
    into_cols <- c(str_c(window, rev(seq(span))), "node")
    out <- patterns %>%
      extract(patterns, into = into_cols, regex = extract_regex, remove = FALSE)
  } else if (window == "r") {
    extract_regex <- str_c("^([^ ]+?)", str_c(rep("\\s(.+?)", span), collapse = ""), "$", sep="")
    into_cols <- c("node", str_c(window, seq(span)))
    out <- patterns %>%
      extract(patterns, into = into_cols, regex = extract_regex, remove = FALSE)
  } else {
    colloc_regex_left <- str_c("^", str_c(rep("(.+?)\\s", span), collapse = ""), sep="")
    colloc_regex_right <- str_c(str_c(rep("\\s(.+?)", span), collapse = ""), "$", sep="")
    node_regex <- "([^ ]+?)"
    extract_regex <- str_c(colloc_regex_left, node_regex, colloc_regex_right)
    into_cols <- c(str_c("l", rev(seq(span))), "node", str_c("r", seq(span)))
    out <- patterns %>%
      extract(patterns, into = into_cols, regex = extract_regex, remove = FALSE)
  }

  if (gathered == TRUE) {
    out <- out %>%
      gather(key = span, value = collocates, -patterns, -node)
    return(out %>%
             arrange(patterns) %>%
             select(patterns, node, collocates, span))
  } else {
    return(out %>%
             arrange(patterns))
  }
}



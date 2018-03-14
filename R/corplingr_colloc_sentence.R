#' Collocates retrieval on sentence-based corpus
#'
#' @description Perform collocate search for a given word/pattern based on corpus text files with one sentence per line (e.g., the Leipzig Corpora) (cf. \bold{Details} below).
#' @param corpus_path character strings of (full) filepath for the corpus text files in \code{.txt} plain-text format.
#'     The corpus file IS a sentence-based corpus, meaning that each line of the file corresponds to one sentence.
#'     Each sentence can be in successive, cohesive sequence (e.g. based on a Novel) or randomised (as in the Leipzig Corpora).
#' @param leipzig_input logical; to check if the input corpus is specifically the Leipzig corpus files (\code{TRUE}) so that the function will remove the sentence number in the beginning of the line.
#' @param pattern regular expressions/exact patterns for the target pattern.
#' @param window window-span direction of the collocates: \code{"r"} ('\bold{right} of the node'), \code{"l"} ('\bold{left} of the node'), or the DEFAULT is \code{"b"} ('both \bold{left} and \bold{right} context-window').
#' @param span integer vector indicating the span of the collocate scope.
#' @param case_insensitive whether the search ignores case (TRUE -- the default) or not (FALSE).
#' @param to_lower_colloc whether to lowercase the retrieved collocates and the nodes (TRUE -- default) or not (FALSE).
#' @param save_interim_results whether to output the interim results (per corpus file) into a tab-separated plain text (TRUE) or not (FALSE -- default).
#' @param coll_output_name name of the file for the collocate tables.
#' @return A tbl_df of raw collocates.
#' @details This function, which is largely built on top of the \code{tidyverse}, is specifically designed to handle collocates search that is not crossing boundary of the sentence in which the search word/pattern occurs.
#'     The reason is that the sentence can be randomised and totaly unrelated (as in the Leipzig Corpora).
#'     Thus, it is important to keep the collocates of the search word/pattern falling within the sentence boundary in which the word/pattern occurs. That way, it aims maintain cohesivness of meaning of the word.
#'
#'     Moreover, the function only outputs the raw collocates data without tabulating the frequency of the collocates and performing association measure of the collocates to the search word/pattern.
#'     Future iteration of this package aims to accommodate this feature.
#' @examples
#' \dontrun{
#' # get the path of the Leipzig corpora
#' leipzig_corpus_path <- c("/my/path/to/leipzig_corpus_1M_1.txt",
#'                          "/my/path/to/leipzig_corpus_300K_2.txt",
#'                          "/my/path/to/leipzig_corpus_300K_3.txt")
#' # retrieve collocate list
#' df <- colloc_sentence(corpus_path = leipzig_corpus_path[2:3],
#'                       leipzig_input = TRUE,
#'                       pattern = "\\bterkalahkan\\b",
#'                       window = "l",
#'                       span = 1,
#'                       case_insensitive = TRUE,
#'                       to_lower_colloc = TRUE,
#'                       save_interim_results = FALSE)
#'
#' # see the output
#' df
#'
#' # count the frequency of the collocates
#' df %>% dplyr::count(w, sort = TRUE)
#' }
#' @export
colloc_sentence <- function(corpus_path = "(full) filepath to sentence-based corpus",
                            leipzig_input = TRUE, # check if the sentence corpus is from leipzig corpora
                            pattern = "regular expressions",
                            window = c("r", "l", "b"),
                            span = 3,
                            case_insensitive = TRUE,
                            to_lower_colloc = TRUE,
                            save_interim_results = FALSE, # save results per corpus
                            coll_output_name = "colloc_tibble_out.txt") {

  all_all_colloc <- tibble::tibble()

  # prepare window span
  span_ori <- span
  span <- corplingr::span_set(window = window, span = span)

  for (h in seq_along(corpus_path)) {
    corpus_name <- stringr::str_replace(basename(corpus_path[h]), "\\.txt$", "")
    cat(paste("Loading/reading the corpus ('", corpus_name, "') into R...\n", sep = ""))
    corpus <- readr::read_lines(file = corpus_path[h])
    pattern_rgx <- stringr::regex(pattern = pattern,
                                  ignore_case = case_insensitive)

    # subset sentence with match
    cat("Extract sentences with the match...\n")
    match_id <- stringr::str_which(string = corpus,
                                   pattern = pattern_rgx)
    if (length(match_id) == 0) {
      cat("SORRY! No match found for the pattern!\nTry another corpus!\n\n")
      next
    }
    sent_with_match <- corpus[match_id]

    # remove sentence number
    if (leipzig_input == TRUE) {
      sent_with_match <- stringr::str_replace(sent_with_match,
                                              pattern = "^\\d+\\s",
                                              replacement = "")
    }

    # tokenising the sentence with the match
    tokens <- stringr::str_split(sent_with_match, "[^a-zA-Z-]+")

    # give names for each tokenised sentence
    names(tokens) <- paste("sentence_", match_id, "_", sep = "")

    # remove empty character
    tokens <- purrr::map(tokens, function(words) words[nzchar(words)])

    # lower casing
    if (to_lower_colloc == TRUE) {
      tokens <- purrr::map(tokens, function(words) stringr::str_to_lower(words))
    }

    # provide sentence boundary
    tokens <- purrr::map(tokens, function(words) c(rep("SENTENCE", span_ori), words, rep("SENTENCE", span_ori)))

    # count how many match found in a sentence
    match_length <- purrr::map(tokens, function(words) stringr::str_count(words, pattern = pattern_rgx))
    match_length <- purrr::map_int(match_length, sum)

    # replicate the sentence according to the found match in a sentence
    tokens_rep <- rep(tokens, match_length)
    sent_with_match_rep <- rep(sent_with_match, match_length)

    # get the match vector position
    match_pos <- purrr::map(tokens, function(words) stringr::str_which(words, pattern = pattern_rgx))
    match_pos <- unlist(match_pos)

    # get the collocates vector positions
    vector_pos <- sapply(match_pos, function(match_pos) match_pos + span)
    vector_pos <- as.data.frame(vector_pos)
    vector_pos <- vector_pos[rownames(vector_pos) != "node", ]

    # prepare data storage
    colloc_df_nrows <- (dim(vector_pos)[1] * dim(vector_pos)[2])
    all_colloc <- data.frame(row.names = seq_len(colloc_df_nrows))

    # progress estimation
    p <- dplyr::progress_estimated(n = length(tokens_rep))

    # for loops to gather the collocates
    cat("Now gathering the collocates...\n")
    for (i in seq_along(tokens_rep)) {
      w <- tokens_rep[[i]][vector_pos[,i]]
      w_span <- rownames(vector_pos)
      w_vector_pos <- vector_pos[,i]
      sent_num <- as.numeric(stringr::str_extract(names(tokens_rep[i]), "\\d+"))
      sent_match <- sent_with_match_rep[i]
      df_temp <- data.frame(w, w_vector_pos, w_span, corpus = corpus_name, sent_num, sent_match, stringsAsFactors = FALSE)
      all_colloc <- rbind(all_colloc, df_temp)
      p$pause(0.05)$tick()$print()
    }
    all_colloc <- tibble::as_tibble(all_colloc)
    filter_call <- quo(!w %in% c("SENTENCE", "sentence"))
    all_colloc <- dplyr::filter(all_colloc, !!filter_call)
    cat(paste("\nDone with corpus ", corpus_name, "!\n\n", sep = ""))
    if (save_interim_results == TRUE) {
      readr::write_delim(all_colloc, path = coll_output_name, delim = "\t", append = TRUE)
      cat(paste("\nSaving the interim results for ", corpus_name, "!\n\n", sep = ""))
    } else {
      all_all_colloc <- dplyr::bind_rows(all_all_colloc, all_colloc)
    }
  }
  cat("All done!\n")
  return(all_all_colloc)
}


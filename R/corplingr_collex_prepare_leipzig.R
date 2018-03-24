#' Prepare Leipzig collocates data for collexemes/collocates analysis
#'
#' @description This function is designed to handle the output of \code{\link{colloc_leipzig}} to generate a tidy data frame required as input of \code{\link{collex_fye}}. The latter is used to compute collexeme/collocate analysis using one-tailed \emph{Fisher-Yates Exact} test.
#' @param list_output The list output of \code{\link{colloc_leipzig}}.
#' @param leipzig_wordlist_path Full path to the wordlist table for each Leipzig Corpus File
#' @param node_pattern Regex patterns of the node word specified in \code{\link{colloc_leipzig}}.
#' @param span Character vector of the context-window span user wants to focus on for the collexeme/collocate analysis.
#'     For instance, single span: \code{"l1"}, \code{"r1"}; or multiple spans: \code{c("r1", "r2")}.
#'
#' @return A tibble data frame
#' @examples
#' \dontrun{
#' # retrieve collocates for a given word
#' rgx <- "\\bmengakhir\\b"
#' coll_df <- colloc_leipzig(leipzig_path = leipzig_corpus_path,
#'                        pattern = rgx,
#'                          window = "r",
#'                          span = 4,
#'                          save_results = FALSE,
#'                          to_lower_colloc = TRUE)
#'
#' # get only the collocates output
#' list_output <- coll_df$collocates
#'
#' # collstr analysis for collocates from Leipzig Corpora
#' ### prepare input table for coll.analysis ### <--- HERE IS THE CALL FOR collex_prepare_leipzig()
#' collex_tb <-collex_prepare_leipzig(list_output = coll_df,
#'                                 leipzig_wordlist_path = leipzig_mywordlist_path,
#'                                 node_pattern = rgx,
#'                                 span = c("r1"))
#' # remove any NA row data
#' collex_tb <- dplyr::filter_all(collex_tb,
#'                               dplyr::all_vars(!is.na(.)))
#'
#' # compute one-tailed FYE for collexeme analysis
#' collex_tb <- dplyr::mutate(collex_tb,
#'                           collstr = collex_fye(a = .data$a, # here is the call to collex_fye
#'                                                n_corpus = .data$corpus_size,
#'                                                n_coll = .data$n_w_in_corp,
#'                                                n_cxn = .data$n_pattern))
#'
#' # sort in decreasing order by collostruction strength
#' dplyr::arrange(collex_tb, dplyr::desc(collstr))
#' }
#' @import dplyr
#' @importFrom readr read_tsv
#' @importFrom purrr is_null
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#' @importFrom stringr str_which

collex_prepare_leipzig <- function(list_output = NULL,
                                   leipzig_wordlist_path = leipzig_mywordlist_path,
                                   node_pattern = "regex for the node word",
                                   span = NULL) {

  coll_df <- list_output[[1]]

  corpus_colloc <- unique(coll_df$corpus)
  leipzig_wordlist_name <- stringr::str_extract(basename(leipzig_wordlist_path), "(?<=__)(.+?)(?=\\.txt$)")

  coll_df <- dplyr::filter(coll_df, .data$w_span != "node")

  if (!purrr::is_null(span)) {
    coll_df <- dplyr::filter(coll_df, .data$w_span %in% span)
  }
  coll_df <- dplyr::group_by(coll_df, .data$w, .data$sent_num, .data$corpus)
  coll_df <- dplyr::distinct(coll_df)
  coll_df <- dplyr::ungroup(coll_df)
  coll_df_freq <- dplyr::count(coll_df, .data$w, sort = TRUE)
  coll_df_freq <- dplyr::rename(coll_df_freq, a = .data$n)

  included_corpus_worlist <- leipzig_wordlist_path[which(corpus_colloc %in% leipzig_wordlist_name)]
  cat("Loading all wordlist table for each corpus...\n")
  wordlist_all <- vector(mode = "list", length = length(included_corpus_worlist))
  for (i in seq_along(included_corpus_worlist)) {
    wl <- readr::read_tsv(file = included_corpus_worlist[i])
    colnames(wl)[stringr::str_which(colnames(wl), "^word$")] <- "w"
    wordlist_all[[i]] <- wl
  }
  cat("Summarising all word frequency...\n")
  wordlist_all <- dplyr::bind_rows(wordlist_all)
  wordlist_all <- dplyr::group_by(wordlist_all, .data$w)
  wordlist_all <- dplyr::summarise(wordlist_all, n_w_in_corp = sum(.data$n))
  wordlist_all <- dplyr::ungroup(wordlist_all)

  corpus_size <- unname(unlist(dplyr::tally(wordlist_all, .data$n_w_in_corp)))
  n_pattern <- dplyr::filter(wordlist_all, stringr::str_detect(.data$w, node_pattern))
  n_pattern <- sum(dplyr::pull(n_pattern, .data$n_w_in_corp))

  collex_tb <- dplyr::left_join(coll_df_freq, wordlist_all, by = "w")
  collex_tb$corpus_size <- corpus_size
  collex_tb$n_pattern <- n_pattern
  return(collex_tb)

}



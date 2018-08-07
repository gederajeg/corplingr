#' Prepare Leipzig collocates data for collexemes/collocates analysis
#'
#' @description This function is designed to handle the output of \code{\link{colloc_leipzig}} to generate a tidy data frame required as input of \code{\link{collex_fye}}.
#'     The latter is used to compute collexeme/collocate strength using one-tailed \emph{Fisher-Yates Exact} test.
#' @param list_output The list output of \code{\link{colloc_leipzig}}.
#' @param leipzig_wordlist_path Full path to the wordlist table for each Leipzig Corpus File. This can be a plain text file or an .RData file.
#' @param node_pattern Regex patterns of the node word specified in \code{\link{colloc_leipzig}}.
#' @param span Character vector of the context-window span user wants to focus on for the collexeme/collocate analysis.
#'     For instance, single span: \code{"l1"}, \code{"r1"}; or multiple spans: \code{c("r1", "r2")}.
#' @param stopwords_list A character vector of the stopword list.
#'
#' @return A tibble data frame
#' @examples
#' \dontrun{
#' # retrieve collocates for a given word
#' rgx <- "\\bmengakhir\\b"
#' coll_df <- colloc_leipzig(leipzig_path = leipzig_corpus_path,
#'                           pattern = rgx,
#'                           window = "r",
#'                           span = 4,
#'                           save_results = FALSE,
#'                           to_lower_colloc = TRUE)
#'
#' # get only the collocates output
#' list_output <- coll_df$collocates
#'
#' # collstr analysis for collocates from Leipzig Corpora
#' ### prepare input table for coll.analysis ### <--- HERE IS THE CALL FOR collex_prepare_leipzig()
#' collex_tb <- collex_prepare_leipzig(list_output = coll_df,
#'                                    leipzig_wordlist_path = leipzig_mywordlist_path,
#'                                    node_pattern = rgx,
#'                                    span = c("r1"),
#'                                    stopwords_list = NULL)
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
#' @importFrom readr read_tsv
#' @importFrom purrr is_null
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#' @importFrom stringr str_which
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr distinct
#' @importFrom dplyr ungroup
#' @importFrom dplyr count
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarise
#' @importFrom dplyr tally
#' @importFrom dplyr pull
#' @importFrom dplyr left_join
#' @export

collex_prepare_leipzig <- function(list_output = NULL,
                                   leipzig_wordlist_path = leipzig_mywordlist_path,
                                   node_pattern = "regex for the node word",
                                   span = NULL,
                                   stopwords_list = NULL) {

  # get the raw collocates table
  coll_df <- list_output[[1]]

  # check if stopwords will be removed from the collocates table
  if (!purrr::is_null(stopwords_list)) {
    message("You chose to REMOVE stopwords from the collocate list!")
    coll_df <- dplyr::filter(coll_df, !.data$w %in% stopwords_list)
  } else {
    message("You chose to RETAIN stopwords from the collocate list!")
  }

  # get the unique corpus names from the collocates table
  corpus_colloc <- unique(coll_df$corpus)

  # use the used corpus names to search for collocates as indices for wordlist files included
  leipzig_wordlist_name <- stringr::str_extract(basename(leipzig_wordlist_path), "(?<=__)(.+?)(?=\\.(txt|RData)$)")

  # get the total occurrence of the node pattern based on its occurrences in the collocates table
  n_pattern <- dim(coll_df[coll_df$w_span == "node", ])[1]

  # retrieve only the collocates
  coll_df <- dplyr::filter(coll_df, .data$w_span != "node")

  # check if the collocates to be measured is restricted to particular span from the node-word
  if (!purrr::is_null(span)) {
    coll_df <- dplyr::filter(coll_df, .data$w_span %in% span)
  }

  # make sure that the collocates for each occurrences of the node in each sentence are counted once
  coll_df <- dplyr::group_by(coll_df, .data$w, .data$sent_num, .data$corpus)
  coll_df <- dplyr::distinct(coll_df) # here is the function to get distinct collocates per node per sentence
  coll_df <- dplyr::ungroup(coll_df)

  # get the count of the collocates frequency
  coll_df_freq <- dplyr::count(coll_df, .data$w, sort = TRUE)
  coll_df_freq <- dplyr::rename(coll_df_freq, a = .data$n)

  # prepare data to load wordlist file of the corpus used to retrieve the collocates
  included_corpus_worlist <- leipzig_wordlist_path[which(corpus_colloc %in% leipzig_wordlist_name)]
  message("Loading all wordlist table for each corpus...")
  wordlist_all <- vector(mode = "list", length = length(included_corpus_worlist))
  for (i in seq_along(included_corpus_worlist)) {

    if (grepl("txt$", included_corpus_worlist[i], perl = TRUE)) {

      wl <- readr::read_tsv(file = included_corpus_worlist[i])

    } else if (grepl("RData$", included_corpus_worlist[i], perl = TRUE)) {

      load(included_corpus_worlist[i])
      wl <- tibble::data_frame(corpus_id = as.character(wlist.df$corpus_id),
                               word = as.character(wlist.df$word),
                               n = wlist.df$n)

    } else {

      stop("The wordlist file should be in either `.txt` or `.RData` file!\n")

    }

    colnames(wl)[stringr::str_which(colnames(wl), "^word$")] <- "w"
    wordlist_all[[i]] <- wl
  }
  message("Summarising all word frequency...")

  # combined all wordlist data frame
  wordlist_all <- dplyr::bind_rows(wordlist_all)

  # remove stopwords from wordlist data frames
  if (!purrr::is_null(stopwords_list)) {
    wordlist_all <- dplyr::filter(wordlist_all, !.data$w %in% stopwords_list)
  } else {
    cat(" ")
  }

  # summarising all the wordlist token frequency
  wordlist_all <- dplyr::group_by(wordlist_all, .data$w)
  wordlist_all <- dplyr::summarise(wordlist_all, n_w_in_corp = sum(.data$n))
  wordlist_all <- dplyr::ungroup(wordlist_all)

  # get the total corpus size from tallying token frequency in each wordlist table
  corpus_size <- unname(unlist(dplyr::tally(wordlist_all, .data$n_w_in_corp)))
  # n_pattern <- dplyr::filter(wordlist_all, stringr::str_detect(.data$w, node_pattern))
  # n_pattern <- sum(dplyr::pull(n_pattern, .data$n_w_in_corp))

  # prepare the output tabble for collexeme analysis
  collex_tb <- dplyr::left_join(coll_df_freq, wordlist_all, by = "w")
  collex_tb$corpus_size <- corpus_size
  collex_tb$n_pattern <- n_pattern
  return(collex_tb)
}



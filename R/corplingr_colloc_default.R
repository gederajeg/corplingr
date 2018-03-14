#' Collocates retrieval for raw corpus text
#'
#' @param corpus_path character strings of (full) filepath for the corpus text files in \code{.txt} plain-text format.
#' @param corpus_list a named list object containing elements constituting a corpus text.
#'     The name of each element should correspond to the corpus file.
#'     There can be more than one element (hence more than one corpus text) within this list object.
#' @param pattern regular expressions/exact patterns for the target pattern.
#' @param window window-span direction of the collocates: \code{"r"} ('\bold{right} of the node'), \code{"l"} ('\bold{left} of the node'), or the DEFAULT is \code{"b"} ('both \bold{left} and \bold{right} context-window').
#' @param span integer vector indicating the span of the collocate scope.
#' @param word_split_regex user-defined regular expressions to tokenise the corpus.
#'     The default is to split at non alphabetic characters but retain hypen "-" as to maintain reduplication, for instance.
#'     The regex for this default setting is \code{"([^éa-zA-Z-]+|--)"}.
#' @param case_insensitive whether the search pattern ignores case (TRUE -- the default) or not (FALSE).
#' @param to_lower_colloc whether to lowercase the retrieved collocates (TRUE -- default) or not (FALSE).
#' @param tokenise_corpus_to_sentence whether to tokenise the input corpus by sentence so that the script can handle the collocates for not crossing sentence boundary.
#'     The default is \code{TRUE} and it uses \code{\link[stringi]{stri_split_boundaries}} to tokenise into sentence before further tokenising into word-tokens with \code{\link[stringr]{str_split}}.
#' @return A list of three elements:
#' \itemize{
#'     \item A tibble of all words in the corpus including the sentence number;
#'     \item A tibble of all retrieved collocates, including their span position and sentence number;
#'     \item Regular expression object of the search pattern.
#' }
#' @examples
#' \dontrun{
#' # do the collocate search
#' df <- colloc_default(corpus_path = orti_bali_path,
#'                      pattern = "^nuju$",
#'                      window = "b", # focusing on both left and right context window
#'                      span = 3) # retrieve 3 collocates to the left and right of the node
#' }
#é
colloc_default <- function(corpus_path = NULL,
                           corpus_list = NULL,
                           pattern = NULL,
                           window = "b",
                           span = 3,
                           word_split_regex = "([^a-zA-Z-]+|--)",
                           case_insensitive = TRUE,
                           to_lower_colloc = TRUE,
                           tokenise_corpus_to_sentence = TRUE) {

  # generate the context-window range
  span_ori <- span
  span <- span_set(window = window, span = span)
  span <- span[names(span) != "node"]

  if (purrr::is_null(corpus_path)) {
    iterators <- length(corpus_list)
    corpus_input <- corpus_list
  } else if (purrr::is_null(corpus_list)) {
    iterators <- length(corpus_path)
    corpus_input <- corpus_path
  }

  # prepare the output storage as list
  all_word_vectors <- vector(mode = "list", length = iterators)
  all_colloc <- vector(mode = "list", length = iterators)

  # progress estimation
  p <- dplyr::progress_estimated(n = iterators)

  for (h in seq_along(corpus_input)) {

    # print out progress report
    p$pause(0.05)$tick()$print()

    # check the type of input file
    if (typeof(corpus_input) == "character") {
      corpus_name <- stringr::str_replace(basename(corpus_input[h]), "\\.txt$", "")
      #cat(paste("Loading/reading the corpus ('", corpus_name, "') into R...\n", sep = ""))

      # load/read each corpus file
      corpus <- readr::read_lines(file = corpus_input[h])

    # if it is a list object of text
    } else if (typeof(corpus_input) == "list") {

      # get the corpus element from the list
      corpus <- corpus_input[[h]]

      # define the corpus name
      if (purrr::is_null(names(corpus_input)) == TRUE) {
        corpus_name <- stringr::str_c("corpus_no_", h, sep = "")
      } else {
        corpus_name <- names(corpus_input)[h]
      }
    }

    # tokenise into sentence
    if (tokenise_corpus_to_sentence == TRUE) {
      corpus <- corplingr::tokenise_sentence(strings = corpus,
                                             to_lower = to_lower_colloc,
                                             window_span = span_ori)
    }

    # generate the search regex
    pattern_rgx <- stringr::regex(pattern = pattern,
                                  ignore_case = case_insensitive)

    # split into word vector
    word_vector <- stringr::str_split(string = corpus, pattern = word_split_regex)
    word_vector <- purrr::set_names(word_vector,
                                    nm = stringr::str_c("sent_", 1:length(word_vector), "_", sep = ""))
    word_vector <- unlist(word_vector)
    word_vector <- word_vector[nzchar(word_vector)]

    # store the word vector and original corpus as tibble data frame
    word_vector_tb <- tibble::tibble(!!dplyr::quo_name(dplyr::quo(w)) := word_vector,
                                     !!dplyr::quo_name(dplyr::quo(w_vector_pos)) := 1:length(word_vector),
                                     !!dplyr::quo_name(dplyr::quo(sent_num)) := names(word_vector),
                                     !!dplyr::quo_name(dplyr::quo(corpus_name)) := corpus_name)
    word_vector_tb <- dplyr::mutate(word_vector_tb,
                                    !!dplyr::quo_name(dplyr::quo(sent_num)) := as.integer(stringr::str_replace_all(!!dplyr::quo(sent_num), "(_\\d+$|^sent_)", "")))
    corpus <- stringr::str_replace_all(corpus,
                                       pattern = stringr::regex("(ZSENTENCEZ|zsentencez)", ignore_case = case_insensitive),
                                       replacement = "")
    corpus <- stringr::str_trim(corpus)
    corpus_tb <- tibble::tibble(!!dplyr::quo_name(dplyr::quo(sent_num)) := 1:length(corpus),
                                !!dplyr::quo_name(dplyr::quo(sent_match)) := corpus)

    # get pattern/word vector position
    match_id <- stringr::str_which(string = word_vector, pattern = pattern_rgx)

    if ((length(match_id) > 0) == TRUE) {

      # get collocates vector position
      colloc_vector_pos <- vector(mode = "list", length = length(match_id))
      for (m in seq_along(match_id)) {
        colloc_pos_temp <- match_id[m] + span
        colloc_vector_pos[[m]] <- colloc_pos_temp[colloc_pos_temp >= 1 & # ensure the positive position of the collocates within the corpus vector
                                                    colloc_pos_temp <= length(word_vector) & # ensure the position of the collocates are not over the length of words in the corpus
                                                    !colloc_pos_temp %in% match_id] # ensure that the collocate position is not the position of the match itself
      }
      colloc_vector_pos <- unlist(colloc_vector_pos)

      # extract the collocates
      colloc_pos_tb <- tibble::tibble(!!dplyr::quo_name(dplyr::quo(w_vector_pos)) := colloc_vector_pos,
                                      !!dplyr::quo_name(dplyr::quo(w_span)) := names(colloc_vector_pos))
      colloc_pos_tb <- colloc_pos_tb[!duplicated(colloc_pos_tb$w_vector_pos), ]
      colloc_temp_tb <- dplyr::left_join(colloc_pos_tb, word_vector_tb, by = "w_vector_pos")
      colloc_temp_tb <- dplyr::left_join(colloc_temp_tb, corpus_tb, by = "sent_num")

      # store all collocates and remove the "ZSENTENCEZ/zsentencez" collocates
      all_colloc[[h]] <- dplyr::filter(colloc_temp_tb, !!dplyr::quo(!w %in% c("ZSENTENCEZ", "zsentencez")))

      # gather all words in the corpus
      all_word_vectors[[h]] <- dplyr::filter(word_vector_tb, !!dplyr::quo(!w %in% c("ZSENTENCEZ", "zsentencez")))
    }
  }
  all_word_vectors <- dplyr::bind_rows(all_word_vectors)
  all_colloc <- dplyr::bind_rows(all_colloc)

  return(list(wordlist_tb = all_word_vectors,
              colloc_tb = all_colloc,
              pattern_regex = pattern_rgx))
}





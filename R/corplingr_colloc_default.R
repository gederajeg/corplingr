#' Collocates retrieval for raw corpus text
#'
#' @description This function retrieve collocates for a word within the user-defined context window based on raw/unannotated corpus texts.
#'     The function use vectorisation approach to determine the \bold{vector}-position of the collocates in relation to the vector-position of the node-word in the corpus word-vector.
#'     There is the argument of \code{tokenise_corpus_to_sentence} (cf. below) that allows user to first split the input, raw corpus into character vector whose elements correspond to a sentence line.
#' @param corpus_path character strings of (full) filepath for the corpus text files in \code{.txt} plain-text format.
#' @param corpus_list a named list object containing elements constituting a corpus text.
#'     The name of each element should correspond to the corpus file.
#'     There can be more than one element (hence more than one corpus text) within this list object.
#' @param pattern regular expressions/exact patterns for the target pattern.
#' @param window window-span direction of the collocates: \code{"r"} ('\bold{right} of the node'), \code{"l"} ('\bold{left} of the node'), or the DEFAULT is \code{"b"} ('both \bold{left} and \bold{right} context-window').
#' @param span integer vector indicating the span of the collocate scope.
#' @param word_split_regex user-defined regular expressions to tokenise the corpus.
#'     The default is to split at non alphabetic characters but retain hypen "-" as to maintain reduplication, for instance.
#'     The regex for this default setting is \code{""([^a-zA-Z-]+|--)""}. Another possible splitting regex may include various characters with diacritics (e.g., \code{'([^a-zA-Z\u00c0-\u00d6\u00d9-\u00f6\u00f9-\u00ff\u0100-\u017e\u1e00-\u1eff]+|--)'})
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
#' # do the collocate search using "corpus_path" input-option
#' df <- colloc_default(corpus_path = orti_bali_path,
#'                      pattern = "^nuju$",
#'                      window = "b", # focusing on both left and right context window
#'                      span = 3) # retrieve 3 collocates to the left and right of the node
#' }
#' @importFrom rlang .data
#' @importFrom purrr is_null
#' @export
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

  # check the type of input file
  if (typeof(corpus_input) == "character") {
    corpus_names <- stringr::str_replace(basename(corpus_input), "\\.txt$", "")
    #cat(paste("Loading/reading the corpus ('", corpus_names, "') into R...\n", sep = ""))

    message("Loading the corpus into R...\n")

    # load/read each corpus file
    corpus <- purrr::map(corpus_path, readr::read_lines)

    # extract non-zero character
    corpus <- purrr::map(corpus, function(x) x[nchar(x) > 1])

    # if it is a list object of text
  } else if (typeof(corpus_input) == "list") {

    # get the corpus element from the list
    corpus <- corpus_input

    # define the corpus name
    if (purrr::is_null(names(corpus_input)) == TRUE) {
      corpus_names <- stringr::str_c("corpus_no_", seq(length(corpus)), sep = "")
    } else {
      corpus_names <- names(corpus)
    }

    rm(corpus_input)
  }

  # tokenise into sentence
  if (tokenise_corpus_to_sentence == TRUE) {
    corpus <- purrr::map(corpus, corplingr::tokenise_sentence,
                         to_lower = to_lower_colloc,
                         window_span = span_ori)
  }

  # names the corpus
  names(corpus) <- corpus_names

  # generate the search regex
  pattern_rgx <- stringr::regex(pattern = pattern,
                                ignore_case = case_insensitive)

  # split into word vector
  message("Tokenising the corpus into word-tokens...\n")
  wv <- purrr::map(corpus, stringr::str_split, pattern = word_split_regex)
  wv <- purrr::map(wv, function(x) purrr::set_names(x, nm = stringr::str_c("_sent_", 1:length(x), "_")))
  wv1 <- unlist(wv)
  wv1 <- wv1[nzchar(wv1)]

  # store the word vector and original corpus as tibble data frame
  sent_id <- NULL
  word_vector_tb <- tibble::tibble(w = unname(wv1),
                                   w_vector_pos = 1:length(wv1),
                                   sent_id = gsub("^.+?(?=sent_)", "", names(wv1), perl = TRUE),
                                   w_pos_in_corpus = as.integer(gsub("^sent.+_(?=\\d+$)", "", .data$sent_id, perl = TRUE)),
                                   corpus_names = gsub("\\._sent.+$", "", names(wv1), perl = TRUE))
  word_vector_tb <- dplyr::mutate(word_vector_tb,
                                  sent_id = as.integer(gsub("(_\\d+$|^sent_)", "", .data$sent_id, perl = TRUE)))

  corpus_only <- purrr::map(corpus, stringr::str_replace_all, pattern = stringr::regex("(ZSENTENCEZ|zsentencez)", ignore_case = case_insensitive), replacement = "")
  corpus_only <- purrr::map(corpus_only, stringr::str_trim)
  corpus_vector <- unlist(corpus_only); rm(corpus_only)
  corpus_tb <- tibble::tibble(sent_id = as.integer(stringr::str_extract(names(corpus_vector), "\\d+$")),
                              corpus_names = gsub("\\d+$", "", names(corpus_vector), perl = TRUE),
                              sent_match = unname(corpus_vector))

  # get pattern/word vector position
  match_id <- stringr::str_which(string = wv1, pattern = pattern_rgx)

  if ((length(match_id) > 0) == TRUE) {

    message("Gathering the collocates...\n")

    # get collocates vector position
    colloc_vector_pos <- map(match_id, function(x) x + span)
    colloc_vector_pos1 <- map(colloc_vector_pos, function(x) x[x >= 1 & # ensure the positive position of the collocates within the corpus vector
                                                                 x <= length(wv1) & # ensure the position of the collocates are not over the length of words in the corpus
                                                                 !x %in% match_id]) # ensure that the collocate position is not the position of the match itself

    colloc_vector_pos <- unlist(colloc_vector_pos1); rm(colloc_vector_pos1)

    # extract the collocates
    colloc_pos_tb <- tibble::tibble(w_vector_pos = colloc_vector_pos,
                                    span = names(colloc_vector_pos))
    colloc_pos_tb <- colloc_pos_tb[!duplicated(colloc_pos_tb$w_vector_pos), ]
    colloc_temp_tb <- dplyr::left_join(colloc_pos_tb, word_vector_tb, by = "w_vector_pos")
    colloc_temp_tb <- dplyr::left_join(colloc_temp_tb, corpus_tb, by = c("sent_id", "corpus_names"))
    colloc_temp_tb <- dplyr::select(colloc_temp_tb, .data$corpus_names,
                                    .data$sent_id, .data$w_pos_in_corpus, .data$w, .data$span,
                                    .data$sent_match)

    # store all collocates and remove the "ZSENTENCEZ/zsentencez" collocates
    colloc_temp_tb <- dplyr::filter(colloc_temp_tb, !.data$w %in% c("ZSENTENCEZ", "zsentencez"))

    # gather all words in the corpus
    all_word_vectors <- dplyr::filter(word_vector_tb, !.data$w %in% c("ZSENTENCEZ", "zsentencez"))

  } else {

    stop("No match is found!")

  }


  message("Done!\n")
  return(list(wordlist_tb = all_word_vectors,
              colloc_tb = colloc_temp_tb,
              pattern_regex = pattern_rgx))

}

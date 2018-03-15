#' Generate tidyverse-style window-span collocates for the Leipzig Corpora
#'
#' @description The function produces tibble-output collocates for Leipzig Corpora files.
#' @param leipzig_path character strings of (i) file names of the Leipzig corpus if they are in the working directory, or (ii) the complete file path to each of the Leipzig corpus files.
#' @param leipzig_corpus_list specify this argument if each Leipzig corpus file has been loaded as R object and acts as an element of a list.
#'     Example of this type of data-input can be seen in \code{data("demo_corpus_leipzig")}.
#'     So specify either \code{leipzig_path} OR \code{leipzig_corpus_list} and set one of them to \code{NULL}.
#' @param pattern regular expressions/exact patterns for the target pattern.
#' @param window window-span direction of the collocates: \code{"r"} ('\bold{right} of the node'), \code{"l"} ('\bold{left} of the node'), or the DEFAULT is \code{"b"} ('both \bold{left} and \bold{right} context-window').
#' @param span integer vector indicating the span of the collocate scope.
#' @param case_insensitive whether the search pattern ignores case (TRUE -- the default) or not (FALSE).
#' @param to_lower_colloc whether to lowercase the retrieved collocates and the nodes (TRUE -- default) or not (FALSE).
#' @param save_results whether to output the collocates into a tab-separated plain text (TRUE) or not (FALSE -- default).
#' @param coll_output_name name of the file for the collocate tables.
#' @param sent_output_name name of the file for the full sentence match containing the collocates.
#' @return a list of two tibbles: (i) for collocates with sentence number of the match, window span information, and the corpus files, and (ii) full-sentences per match with sentence number and corpus file
#' @importFrom dplyr progress_estimated
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
#' @importFrom readr write_delim
#' @importFrom readr write_lines
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom purrr map_chr
#' @importFrom purrr set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#' @importFrom stringr regex
#' @importFrom stringr str_which
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom stringr str_sub
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom stringr str_subset
#' @importFrom dplyr %>%
#' @importFrom dplyr quo_name
#' @importFrom dplyr quo
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' # get the corpus filepaths
#' # so this example use the filepath input rather than list of corpus
#' leipzig_corpus_path <- c("my/path/to/leipzig_corpus_file_1M-sent_1.txt",
#'                        "my/path/to/leipzig_corpus_file_300K-sent_2.txt",
#'                        "my/path/to/leipzig_corpus_file_300K-sent_3.txt")
#'
#' # run the function
#' colloc <- colloc_leipzig(leipzig_path = leipzig_corpus_path[2:3],
#'                               pattern = "\\bterelakkan\\b",
#'                               window = "b",
#'                               span = 3,
#'                               save_results = FALSE,
#'                               to_lower_colloc = TRUE)
#' # Inspect outputs
#' ## This one outputs the collocates tibble
#' colloc$collocates
#'
#' ## This one outputs the sentence matches tibble
#' colloc$sentence_matches
#' }
#' @export


#leipzig_path = leipzig_corpus_path[2]
#pattern = "\\bmembeli\\b"
#window = "b"
#span = 4
#case_insensitive = TRUE
#to_lower_colloc = TRUE
#save_results = FALSE
#c = 1
#r = 1
#s = 1


colloc_leipzig <- function(leipzig_path = NULL,
                           leipzig_corpus_list = NULL,
                           pattern = NULL,
                           window = "b",
                           span = 2,
                           case_insensitive = TRUE,
                           to_lower_colloc = TRUE,
                           save_results = FALSE,
                           coll_output_name = "colloc_tidy_colloc_out.txt",
                           sent_output_name = "colloc_tidy_sent_out.txt") {

  # check the specified argument for the corpus input
  if (purrr::is_null(leipzig_path) & !purrr::is_null(leipzig_corpus_list)) {

    # corpus input as a list of text
    corpus_input <- leipzig_corpus_list

    # get the corpus names
    corpus_names <- names(leipzig_corpus_list)

    if (purrr::is_null(corpus_names)) {
      corpus_names <- stringr::str_c("corpus_list_element_",
                                     seq_along(leipzig_corpus_list), sep = "")
    }

  } else if (!purrr::is_null(leipzig_path) & purrr::is_null(leipzig_corpus_list)) {

    # corpus input as filepath
    corpus_input <- leipzig_path

    # corpus names
    corpus_names <- stringr::str_replace(basename(corpus_input), '-sentences.*$', '')
  } # end of the check corpus input type


  # preparing the window span
  span <- corplingr::span_set(window = window, span = span)
  corp_q <- rlang::sym(sprintf("corpus"))

  # output storage
  colloc_all <- vector(mode = "list", length = length(corpus_input))
  sent_match_all <- vector(mode = "list", length = length(corpus_input))
  #colloc_all <- tibble::tibble()
  #sent_match_all <- tibble::tibble()

  for (c in seq_along(corpus_input)) {

    if (typeof(corpus_input) == "character") {
      # read in the corpus text if it is a filepath
      corpora <- readr::read_lines(file = corpus_input[c])
      cat('"', basename(corpus_input[c]), '" ', "has been loaded!\n", sep = "")
    } else if (typeof(corpus_input) == "list") {
      corpora <- corpus_input[[c]]
      cat('Processing "', corpus_names[c], '"...\n', sep = "")
    }

    for (r in seq_along(pattern)) {
      # detect the search pattern and retrieve the citation with the match
      match_id <- stringr::str_which(corpora, stringr::regex(pattern[r], ignore_case = case_insensitive))
      sub_corpus <- corpora[match_id]

      # detect if any matches found
      if (length(sub_corpus) == 0) {

        cat("NO MATCH(ES) for the pattern you are searching for!\nTRY another corpus file!\n\n")
        next

      } else {

        cat("At least one match for the search pattern is detected in the corpus!\n\n")

        ## PREPARE THE MATCHED SENTENCES
        # detect the sentence number in which the match is found
        sent_id <- match_id

        # delete sentence number and put spaces around non-white-space characters
        sub_corpus <- stringr::str_replace_all(sub_corpus, "^(\\d+?\\s)", "")
        sub_corpus <- stringr::str_replace_all(sub_corpus, "(--)", " ")
        sub_corpus <- stringr::str_replace_all(sub_corpus, "([^a-zA-Z0-9-\\s]+)", " \\1 ")
        sub_corpus <- stringr::str_replace_all(sub_corpus, "\\s+", " ")
        sub_corpus <- stringr::str_trim(sub_corpus, side = "both")

        # retrieve the corpus names
        corpus_name <- corpus_names[c]

        # get the number of matches of the search word found in the corpus
        match_length <- stringr::str_count(sub_corpus, stringr::regex(pattern[r], ignore_case = case_insensitive))

        # replicate the sentences/string based on the number of matches found in the string
        sent_with_match <- rep(sub_corpus, match_length)

        # replicate the sentence numbers/IDs based on the number of matches found in the string
        sent_id <- rep(sent_id, match_length)

        # get the starting and end position of the pattern
        # and store as a tibble
        position <- stringr::str_locate_all(sub_corpus, stringr::regex(pattern[r], ignore_case = case_insensitive))
        position <- purrr::map(position, tibble::as_tibble)
        position <- purrr::map_df(position, dplyr::bind_rows)

        ## RETRIEVE THE COLLOCATES
        all_colloc <- vector(mode = "list", length = length(sent_with_match))
        all_sent_match <- vector(mode = "list", length = length(sent_with_match))
        #all_colloc <- tibble()
        #all_sent_match <- tibble()
        p <- dplyr::progress_estimated(n = length(sent_with_match))
        for (s in seq_along(sent_with_match)) {

          if (position[s,1]==1) {

            node <- stringr::str_sub(sent_with_match[s], position[s,1], position[s,2])
            right <- stringr::str_sub(sent_with_match[s], position[s,2]+1, nchar(sent_with_match[s]))
            sent_match_tagged <- stringr::str_c("NODEWORD", right, sep = "")
            #sent.match.tagged_1 <- stringr::str_c("<node>", node, "</node>", right, sep = "")

          } else if (position[s,2] == nchar(sent_with_match[s])) {

            node <- stringr::str_sub(sent_with_match[s], position[s,1], position[s,2])
            left <- stringr::str_sub(sent_with_match[s], 1, position[s,1]-1)
            sent_match_tagged <- stringr::str_c(left, "NODEWORD", sep = "")
            #sent.match.tagged_1 <- stringr::str_c(left, "<node>", node, "</node>", sep = "")

          } else if (position[s,1] != 1 & position[s,2] != nchar(sent_with_match[s])) {

            node <- stringr::str_sub(sent_with_match[s], position[s,1], position[s,2])
            right <- stringr::str_sub(sent_with_match[s], position[s,2]+1, nchar(sent_with_match[s]))
            left <- stringr::str_sub(sent_with_match[s], 1, position[s,1]-1)
            sent_match_tagged <- stringr::str_c(left, "NODEWORD", right, sep = "")
            #sent.match.tagged_1 <- stringr::str_c(left, "<node>", node, "</node>", right, sep = "")

          } else if (position[s,1] == 1 & position[s,2] == nchar(sent_with_match[s])) {
            next
          }

          # get the vector position of every word-character in a sentence
          colloc <- stringr::str_split(sent_match_tagged, stringr::regex("[^a-z0-9-]+", ignore_case = T))
          colloc <- unlist(colloc)
          colloc <- colloc[nzchar(colloc)]

          # give names for each substracted word with their resulting 'vector position'
          colloc <- purrr::set_names(x = colloc, nm = seq(length(colloc)))

          # store these words and their vector positions into a tibble
          sent_match <- sent_with_match[s]
          w <- colloc
          w_vector_pos <- as.integer(names(colloc))
          colloc <- tibble::tibble(w,
                                   w_vector_pos,
                                   sent_match)

          if (to_lower_colloc == TRUE) {
            colloc <- dplyr::mutate(colloc,
                                    !!dplyr::quo_name(dplyr::quo(w)) := stringr::str_to_lower(!!dplyr::quo(w)))
          } else {
            colloc <- colloc
          }
          rm(sent_match_tagged)

          # calculate the collocate position of each words in relation to the node word
          colloc_vector_pos <- dplyr::select(colloc, -!!dplyr::quo(sent_match))
          colloc_vector_pos <- dplyr::filter(colloc, stringr::str_detect(!!dplyr::quo(w), stringr::regex("nodeword", ignore_case = TRUE))) # get the vector position of the node
          colloc_vector_pos <- colloc_vector_pos$w_vector_pos + span # get the span vector-position for the collocates
          colloc_vector_pos <- colloc_vector_pos[colloc_vector_pos <= dim(colloc)[1] & colloc_vector_pos > 0]

          if (all(names(colloc_vector_pos) == "node") == TRUE) {
            next
          } else {
            # retrive the collocates and left_join them with their span ID
            w_span <- names(colloc_vector_pos)
            w_vector_pos <- colloc_vector_pos
            colloc_span_pos <- tibble::tibble(w_span, w_vector_pos)
            colloc <- dplyr::left_join(colloc, colloc_span_pos, by = "w_vector_pos")
            colloc <- dplyr::filter(colloc, !is.na(!!dplyr::quo(w_span)))
            colloc <- dplyr::mutate(colloc,
                                    !!corp_q := corpus_name,
                                    !!dplyr::quo_name(dplyr::quo(w)) := stringr::str_replace(!!dplyr::quo(w), "nodeword", node),
                                    !!dplyr::quo_name(dplyr::quo(sent_num)) := sent_id[s])
            colloc <- dplyr::select(colloc,
                                    !!dplyr::quo(w), !!dplyr::quo(w_vector_pos), !!dplyr::quo(w_span), !!corp_q, !!dplyr::quo(sent_num), !!dplyr::quo(sent_match))

            # create a separate dbase from the collocate list
            corpus_labs <- unique(colloc$corpus)
            sent_num <- unique(colloc$sent_num)
            sent_match <- sent_with_match[s]
            full_text_counter <- tibble::tibble(corpus_labs,
                                              sent_num,
                                              node,
                                              sent_match)
            colnames(full_text_counter)[1] <- "corpus"

            # gather all collocates
            all_colloc[[s]] <- colloc
            #all_colloc <- dplyr::bind_rows(all_colloc, colloc)
            rm(colloc)
            all_sent_match[[s]] <- full_text_counter
            #all_sent_match <- dplyr::bind_rows(all_sent_match, full_text_counter)
            p$pause(0.05)$tick()$print()
            if (s == length(sent_with_match)) {
              cat("\nDone gathering the collocates for each sentence!\n\n")
            }
          }
        } # end of "s" loop
      }
      all_colloc <- dplyr::bind_rows(all_colloc)
      all_sent_match <- dplyr::bind_rows(all_sent_match)
      colloc_all[[c]] <- all_colloc
      sent_match_all[[c]] <- all_sent_match

      #colloc_all <- dplyr::bind_rows(colloc_all, all_colloc)
      #sent_match_all <- dplyr::bind_rows(sent_match_all, all_sent_match)
    } # end of "r" loop
  } # end of "c" loop

  # gathering all output into one tibble by unlisting
  colloc_all <- dplyr::bind_rows(colloc_all)
  sent_match_all <- dplyr::bind_rows(sent_match_all)

  if (dim(colloc_all)[1] == 0) {
    message("No match is found at all!")
  } else {
    if (save_results == TRUE) {
      readr::write_delim(x = colloc_all, path = coll_output_name, delim = "\t", append = TRUE)
      readr::write_delim(x = sent_match_all, path = sent_output_name, delim="\t", append = TRUE)
      cat("Collocates table and sentence match of the node have been saved!\n\n")
    } else {
      out <- list(colloc_all, sent_match_all)
      names(out) <- c("collocates", "sentence_matches")
      return(out)
    }
  }
}



#' Generate tidyverse-style window-span collocates for the Leipzig Corpora
#'
#' @description The function produces tibble-output collocates for Leipzig Corpora files.
#' @param corpus (i) file names of the corpus if they are in the working directory, or (ii) the complete file path to each of the Leipzig corpus files
#' @param pattern regular expressions/exact patterns for the target pattern
#' @param window window-span direction of the collocates: \code{"r"} ('right of the node'), \code{"l"} ('left of the node'), or the DEFAULT is \code{"b"} ('both window')
#' @param span numeric vector indicating the span of the collocate scope
#' @param case_insensitive whether the search ignores case (TRUE -- the default) or not (FALSE)
#' @param tolower_colloc whether to lower case the retrieved collocates and the nodes (TRUE -- default) or not (FALSE)
#' @param save_results whether to output the collocates into a tab-separated plain text (TRUE) or not (FALSE -- default)
#' @param coll_output_name name of the file for the collocate tables
#' @param sent_output_name name of the file for the full sentence match containing the collocates
#' @return a list of two tibbles: (i) for collocates with sentence number of the match, window span information, and the corpus files, and (ii) full-sentences per match with sent number and corpus file
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
#' @examples
#' \dontrun{
#' colloc <- colloc_leipzig(corpus = corpus_files_path[2:3],
#'                               pattern = "\\bterelakkan\\b",
#'                               window = "b",
#'                               span = 3,
#'                               save_results = FALSE,
#'                               tolower_colloc = TRUE)
#' # Inspect outputs
#'
#' colloc$collocates
#'
#' colloc$sentence_matches
#' }
#' @export


#corpus = corpus_files_path[2:3]
#pattern = "\\bmembeli\\b"
#window = "r"
#span = 1
#case_insensitive = TRUE
#tolower_colloc = TRUE
#save_results = FALSE
#c = 1
#r = 1
#s = 1


colloc_leipzig <- function(corpus = "path to leipzig corpus",
                                    pattern = "regex",
                                    window = "b",
                                    span = 2,
                                    case_insensitive = TRUE,
                                    tolower_colloc = TRUE,
                                    save_results = FALSE,
                                    coll_output_name = "colloc_tidy_colloc_out.txt",
                                    sent_output_name = "colloc_tidy_sent_out.txt") {

  # preparing the window span
  if (window == "l") {
    span1 <- span
    span <- -span:0
    names(span) <- c(stringr::str_c("l", span1:1, sep = ""), "node")
  } else if (window == "r") {
    span1 <- span
    span <- 0:span
    names(span) <- c("node", stringr::str_c("r", 1:span1))
  } else {
    span1 <- span
    span <- -span:span
    names(span) <- c(stringr::str_c("l", span1:1, sep = ""), "node", stringr::str_c("r", 1:span1, sep = ""))
  }

  colloc_all <- tibble::tibble()
  sentmatch_all <- tibble::tibble()
  for (c in seq_along(corpus)) {
    # read in the corpus text
    corpora <- readr::read_lines(file = corpus[c])
    cat('"', basename(corpus[c]), '" ', "has been loaded!\n", sep = "")

    for (r in seq_along(pattern)) {
      # detect the search pattern and retrieve the citation with the match
      match.id <- stringr::str_which(corpora, stringr::regex(pattern[r], ignore_case = case_insensitive))
      subcorpus <- corpora[match.id]

      # detect if any matches found
      if (length(subcorpus) == 0) {

        cat("NO MATCH(ES) for the pattern you are searching for!\nTRY another corpus!\n\n")

      } else {

        cat("At least one match for the search pattern is detected in the corpus!\n\n")

        ## PREPARE THE MATCHED SENTENCES
        # detect the sentence number in which the match is found
        sent_id <- match.id

        # delete sentence number and put spaces around non-white-space characters
        subcorpus <- subcorpus %>%
          stringr::str_replace_all("^(\\d+?\\s)", "") %>%
          stringr::str_replace_all("(--)", " ") %>%
          stringr::str_replace_all("([^a-zA-Z0-9-\\s]+)", " \\1 ") %>%
          stringr::str_replace_all("\\s+", " ") %>%
          stringr::str_trim()

        # retrieve the corpus names
        corpus_name <- basename(corpus[c]) %>%
          stringr::str_replace('-sentences.*$', '')

        # get the number of matches of the search word found in the corpus
        match_length <- stringr::str_count(subcorpus, stringr::regex(pattern[r], ignore_case = case_insensitive))

        # replicate the sentences/string based on the number of matches found in the string
        sent.with.match <- rep(subcorpus, match_length)

        # replicate the sentence numbers/IDs based on the number of matches found in the string
        sent_id <- rep(sent_id, match_length)

        # get the starting and end position of the pattern
        # and store as a tibble
        position <- stringr::str_locate_all(subcorpus, stringr::regex(pattern[r], ignore_case = case_insensitive)) %>%
          purrr::map(tibble::as_tibble) %>%
          purrr::map_df(dplyr::bind_rows)

        ## RETRIEVE THE COLLOCATES
        all_colloc <- tibble()
        all_sentmatches <- tibble()
        p <- progress_estimated(n = length(sent.with.match))
        for (s in seq_along(sent.with.match)) {

          if (position[s,1]==1) {
            node <- stringr::str_sub(sent.with.match[s], position[s,1], position[s,2])
            right <- stringr::str_sub(sent.with.match[s], position[s,2]+1, nchar(sent.with.match[s]))
            sent.match.tagged <- stringr::str_c("NODEWORD", right, sep = "")
            sent.match.tagged_1 <- stringr::str_c("<node>", node, "</node>", right, sep = "")
          } else if (position[s,2] == nchar(sent.with.match[s])) {
            node <- stringr::str_sub(sent.with.match[s], position[s,1], position[s,2])
            left <- stringr::str_sub(sent.with.match[s], 1, position[s,1]-1)
            sent.match.tagged <- stringr::str_c(left, "NODEWORD", sep = "")
            sent.match.tagged_1 <- stringr::str_c(left, "<node>", node, "</node>", sep = "")
          } else if (position[s,1] != 1 & position[s,2] != nchar(sent.with.match[s])) {
            node <- stringr::str_sub(sent.with.match[s], position[s,1], position[s,2])
            right <- stringr::str_sub(sent.with.match[s], position[s,2]+1, nchar(sent.with.match[s]))
            left <- stringr::str_sub(sent.with.match[s], 1, position[s,1]-1)
            sent.match.tagged <- stringr::str_c(left, "NODEWORD", right, sep = "")
            sent.match.tagged_1 <- stringr::str_c(left, "<node>", node, "</node>", right, sep = "")
          } else if (position[s,1] == 1 & position[s,2] == nchar(sent.with.match[s])) {
            next
          }

          # get the vector position of every word-character in a sentence
          colloc <- sent.match.tagged %>%
            stringr::str_split(stringr::regex("[^a-z0-9-]+", ignore_case = T)) %>%
            unlist() %>%
            .[nzchar(.)] %>%
            purrr::set_names(nm = seq(length(.))) %>% # give names for each substracted word with their resulting 'vector position'
            tibble::tibble(string = sent.match.tagged_1, # store these words and their vector positions into a tibble
                           w = .,
                           vector.pos = as.numeric(names(.)))

          if (tolower_colloc == TRUE) {
            colloc <- colloc %>%
              dplyr::mutate(w = stringr::str_to_lower(w))
          } else {
            colloc <- colloc
          }
          rm(sent.match.tagged, sent.match.tagged_1)

          # calculate the collocate position of each words in relation to the node word
          colloc.vector.pos <- colloc[-1] %>%
            dplyr::filter(stringr::str_detect(w, stringr::regex("nodeword", ignore_case = TRUE))) %>% # get the vector position of the node
            .$vector.pos + span # get the span vector-position for the collocates
          colloc.vector.pos <- colloc.vector.pos[colloc.vector.pos<=dim(colloc)[1] & colloc.vector.pos > 0]

          if (all(names(colloc.vector.pos) == "node") == TRUE) {
            next
          } else {
            # retrive the collocates and left_join them with their span ID
            colloc <- colloc %>%
              dplyr::left_join(colloc.vector.pos %>%
                          tibble::tibble(wspan = names(.),
                                 vector.pos = .),
                        by = 'vector.pos') %>%
              dplyr::filter(!is.na(wspan)) %>%
              dplyr::mutate(corpus = corpus_name,
                     sent = sent_id[s]) %>%
              dplyr::select(w, wspan, corpus, sent, string) %>%
              dplyr::mutate(w = stringr::str_replace(w, "nodeword", node))
            #colloc <- colloc %>%
            # mutate(tag=if_else(wspan=='node',
            #                   stringr::str_c("<node>", w, "</node>", sep=""),
            #                  stringr::str_c("<coll span=", wspan, ">", w, "</coll>", sep="")))

            # tag the sentence match with the collocate id and create a separate dbase from the collocate list
            counterfulltext <- tibble::tibble(corpus = unique(colloc$corpus),
                                      sent.no = unique(colloc$sent),
                                      node = node,
                                      sent = sent.with.match[s])

            # gather all collocates
            all_colloc <- dplyr::bind_rows(all_colloc, colloc)
            rm(colloc)
            all_sentmatches <- dplyr::bind_rows(all_sentmatches, counterfulltext)
            p$pause(0.05)$tick()$print()
            if (s == length(sent.with.match)) {
              cat("\nDone gathering the collocates for each sentence!\n\n")
            }
          }
        } # end of "s" loop
      }
      colloc_all <- dplyr::bind_rows(colloc_all, all_colloc)
      sentmatch_all <- dplyr::bind_rows(sentmatch_all, all_sentmatches)
    } # end of "r" loop
  } # end of "c" loop
  if (save_results == TRUE) {
    readr::write_delim(x = colloc_all, path = coll_output_name, delim = "\t", append = TRUE)
    readr::write_delim(x = sentmatch_all, path = sent_output_name, delim="\t", append = TRUE)
    cat("Collocates table and sentence match of the node have been saved!\n\n")
  } else {
    out <- list(colloc_all, sentmatch_all)
    names(out) <- c("collocates", "sentence_matches")
    return(out)
  }
}



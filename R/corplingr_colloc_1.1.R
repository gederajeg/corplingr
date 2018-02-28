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
#' @importFrom readr write_delim
#' @importFrom readr write_lines
#' @importFrom purrr map_dbl
#' @importFrom purrr map_chr
#' @examples
#' Do not run!
#'
#' colloc <- colloc_leipzig_tidy(corpus = corpus_files_path[2:3],
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


colloc_leipzig_tidy <- function(corpus = "path to leipzig corpus",
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
    names(span) <- c(str_c("l", span1:1, sep = ""), "node")
  } else if (window == "r") {
    span1 <- span
    span <- 0:span
    names(span) <- c("node", str_c("r", 1:span1))
  } else {
    span1 <- span
    span <- -span:span
    names(span) <- c(str_c("l", span1:1, sep = ""), "node", str_c("r", 1:span1, sep = ""))
  }

  colloc_all <- tibble()
  sentmatch_all <- tibble()
  for (c in seq_along(corpus)) {
    # read in the corpus text
    corpora <- read_lines(file = corpus[c])
    cat('"', basename(corpus[c]), '" ', "has been loaded!\n", sep = "")

    for (r in seq_along(pattern)) {
      # detect the search pattern and retrieve the citation with the match
      subcorpus <- corpora %>%
        str_subset(regex(pattern[r], ignore_case = case_insensitive))

      # detect if any matches found
      if (length(subcorpus) == 0) {

        cat("NO MATCH(ES) for the pattern you are searching for!\nTRY another corpus!\n\n")

      } else {

        cat("At least one match for the search pattern is detected in the corpus!\n\n")

        ## PREPARE THE MATCHED SENTENCES
        # detect the sentence number in which the match is found
        sent_id <- subcorpus %>%
          str_extract("(^\\d+?(?=\\s))")

        # delete sentence number and put spaces around non-white-space characters
        subcorpus <- subcorpus %>%
          str_replace_all("^(\\d+?\\s)", "") %>%
          str_replace_all("(--)", " ") %>%
          str_replace_all("([^a-zA-Z0-9-\\s]+)", " \\1 ") %>%
          str_replace_all("\\s+", " ") %>%
          str_trim()

        # retrieve the corpus names
        corpus_name <- basename(corpus[c]) %>%
          str_replace('-sentences.*$', '')

        # get the number of matches of the search word found in the corpus
        match_length <- str_count(subcorpus, regex(pattern[r], ignore_case = case_insensitive))

        # replicate the sentences/string based on the number of matches found in the string
        sent.with.match <- rep(subcorpus, match_length)

        # replicate the sentence numbers/IDs based on the number of matches found in the string
        sent_id <- rep(sent_id, match_length)

        # get the starting and end position of the pattern
        # and store as a tibble
        position <- str_locate_all(subcorpus, regex(pattern[r], ignore_case = case_insensitive)) %>%
          map(as_tibble) %>%
          map_df(bind_rows)

        ## RETRIEVE THE COLLOCATES
        all_colloc <- tibble()
        all_sentmatches <- tibble()
        p <- progress_estimated(n = length(sent.with.match))
        for (s in seq_along(sent.with.match)) {

          if (position[s,1]==1) {
            node <- str_sub(sent.with.match[s], position[s,1], position[s,2])
            right <- str_sub(sent.with.match[s], position[s,2]+1, nchar(sent.with.match[s]))
            sent.match.tagged <- str_c("NODEWORD", right, sep = "")
            sent.match.tagged_1 <- str_c("<node>", node, "</node>", right, sep = "")
          } else if (position[s,2] == nchar(sent.with.match[s])) {
            node <- str_sub(sent.with.match[s], position[s,1], position[s,2])
            left <- str_sub(sent.with.match[s], 1, position[s,1]-1)
            sent.match.tagged <- str_c(left, "NODEWORD", sep = "")
            sent.match.tagged_1 <- str_c(left, "<node>", node, "</node>", sep = "")
          } else if (position[s,1] != 1 & position[s,2] != nchar(sent.with.match[s])) {
            node <- str_sub(sent.with.match[s], position[s,1], position[s,2])
            right <- str_sub(sent.with.match[s], position[s,2]+1, nchar(sent.with.match[s]))
            left <- str_sub(sent.with.match[s], 1, position[s,1]-1)
            sent.match.tagged <- str_c(left, "NODEWORD", right, sep = "")
            sent.match.tagged_1 <- str_c(left, "<node>", node, "</node>", right, sep = "")
          } else if (position[s,1] == 1 & position[s,2] == nchar(sent.with.match[s])) {
            next
          }

          # get the vector position of every word-character in a sentence
          colloc <- sent.match.tagged %>%
            str_split(regex("[^a-z0-9-]+", ignore_case = T)) %>%
            unlist() %>%
            .[nchar(.)!=0] %>%
            set_names(nm = seq(length(.))) %>% # give names for each substracted word with their resulting 'vector position'
            tibble(string = sent.match.tagged_1, # store these words and their vector positions into a tibble
                   w = .,
                   vector.pos = as.numeric(names(.)))

          if (tolower_colloc == TRUE) {
            colloc <- colloc %>%
              mutate(w = tolower(w))
          } else {
            colloc <- colloc
          }
          rm(sent.match.tagged, sent.match.tagged_1)

          # calculate the collocate position of each words in relation to the node word
          colloc.vector.pos <- colloc[-1] %>%
            filter(str_detect(w, regex("nodeword", ignore_case = TRUE))) %>% # get the vector position of the node
            .$vector.pos + span # get the span vector-position for the collocates
          colloc.vector.pos <- colloc.vector.pos[colloc.vector.pos<=dim(colloc)[1] & colloc.vector.pos > 0]

          if (all(names(colloc.vector.pos) == "node") == TRUE) {
            next
          } else {
            # retrive the collocates and left_join them with their span ID
            colloc <- colloc %>%
              left_join(colloc.vector.pos %>%
                          tibble(wspan = names(.),
                                 vector.pos = .),
                        by='vector.pos') %>%
              filter(!is.na(wspan)) %>%
              mutate(corpus = corpus_name,
                     sent = sent_id[s]) %>%
              select(w, vector.pos, wspan, corpus, sent, string) %>%
              mutate(w = str_replace(w, "nodeword", node))
            #colloc <- colloc %>%
            # mutate(tag=if_else(wspan=='node',
            #                   str_c("<node>", w, "</node>", sep=""),
            #                  str_c("<coll span=", wspan, ">", w, "</coll>", sep="")))

            # tag the sentence match with the collocate id and create a separate dbase from the collocate list
            counterfulltext <- tibble(corpus = unique(colloc$corpus),
                                      sent.no = unique(colloc$sent),
                                      node = node,
                                      sent = sent.with.match[s])

            # gather all collocates
            all_colloc <- bind_rows(all_colloc, colloc)
            rm(colloc)
            all_sentmatches <- bind_rows(all_sentmatches, counterfulltext)
            p$pause(0.1)$tick()$print()
            if (s == length(sent.with.match)) {
              cat("\nDone gathering the collocates for each sentence!\n\n")
            }
          }
        } # end of "s" loop
      }
      colloc_all <- bind_rows(colloc_all, all_colloc)
      sentmatch_all <- bind_rows(sentmatch_all, all_sentmatches)
    } # end of "r" loop
  } # end of "c" loop
  if (save_results == TRUE) {
    write_delim(x = colloc_all, path = coll_output_name, delim = "\t", append = TRUE)
    write_delim(x = sentmatch_all, path = sent_output_name, delim="\t", append = TRUE)
    cat("Collocates table and sentence match of the node have been saved!\n\n")
  } else {
    out <- list(colloc_all, sentmatch_all)
    names(out) <- c("collocates", "sentence_matches")
    return(out)
  }
}

#' Vectorised collocate extraction
#'
#' @importFrom stringr str_extract
#' @importFrom purrr map_int
#' @importFrom purrr is_empty

colloc_leipzig_vector <- function(corpus.vect.path = NULL, corpus.vect = NULL, pattern = NULL, case.insensitive = TRUE, window = "r", span) {
  waktu <- unlist(str_split(Sys.time(), "\\s"))
  hari <- waktu[1]
  jam1 <- waktu[2]
  if (is.null(corpus.vect) == TRUE & is.null(corpus.vect.path) == FALSE) {
    load(corpus.vect.path)
    corpus <- corpus.word.vector
    rm(corpus.word.vector)
    corpus.id <- str_extract(corpus.vect.path, "(?<=__)(.+?)(?=\\.RData)")
    cat(paste("\nDone loading the corpus vector: '", corpus.id, "'...\n", sep = ""))
  } else if (is.null(corpus.vect) == FALSE & is.null(corpus.vect.path) == TRUE) {
    cat("\nPlease specify the 'corpus.vect.path' argument with the name/id (NOT THE FILE PATH!!!) for the corpus vector!\n")
  } else if (is.null(corpus.vect) == FALSE & is.null(corpus.vect.path) == FALSE) {
    if (is.list(corpus.vect) == FALSE) {
      cat("\nYou need the corpus vector in the form of 'list' of tokenised sentences!\n")
    } else {
      corpus <- corpus.vect
      corpus.id <- corpus.vect.path
    }
  }

  if (window == "r") {
    collspan <- 1:span
    names(collspan) <- paste("r", collspan, sep = "")
  } else if (window == "l") {
    collspan <- -span:-1
    names(collspan) <- paste("l", span:1, sep = "")
  } else {
    l <- -span:-1
    names(l) <- paste("l", span:1, sep = "")
    r <- 1:span
    names(r) <- paste("r", 1:span, sep = "")
    collspan <- c(l, r)
  }
  cat("Gathering the pattern vector position...\n")
  match.vect.pos <- corpus %>%
    map(~str_which(., pattern = regex(pattern, ignore_case = case.insensitive)))

  cat("Counting the length of the match found...\n")
  match.length <- match.vect.pos %>%
    map_int(length) %>%
    .[.>0]
  match.vect.pos1 <- unname(unlist(match.vect.pos[names(match.length)]))
  rm(match.vect.pos)
  match.sent.num <- unlist(str_extract_all(names(match.length), "\\d+")) %>%
    as.numeric()
  cat("Replicating the sentence for the total number of match found in each sentence...\n")
  corpus1 <- rep(corpus[match.sent.num], match.length)
  rm(corpus, match.length)
  colloc.df <- tibble()
  prog <- round(summary(1:length(corpus1))[-4])
  for (i in seq_along(corpus1)) {
    if (i == prog[1]) {
      cat("We start gathering the collocates...\n")
    } else if (i == prog[2]) {
      cat("25%...\n")
    } else if (i == prog[3]) {
      cat("50%...\n")
    } else if (i == prog[4]) {
      cat("75%...\n")
    } else if (i == prog[5]) {
      cat("The last gathering phase!\n\n")
    }
    # collocates vector position
    collpos <- match.vect.pos1[i] + collspan
    collpos <- collpos[collpos <= length(corpus1[[i]]) & collpos > 0] # get only the valid collocate position
    if (is_empty(collpos) == TRUE) {
      #cat("No available collocates position for sentence no. ", i, "!\n", sep = "")
      next
    } else {
      # get the collocates
      colloc <- corpus1[[i]][collpos]
      colloc.df.temp <- tibble(w = colloc,
                               vector.pos = unname(collpos),
                               wspan = names(collpos),
                               corpus = corpus.id,
                               sent = as.numeric(str_extract(names(corpus1[i]), "\\d+")))
      colloc.df <- bind_rows(colloc.df, colloc.df.temp)
    }
  }
  waktu <- unlist(str_split(Sys.time(), "\\s"))
  hari <- waktu[1]
  jam2 <- waktu[2]
  cat(paste("\nStarting at: ", hari, " ", jam1, "...\n", sep = ""))
  cat(paste("Finished at: ", hari, " ", jam2, "...\n", sep = ""))
  return(colloc.df)
}

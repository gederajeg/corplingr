### window-based collocational retrieval function

#window = 5

#span <- -window:window

#span.label <- c(paste("L", window:1,sep=""), "node", paste("R", 1:window,sep=""))

#names(span) <- span.label

non_word_definition <- "([^a-zA-Z-]+|--)"

## OLD function to conduct window-based collocation in Leipzig Corpora----
## require tidy results of CONCORDANCE!
colloc_leipzig_tidy_old1 <- function(concordance=NULL, window = 5, non_word_definition="([^a-zA-Z-]+|--)") {

  # load stopwords
  stopwords1 <- "/Users/Primahadi/Downloads/stopwords_id.txt"
  stopwords2 <- "/Users/Primahadi/Google Drive/MONASH PhD Journey/1st Year (mid 2015-mid 2016)/analysis/collocation/stopword_id_new.txt"
  stopwords <- read_lines(stopwords1, skip = 2)
  stopwords <- c(stopwords, read_lines(stopwords2)) %>% unique()

  # non_word_pattern contains REGEX defining the non-word characters to split
  non_word_pattern <- non_word_definition

  # setting up the span
  span <- -window:window
  span.label <- c(paste("L", window:1,sep=""), "node", paste("R", 1:window,sep=""))
  names(span) <- span.label

  # split matched sentences into tidy data using tidytext's 'unnest_tokens'
  split_sent <- concordance %>%
    select(corpus, sent_id, node, node_sentences) %>%
    tidytext::unnest_tokens(collocates, node_sentences, token = 'regex', pattern = non_word_pattern) %>% # tokenise the sentences with NODEWORD marked for the matched search word
    #filter(sent_id != collocates) %>%
    split(.$sent_id) %>%
    map(~bind_cols(., tibble(pos = 1:nrow(.))))

  # length of nodeword match in a sentence
  nodeword_length <- split_sent %>%
    map(~filter(., collocates == 'nodeword')) %>%
    map(~select(., pos)) %>%
    map_dbl(~nrow(.))

  if (any(nodeword_length > 1) == TRUE) { # check if there is more than one nodeword in a sentence to split the collocation identification task
    cat("There is at least one sentence within which two search words are found!\n")

    split_sent2 <- split_sent[nodeword_length[nodeword_length > 1] %>%
                                names() %>%
                                c()] # retrieve sentences with more than one match word

    split_sent1 <- split_sent[nodeword_length[nodeword_length == 1] %>%
                                names() %>%
                                c()] # retrieve sentences with one match word

    # retrieve the position of the node and the node's corresponding collocational spans
    one_node_collspan <- split_sent1 %>%
      map(~filter(., collocates == 'nodeword')) %>%
      map(~select(., pos)) %>%
      map_dbl(~unlist(.)) %>%
      map(~.+span) %>% # determine the collocates sequence in each sentence
      map(~.[. > 0]) # retrieve only collocates sequence that is not negative integer

    # preparing list to store the collocates for single-node match in a sentence
    all_collocs_singlenode <- tibble()

    # for-loops to filter the collocates based on the designated collocates span from the node
    for (i in seq_along(split_sent1)) {

      counter_colloc <- split_sent1[[i]] %>%
        filter(pos %in% one_node_collspan[[i]])

      span_table <- one_node_collspan[[i]] %>%
        tibble(pos = ., span = names(.))

      counter_colloc <- counter_colloc %>%
        left_join(span_table, by = 'pos')

      all_collocs_singlenode <- bind_rows(all_collocs_singlenode, counter_colloc)
    }

    # preparing list to store the collocates for multi-node match in a sentence
    all_collocs_multinodes <- tibble()

    # retrieving collocational span index for each sentence with more than one nodeword
    # AND subset the collocates in each sentence based on the span index
    for (i in seq_along(split_sent2)) {

      for (j in 1:nodeword_length[nodeword_length > 1][i]) {

        coll_span <- split_sent2[[i]] %>%
          filter(collocates == 'nodeword') %>%
          select(pos) %>%
          .[j,] %>%
          map(~.+span) %>%
          map_df(~tibble(pos = ., span = names(.)))

        counter_colloc <- split_sent2[[i]] %>%
          filter(pos %in% coll_span$pos)

        counter_colloc <- counter_colloc %>%
          left_join(coll_span, by = 'pos')

        all_collocs_multinodes <- bind_rows(all_collocs_multinodes, counter_colloc)
      }
    }
    collocates_tidy <- bind_rows(all_collocs_singlenode, all_collocs_multinodes)

  } else {
    cat("The search word occurs only once in each sentence!\n")

    # code block for when there is only one nodeword in each retrieved sentence
    # retrieve the position of the node and the node's corresponding collocational spans
    one_node_collspan <- split_sent %>%
      map(~filter(., collocates == 'nodeword')) %>%
      map(~select(., pos)) %>%
      map_dbl(~unlist(.)) %>%
      map(~.+span) %>% # determine the collocates sequence in each sentence
      map(~.[. > 0]) # retrieve only collocates sequence that is not negative integer

    # preparing list to store the collocates
    all_collocs_singlenode <- tibble()

    # for-loops to filter the collocates based on the designated collocates span from the node
    for (i in seq_along(split_sent)) {

      counter_colloc <- split_sent[[i]] %>%
        filter(pos %in% one_node_collspan[[i]])

      span_table <- one_node_collspan[[i]] %>%
        tibble(pos = ., span = names(.))

      counter_colloc <- counter_colloc %>%
        left_join(span_table, by = 'pos')

      all_collocs_singlenode <- bind_rows(all_collocs_singlenode, counter_colloc)
    }

  }
}

#corpus_file_names=corpus_files[3:4]
#reg="(?<!-)\\b(?i)kemarahan\\b(?!-)"
#save.results=FALSE
#coll.output.name="anger_colloc.txt"
#sent.output.name="anger_colloc_sent_match.txt"


#c=2 # corpus indices
#r=1 # regex indes

## NEW function to create window-based collocates in Leipzig Corpora----
colloc_leipzig_tidy_old2 <- function(regex=NULL,
                                    corpus_file_names=NULL,
                                    window_span=4,
                                    save_results=FALSE,
                                    coll_output_name="colloc_tidy_colloc_out.txt",
                                    sent_output_name="colloc_tidy_sent_out.txt") {

  # preparing the window span
  window = window_span
  span <- -window:window
  span.label <- c(paste("L", window:1,sep=""), "node", paste("R", 1:window,sep=""))
  names(span) <- span.label

  for (c in seq_along(corpus_file_names)) {
    # read in the corpus text
    corpora <- read_lines(file = corpus_file_names[c])
    cat('"', basename(corpus_file_names[c]), '" ', "has been loaded!\n", sep = "")

    for (r in seq_along(regex)) {
      # detect the search pattern and retrieve the citation with the match
      subcorpus <- corpora %>%
        str_subset(regex[r])

      # detect if any matches found
      if (length(subcorpus) == 0) {

        cat("NO MATCH(ES) for the pattern you are searching for!\nTRY another corpus!\n\n")

      } else {

        cat("At least one match for the search pattern is detected in the corpus!\n\n")

        ## PREPARE THE MATCHED SENTENCES
        # detect the sentence number in which the match is found
        sent_id <- subcorpus %>%
          str_extract("(^\\d+?(?=\\s))")

        # delete sentence number
        subcorpus <- subcorpus %>%
          str_replace_all("^(\\d+?\\s)", "") %>%
          str_replace_all("(--)", " ") %>%
          str_replace_all("([^a-zA-Z0-9-\\s]+)", " \\1 ") %>%
          str_replace_all("\\s+", " ") %>%
          str_trim()

        # retrieve the corpus names
        corpus_id <- basename(corpus_file_names[c]) %>%
          str_replace('-sentences.*$', '')

        # get the number of matches of the search word found in the corpus
        match_length <- str_count(subcorpus, regex[r])

        # replicate the sentences/string based on the number of matches found in the string
        sent.with.match <- rep(subcorpus, match_length)

        # replicate the sentence numbers/IDs based on the number of matches found in the string
        sent_id <- rep(sent_id, match_length)

        # get the starting and end position of the pattern
        # and store as a tibble
        position <- str_locate_all(subcorpus, regex[r]) %>%
          map(as_tibble) %>%
          map_df(bind_rows)

        ## RETRIEVE THE COLLOCATES
        all_colloc <- tibble()
        all_sentmatches <- tibble()
        for (s in seq_along(sent.with.match)) {

          # get the starting position of every word-character in a sentence
          pos <- sent.with.match[s] %>%
            str_locate_all("\\b([a-zA-Z0-9-]+)\\b") %>% # get the position
            map(as_tibble) %>% # turn into a tibble
            map_df(bind_rows) %>% # combine these character-positions tibble
            mutate(string=sent.with.match[s]) %>% # add the sentence match
            select(string, everything())

          # extract all the words in the sentence by extracting them using their starting and end character position
          w <- pos %>%
            pmap_chr(str_sub) %>% # substract each word
            setNames(1:length(.)) %>% # give names for each substracted word with their resulting 'vector position'
            tibble(vector.pos=as.numeric(names(.)), w=.) # store these words and their vector positions into a tibble

          # combine the words with the tibble of their character position in 'pos'
          colloc <- bind_cols(pos, w)
          rm(pos, w)

          # calculate the collocate position of each words in relation to the node word
          colloc.vector.pos <- colloc[-1] %>%
            filter(position$start[s]==colloc$start & position$end[s]==colloc$end) %>% # get the vector position of the node
            .$vector.pos + span # get the span vector-position for the collocates

          # retrive the collocates and left_join them with their span ID
          colloc <- colloc %>%
            left_join(colloc.vector.pos %>%
                        tibble(wspan=names(.),
                               vector.pos=.),
                      by='vector.pos') %>%
            filter(!is.na(wspan)) %>%
            select(-string) %>%
            mutate(corpus=corpus_id,
                   sent=sent_id[s])
          #colloc <- colloc %>%
          # mutate(tag=if_else(wspan=='node',
          #                   str_c("<node>", w, "</node>", sep=""),
          #                  str_c("<coll span=", wspan, ">", w, "</coll>", sep="")))

          # tag the sentence match with the collocate id and create a separate dbase from the collocate list
          counterfulltext <- tibble(corpus=unique(colloc$corpus),
                                    sent.no=unique(colloc$sent),
                                    node=colloc$w[colloc$wspan=="node"],
                                    sent=sent.with.match[s])

          # gather all collocates
          all_colloc <- bind_rows(all_colloc, colloc)
          rm(colloc)
          all_sentmatches <- bind_rows(all_sentmatches, counterfulltext)
          if (s==length(sent.with.match)) {
            cat("Done gathering the collocates for each sentence.\nNow tagging the collocates in the sentence match!\n")
          }
        } # end of "s" loop
        if (save_results==TRUE) {
          write_delim(x=all_colloc, path=coll_output_name, delim="\t", append=TRUE)
          write_delim(x=all_sentmatches, path=sent_output_name, delim="\t", append=TRUE)
          cat("Collocates table and sentence match of the node have been saved!\n\n")
        }
        #cat("Done with the tagging!\n")
      }
    } # end of "r" loop
  } # end of "c" loop
}



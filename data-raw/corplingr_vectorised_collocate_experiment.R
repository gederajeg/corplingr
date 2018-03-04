#' Vectorised collocate extraction
#'
#' @param corpus.vect.path filepath to \code{.RData} containing tokenised Leipzig Corpora saved as list of character vectors.
#' @param corpus.vect
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
    cat("\nPlease specify the 'corpus.vect.path' argument with the name/id for the corpus vector (NOT THE FILE PATH!!!).\n")
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

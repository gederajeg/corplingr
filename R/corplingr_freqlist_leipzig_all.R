#' Frequency list of all words in a Leipzig Corpus file
#'
#' @description The function generates a frequency list of all word-tokens in a single Leipzig Corpus file.
#'     While users can input all filepath to all corpus files, for memory-efficiency, it is recommended that each file is processed in separate function-call.
#'     If it is decided to process all corpus files, the functions output a List with as many elements as the number of the input filepath.
#' @param split_regex user-defined regular expressions to tokenise the corpus.
#' @param leipzig_path full filepath to one or more of the Leipzig Corpus file(s).
#' @param case_insensitive logical; ignoring (\code{TRUE}) or maintaining (\code{FALSE}) the case when splitting the corpus into word token.
#' @return A tibble of frequency list in descending order of the frequency.
#' @examples
#' \dontrun{
#' wlist_all <- freqlist_leipzig_all(split_regex = "([^a-zA-Z0-9-]+|--)",
#'                                   leipzig_path = leipzig_corpus_path[1])
#' }
#' @importFrom rlang sym
#' @importFrom rlang :=
#' @importFrom stringr str_split
#' @importFrom stringr regex
#' @importFrom readr read_lines
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @importFrom dplyr count
#' @importFrom utils menu
#' @export

freqlist_leipzig_all <- function(split_regex = "([^a-zA-Z0-9-]+|--)",
                                 leipzig_path = NULL,
                                 case_insensitive = TRUE) {
  if (length(leipzig_path) > 1) {
    message(paste("You chose to generate frequency list for all words across ", length(leipzig_path), " corpus files!\n", sep = ""))
    #choice <- utils::menu(choices = c("CONTINUE", "EXIT"),
    #               title = "If you wish to continue, type-in '1' into the console (without the quote!); otherwise, type-in '2'.")
    #if (choice == 1) {
      freqlist_all <- vector(mode = "list", length = length(leipzig_path))
      names(freqlist_all) <- stringr::str_replace(basename(leipzig_path), "-sentences\\..+$", "")
    #} else {
      #messages <- "You decided to exit the operation!"
      #return(print(messages))
    #}

  } else if (length(leipzig_path) == 1) {
    freqlist <- tibble::tibble()
  }
  for (i in seq_along(leipzig_path)) {
    cat("Reading-in the corpus file...\n")
    corpora <- readr::read_lines(file = leipzig_path[i])
    cat('"', basename(leipzig_path[i]), '" ', "has been loaded!\n", sep = "")
    cat('Tokenising the corpus into word-tokens...\n')
    wtoken <- stringr::str_split(corpora,
                                 stringr::regex(pattern = split_regex,
                                                ignore_case = case_insensitive))
    wtoken <- unlist(wtoken)
    wtoken <- wtoken[nzchar(wtoken)]
    wtoken <- stringr::str_to_lower(wtoken)
    word_q <- rlang::sym(sprintf("word"))
    cat('Generating the frequency list...\n')
    freqlist <- tibble::tibble(!!word_q := wtoken)
    freqlist <- dplyr::count(freqlist, !!word_q, sort = TRUE)
    if (length(leipzig_path) == 1) {
      return(freqlist)
      cat('Done!\n')
    } else if (length(leipzig_path) > 1) {
      #freqlist_all[[i]] <- list(freqlist)
      freqlist_all[[i]] <- freqlist
      cat(paste("Done with corpus no. ", i, "!\n\n", sep = ""))
    }
  }
  return(freqlist_all)
}

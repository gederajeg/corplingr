#' Generate word ngram from Leipzig sentence-corpora
#'
#' @description The function generates word ngram from input vector of texts. See \bold{Details}.
#' @param corpus_vector sentence vector or any other character vectors.
#' @param ngram_length the length of the ngram as a single integer vector (e.g., 3, 4, etc.).
#' @param regex_split characters to be split from the input vectors. The default is \code{"[^a-z-]+"}, i.e. all non alphabetical characters and hypen (to maintain reduplication, etc.).
#' @param case_insensitive whether to ignore case when splitting the vectors (\code{TRUE} -- default) or not (\code{FALSE}).
#' @param sample_n the number of sentence lines to be used for generating the ngram. The default is \code{NULL}, thus using all the elements in \code{corpus_vector} argument. If specified, used non-negative numbers and the ngram will be generated from the sampled elements.
#' @details The function requires input-vector whose each element corresponds to one sentence or not (i.e., where one sentence is in fact split as two vector-elements).
#'     The source text can be from the sentence-based corpus like Leipzig Corpora, other text files where each line corresponds to sentence, and any other files that do not corresponds to the former.
#'     If the input is clearly consists of one-sentence per vector-element, it is suggested to append "SENT/SENTENCE" string in the beginning and end of the element; the script will include identifying such string and removing them when part of the ngram, indicating an ngram crossing the sentence boundary.
#'     This is important because, for instance, the sentences in the Leipzig Corpora are not related to one another (i.e., randomised).
#'     Based on this input, the function then splits the sentences into word tokens and generates the ngrams from these word vectors.
#' @return a list of two elements: (A) tibble consisting of (i) \code{ngram} and (ii) \code{n} (token frequencies); (B) tibble of wordlist from the \code{corpus_vector} vector.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_split
#' @importFrom stringr regex
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom rlang sym
#' @examples
#' \dontrun{
#' # load the corplingr package
#' devtools::load_all("~/Documents/_my_r_packages/corplingr")
#'
#' # load the .RData for the cleaned sentence corpora
#' load(corpus_cleaned_path[1])
#'
#' # if it is a sentence-per-line input-vector, mark the element with "SENT"
#' sentence_cleaned <- paste("SENT", sentence_cleaned, "SENT", sep = " ")
#'
#' # generate the ngram
#' ngram_out <- ngram_word_leipzig(corpus_vector = sentence_cleaned,
#'                                 ngram_length = 2,
#'                                 regex_split = "[^a-z]+",
#'                                 case_insensitive = TRUE,
#'                                 sample_n = 2000)
#'
#' }

ngram_word_leipzig <- function(corpus_vector, ngram_length = 2, regex_split = "[^a-z-]+", case_insensitive = TRUE, sample_n = NULL) {

  if (!is.null(sample_n)) {
    cat("You chose to take a random sample of", sample_n, "sentences to generate the ngram!\n")
    textfile <- corpus_vector[sample(1:length(corpus_vector), sample_n)]
  } else {
    cat("No sampling is conducted for the sentences!\n")
    textfile <- corpus_vector
  }
  textfile <- stringr::str_to_lower(textfile)
  cat("Tokenising the corpus...\n")
  textvect <- stringr::str_split(textfile, stringr::regex(regex_split, ignore_case = case_insensitive))
  textvect <- unlist(textvect)
  textvect <- textvect[nzchar(textvect)]
  #corpus.size <- length(textvect[!str_detect(textvect, "^SENT$")])
  cat("Generating the ngram sequences...\n")

  # Gries (2017)
  ngram_mtx <- mapply(seq,
                      1:(length(textvect) - (ngram_length - 1)),
                      ngram_length:length(textvect))
  # Gries (2017)
  cat("Gathering the ngram...\n")
  ngram <- apply(ngram_mtx, 2, function(items) paste(textvect[items], collapse = " "))

  cat("Preparing for output...\n")
  ngram <- tibble::tibble(ngram) %>%
    dplyr::filter(!stringr::str_detect(ngram, "\\b(?i)sent\\b")) %>%
    dplyr::count(ngram, sort = TRUE)
  ngram_q <- rlang::sym(sprintf("ngram"))
  ngram <- tidyr::separate(data = ngram,
                           col = !!ngram_q,
                           into = paste("w", as.character(seq(ngram_length)), sep=""),
                           sep = " ",
                           remove = FALSE)
  words <- textvect[!str_detect(textvect, "^(?i)SENT$")]
  wlist <- tibble::tibble(words) %>%
    dplyr::count(!!quo(words), sort = TRUE)

  output <- list(ngram = ngram, wlist = wlist)
  cat("Done!\n")
  return(output)
}

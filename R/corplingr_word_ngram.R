#' Generate word ngram from Leipzig sentence-corpora
#'
#' @description The function generates word ngram from input vector of texts. The function requires input-vector of sentences from the Leipzig Corpora as its elements. The sentences should begin and end with "SENT" element so as user can identify and remove ngram that cross the sentence boundary. This is important because the sentences in the Leipzig Corpora are not related to one another (i.e., randomised). The sentences are split into word tokens and the script generates the ngrams from these word vectors.
#' @param input sentence vector or any other character vectors
#' @param gram_length the length of the ngram as a single numeric vector (e.g., 3, 4, etc.)
#' @param split characters to be split from the input vectors. The default is \code{"[^a-z-]+"}, i.e. all non alphabetical characters and hypen (to maintain reduplication, etc.).
#' @param case_insensitive whether to ignore case when splitting the vectors (\code{TRUE} -- default) or not (\code{FALSE}).
#' @param sample_n the number of sentence lines to be used for generating the ngram. The default is \code{NULL}, thus using all the elements in \code{input} argument. If specified, used non-negative numbers and the ngram will be generated from the sampled elements.
#' @return a list of two elements: (A) tibble consisting of (i) \code{ngram} and (ii) \code{n} (token frequencies); (B) tibble of wordlist from the \code{input} vector.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_split
#' @importFrom stringr regex
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom tidyr separate
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
#' ngram_out <- ngram_word_leipzig(input = sentence_cleaned,
#'                                 gram_length = 2, split = "[^a-z]+",
#'                                 case_insensitive = TRUE,
#'                                 sample_n = NULL)
#'
#' }
#'


ngram_word_leipzig <- function(input, gram_length = 2, split = "[^a-z-]+", case_insensitive = TRUE, sample_n = NULL) {

  if (!is.null(sample_n)) {
    textfile <- input[sample(1:length(input), sample_n)]
  } else {
    textfile <- input
  }
    textfile <- str_to_lower(textfile)
    textvect <- str_split(textfile, regex(split, ignore.case = case_insensitive))
    textvect <- unlist(textvect)
    textvect <- textvect[nzchar(textvect)]
    #corpus.size <- length(textvect[!str_detect(textvect, "^SENT$")])
    ngram.mtx <- mapply(seq,
                        1:(length(textvect) - (gram_length - 1)),
                        gram_length:length(textvect))
    ngram <- apply(ngram.mtx, 2, function(items) paste(textvect[items], collapse = " "))
    ngram <- tibble(ngram) %>%
      filter(!str_detect(ngram, "\\b(?i)sent\\b")) %>%
      count(ngram, sort = TRUE)
    ngram <- separate(ngram, ngram, paste("w", as.character(seq(gram_length)), sep=""), " ", F)
    wlist <- tibble(words = textvect[!str_detect(textvect, "^(?i)SENT$")]) %>%
      count(words, sort = TRUE)

    output <- list(ngram = ngram, wlist = wlist)
    return(output)
}

#' Combine the ngram output with the frequency list of the elements of the ngram
#'
#' @description Left join the frequency list for each element making up the ngram
#' @param df.x ngram table output by \code{\link{ngram_word_leipzig}}.
#' @param df.y word list table output by \code{\link{ngram_word_leipzig}}.
#' @param ... capture the word elements of the ngram (e.g., \code{w1, w2, w3, etc.}).
#' @return a joint tibble for the ngram and frequency list for each component word of the ngram
#' @importFrom dplyr funs
#' @importFrom dplyr quo
#' @importFrom dplyr quos
#' @importFrom dplyr quo_name
#' @importFrom dplyr left_join
#' @importFrom dplyr rename_if
#' @importFrom dplyr %>%
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
#' ngramout <- ngram_word_leipzig(sentence_cleaned, sample_n = 10000)
#' ngram <- ngramout$ngram
#' wlist <- ngramout$wlist
#'
#' # joint the frequency list
#' df <- joint_freq(ngram, wlist, w1, w2)
#' df
#' }


joint_freq <- function(df.x, df.y, ...) {

  quo_by <- quos(...)
  for (i in seq_along(quo_by)) {
    quo_by_name <- quo_name(quo_by[[i]])
    df.y <- rename_if(df.y, is.character, funs(function(.) !!quo_by_name))
    df.y <- rename_if(df.y, is.numeric, funs(function(.) paste("n_", !!quo_by_name, sep = "")))
    df.x <- df.x %>% left_join(df.y, by = quo_by_name)
  }
  return(df.x)
}

#' Association measure for the 2-word ngram
#'
#' @description The function performs association measure (i.e. Collostruction Strength) and odds ratio for each bigram using the embedded \code{\link[stats]{fisher.test}} from the base R.
#' @param df data frame from the output of \code{\link{ngram_word_leipzig}} and \code{\link{joint_freq}}.
#' @param min_ngram minimun token frequency of the 2-word ngram to be included in the computation. The default is \code{10}
#' @param two_tailed two-sided test for the Fisher Exact (\code{TRUE} -- the default) or one-sided (\code{FALSE}).
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr filter_at
#' @importFrom dplyr vars
#' @importFrom dplyr all_vars
#' @importFrom dplyr if_else
#'
ngram2_assoc <- function(df, min_ngram = 10, two_tailed = TRUE) {

  data.all <- df
  data.all <- data.all %>%
    rename(a = n) %>%
    filter(a > min_ngram)
  data.all <- data.all %>%
    mutate(corpus_size = sum(a)) %>%
    mutate(c = as.numeric(n_w2 - a),
           b = as.numeric(n_w1 - a),
           d = as.numeric(corpus_size - (a + b + c)))
  data.all <- data.all %>%
    filter_at(vars(c, b, d), all_vars(. > 0)) %>%
    mutate(a_exp = (as.numeric(n_w1) * as.numeric(n_w2))/corpus_size)
  data.all <- data.all %>%
    mutate(pfye = fye2(a, b, c, d, a_exp, two.sided = two_tailed, "p"),
           pholm = p.adjust(pfye, method = "holm"),
           oddsr = fye2(a, b, c, d, a_exp, two.sided = two_tailed, "or"),
           collstr = if_else(a > a_exp, round(-log10(pfye), digits = 3), round(log10(pfye), digits = 3)),
           dp.w1.cue.w2 = round((a/(a + b) - c/(c + d)), 3),
           dp.w2.cue.w1 = round((a/(a + c) - b/(b + d)), 3),
           assoc = if_else(a > a_exp, "attr", "rep"),
           assoc = replace(assoc, !a > a_exp & !a < a_exp, "chance"),
           dec = if_else(pholm < 0.1, "ms", "ns"),
           dec = if_else(pholm < 0.05, "*", dec),
           dec = if_else(pholm < 0.01, "**", dec),
           dec = if_else(pholm < 0.001, "***", dec))
  return(data.all)
}

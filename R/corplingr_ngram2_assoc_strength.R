#' Association measure for the 2-word ngram
#'
#' @description The function performs association measure (i.e. Collostruction Strength) and odds ratio for each bigram using the embedded \code{\link[stats]{fisher.test}} from the base R.
#' @param df data frame from the output of \code{\link{ngram_word_jointfreq}}.
#' @param min_ngram_token minimun token frequency of the 2-word ngram to be included in the computation. The default is \code{10}
#' @param two_tailed two-sided test for the Fisher Exact (\code{TRUE} -- the default) or one-sided (\code{FALSE}).
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr filter_at
#' @importFrom dplyr vars
#' @importFrom dplyr all_vars
#' @importFrom dplyr if_else
#' @importFrom stats p.adjust
#' @importFrom dplyr quo
#' @importFrom dplyr quo_name
#' @importFrom rlang :=
#' @examples
#' \dontrun{
#' df_am <- ngram2_assoc_strength(df = df, min_ngram_token = 10, two_tailed = TRUE)
#' }
ngram2_assoc_strength <- function(df, min_ngram_token = 10, two_tailed = TRUE) {

  data_all <- df
  colnames(data_all)[grep("^n$", colnames(data_all), perl = T)] <- "a"
  data_all <- data_all[data_all$a > min_ngram_token, ]

  # get the freq of corpus_size, a, b, c, and d cells
  data_all$corpus_size <- sum(data_all$a)
  data_all$c <- as.numeric(data_all$n_w2 - data_all$a)
  data_all$b <- as.numeric(data_all$n_w1 - data_all$a)
  data_all$d <- as.numeric(data_all$corpus_size - (data_all$a + data_all$b + data_all$c))

  # get the expected frequency for cell "a"
  data_all <- data_all[data_all$b > 0 & data_all$c > 0 & data_all$d > 0, ]
  data_all$a_exp <- (as.numeric(data_all$n_w1) * as.numeric(data_all$n_w2))/data_all$corpus_size

  data_all$p_fye <- fye2(data_all$a, data_all$b, data_all$c, data_all$d, data_all$a_exp, two_sided = two_tailed, res = "p")
  data_all$p_holm <- stats::p.adjust(data_all$p_fye, method = "holm")
  data_all$collstr <- dplyr::if_else(data_all$a > data_all$a_exp,
                                     round(-log10(data_all$p_fye), digits = 3),
                                     round(log10(data_all$p_fye), digits = 3))
  data_all$dp_w1_cue_w2 <- round((data_all$a/(data_all$a + data_all$b) - data_all$c/(data_all$c + data_all$d)), 3)
  data_all$dp_w2_cue_w1 <- round((data_all$a/(data_all$a + data_all$c) - data_all$b/(data_all$b + data_all$d)), 3)
  data_all$assoc <- dplyr::if_else(data_all$a > data_all$a_exp, "attr", "rep")
  data_all$assoc <- replace(data_all$assoc, !data_all$a > data_all$a_exp & !data_all$a < data_all$a_exp, "chance")
  data_all$dec <- dplyr::if_else(data_all$p_holm < 0.1, "ms", "ns")
  data_all$dec <- replace(data_all$dec, data_all$p_holm < 0.05, "*")
  data_all$dec <- replace(data_all$dec, data_all$p_holm < 0.01, "**")
  data_all$dec <- replace(data_all$dec, data_all$p_holm < 0.001, "***")
  return(data_all)
}

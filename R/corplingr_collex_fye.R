#' Iterative \emph{Fisher-Yates Exact} test for collexeme/collocational analysis
#'
#' @description This is a vectorised wrapper for the \code{\link[stats]{dhyper}} function in the \code{stats} package.
#'     The implementation of the code is adapted from Gries (2012).
#'     \code{collex_fye} also provides a logical argument (i.e., \code{two_sided}) whose value is passed to the \code{alternative} argument of the embedded \code{\link[stats]{fisher.test}} if \code{two_sided} is \code{TRUE}.
#' @param a cell \code{a} in a 2-by-2 crosstabulation matrix (viz. representing the co-occurrence tokens of the levels of the variables. For instance, word-word co-occurrences, or word-construction co-occurrences).
#' @param a_exp expected frequency for cell \code{a} in the 2-by-2 crosstabulation matrix
#' @param n_w_in_corp the total frequency of the collexemes/collocates of the target construction/node word in the corpus.
#' @param corpus_size the total size (in word tokens) of the corpus.
#' @param n_pattern the total frequency of occurrence of the target construction/node word in the corpus.
#' @param two_sided logical; whether to perform one-sided test (\code{FALSE} -- Default) or two-sided (\code{TRUE}).
#' @param collstr_res logical; whether output the FYE \emph{p}-value as the Collostruction Strength value (\code{TRUE} -- the default) or just report the \emph{p}-value (\code{FALSE}).
#' @param float the floating digits of the Collostruction/Collocation Strength. The default value is \code{3}.
#' @return Numeric vector of the same length as \code{a} interpreted as the Collostruction Strength of the construction/node word with the collexemes/collocates.
#'     Collostruction Strength is (i) the negative logarithm to the base of ten of the Fisher-Yates Exact test \emph{p}-value when \code{a} > \code{a_exp}, and (ii) the positive logarithm when \code{a} <= \code{a_exp}.
#' @examples
#' \dontrun{
#' # do the collocate search using "corpus_path" input-option
#' library(tidyverse)
#' df <- colloc_default(corpus_path = orti_bali_path,
#'                      pattern = "^nuju$",
#'                      window = "b", # focusing on both left and right context window
#'                      span = 3) # retrieve 3 collocates to the left and right of the node
#' # prepare the collexeme analysis input tibble
#' # and select to focus on R1 and R2 collocates.
#' collex_tb <- collex_prepare(df, span = c("r1", "r2"))
#'
#' # run the Fisher-Yates Exact (FYE) Test in vectorised fashion with the help of purrr's pmap
#' # the example below runs the one-tailed FYE and output the p-value in log10 of CollStr value
#' collex_tb <- mutate(collex_tb,
#'                     collstr = purrr::pmap_dbl(list(a, a_exp, n_w_in_corp, corpus_size, n_pattern),
#'                                    collex_fye, two_sided = FALSE, collstr_res = TRUE))
#' # preview the results
#' collex_tb
#' }
#' @importFrom stats end
#' @importFrom stats p.adjust
#' @importFrom stats start
#' @importFrom stats fisher.test
#' @importFrom stats dhyper
#' @export
collex_fye <- function(a = "frequency of co-occurrence of the collocate and the node",
                       a_exp = "expected frequency",
                       n_w_in_corp = "total frequency of collexemes/collocates in the whole corpus",
                       corpus_size = "total size of the corpus",
                       n_pattern = "total frequency of the construction/node word in the whole corpus",
                       two_sided = FALSE,
                       collstr_res = TRUE,
                       float = 3) {

  if (two_sided == FALSE) {

    if (a > a_exp) {

      pfye <- sum(stats::dhyper(a:n_pattern, n_pattern, (corpus_size - n_pattern), n_w_in_corp))

    } else {

      pfye <- sum(stats::dhyper(0:a, n_pattern, (corpus_size - n_pattern), n_w_in_corp))

    }

  } else {

    # generate the remaining cells if two-sided test is chosen
    # because the two-sided test uses fisher.test that requires 2-by-2 input matrix
    b <- n_w_in_corp - a
    c <- n_pattern - a
    d <- corpus_size - (a + b + c)

    cross_table <- rbind(c(a, b), c(c, d)) # create a 2-by-2 matrix for FYE input

    pfye <- stats::fisher.test(cross_table, alternative = "two.sided")$p.value

  }

  if (collstr_res == TRUE) {
    if (a > a_exp) {
      collstr <- round(-log10(pfye), digits = float)
    } else {
      collstr <- round(log10(pfye), digits = float)
    }

    return(collstr)
  } else {

    return(pfye)

  }

}

#' Iterative \emph{Fisher-Yates Exact} test for collexeme/collocational analysis
#'
#' @description This is a vectorised wrapper for the \code{\link[stats]{dhyper}} function in the \code{stats} package.
#'     The implementation of the code is adapted from Gries (2012).
#'     \code{collex_fye} also provides a logical argument (i.e., \code{two_sided}) whose value is passed to the \code{alternative} argument of the embedded \code{\link[stats]{fisher.test}} if \code{two_sided} is \code{TRUE}.
#' @param a cell \code{a} in a 2-by-2 crosstabulation matrix (viz. representing the co-occurrence tokens of the levels of the variables. For instance, word-word co-occurrences, or word-construction co-occurrences).
#' @param n_corpus the total size (in word tokens) of the corpus.
#' @param n_coll the total frequency of the collexemes/collocates of the target construction/node word in the corpus.
#' @param n_cxn the total frequency of occurrence of the target construction/node word in the corpus.
#' @param two_sided logical; whether to perform one-sided test (\code{FALSE} -- Default) or two-sided (\code{TRUE}).
#' @param collstr_res logical; whether output the FYE \emph{p}-value as the Collostruction Strength value (\code{TRUE} -- the default) or just report the \emph{p}-value (\code{FALSE}).
#' @param float the floating digits of the Collostruction Strength. The default is \code{3}.
#' @param mpfr_precision a number indicating the maximal precision to be used in \bold{bits}. This is passed to the \code{precBits} argument of \code{\link[Rmpfr]{mpfr}}.
#' @return Numeric vector of the same length as \code{a} interpreted as the Collostruction Strength of the construction/node word with the collexemes/collocates.
#'     Collostruction Strength is (i) the negative logarithm to the base of ten of the Fisher-Yates Exact test \emph{p}-value when \code{a} > \code{a_exp}, and (ii) the positive logarithm when \code{a} <= \code{a_exp}.
#' @importFrom stats end
#' @importFrom stats p.adjust
#' @importFrom stats start
#' @importFrom stats fisher.test
#' @importFrom stats dhyper
#' @importFrom dplyr progress_estimated
#' @importFrom Rmpfr mpfr
#' @importFrom Rmpfr asNumeric
#' @export
collex_fye <- function(a,
                 n_corpus = "total size of the corpus",
                 n_coll = "total frequency of collexemes/collocates in the corpus as a whole",
                 n_cxn = "total frequency of the construction/node word in the corpus as a whole",
                 two_sided = FALSE,
                 collstr_res = TRUE,
                 float = 3,
                 mpfr_precision = 500) {

  # generate results collector
  results <- vector(mode = "numeric", length = length(a))

  # generate progress bar estimates
  p <- dplyr::progress_estimated(length(a))

  # generate the remaining cells if two-sided test is chosen
  # because the two-sided test uses fisher.test that requires 2-by-2 input matrix
  b <- n_coll - a
  c <- n_cxn - a
  d <- n_corpus - (a + b + c)

  # generate the expected co-occurrence frequencies
  # a_exp <- (a + b) * (a + c)/(a + b + c + d)
  a_exp <- Rmpfr::asNumeric(Rmpfr::mpfr((a + b), mpfr_precision)) * Rmpfr::asNumeric(Rmpfr::mpfr((a + c), mpfr_precision))/(a + b + c + d)

  for (i in seq_along(a)) {

    # print progress bar
    p$pause(0.1)$tick()$print()
    #cat(i, "\n")

    if (two_sided == FALSE) { # perform one-tailed FYE test

      if (a[i] > a_exp[i]) { # check if the obs.freq is more than exp.freq; if yes, take 'greater' alternative in the FYE
        # output <- sum(stats::dhyper(a[i]:n_cxn[i], n_cxn[i], (n_corpus - n_cxn), n_coll[i]))
        output <- Rmpfr::asNumeric(sum(Rmpfr::mpfr(stats::dhyper(a[i]:n_cxn[i], n_cxn[i], (n_corpus - n_cxn), n_coll[i]), mpfr_precision)))
      } else { # check if the obs.freq is less than exp.freq; if yes, take 'less' alternative in the FYE
        # output <- sum(stats::dhyper(0:a[i], n_cxn[i], (n_corpus - n_cxn), n_coll[i])) # one-tailed FYE for less-than-exp obs.freq
        output <- Rmpfr::asNumeric(sum(Rmpfr::mpfr(stats::dhyper(0:a[i], n_cxn[i], (n_corpus - n_cxn), n_coll[i]), mpfr_precision)))
      }
      if (collstr_res == TRUE) {
        output <- round(-log10(output), digits = float)
      }

    } else { # perform two-tailed FYE test
      cross_table <- rbind(c(a[i], b[i]), c(c[i], d[i])) # create a 2-by-2 matrix for FYE input
      # output <- stats::fisher.test(cross_table, alternative = "two.sided")$p.value
      output <- Rmpfr::asNumeric(Rmpfr::mpfr(stats::fisher.test(cross_table, alternative = "two.sided")$p.value, mpfr_precision))

      if (collstr_res == TRUE) {
        if (a[i] > a_exp[i]) {
          output <- round(-log10(output), digits = float)
        } else {
          output <- round(log10(output), digits = float)
        }
      }
    }
    results[i] <- output
  }
  return(results)
}

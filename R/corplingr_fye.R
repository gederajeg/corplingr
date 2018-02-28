#' Fisher Yates Exact test
#'
#' @description This is a vectorised wrapper for \code{fisher.test()} in the \code{stats} package. The implementation of the code is slightly adapted from Levshina's (2015) \code{pv.Fisher.collstr()}. \code{fye2()} adds the argument for values in the \code{alternative} argument in the \code{fisher.test()}.
#' @param a cell \code{a} in a 2-by-2 crosstabulation matrix (viz. representing the co-occurrence tokens of the levels of the variables. For instance, word-word co-occurrences, or word-construction co-occurrences).
#' @param b cell \code{b} in the matrix (it represents alternative items for the co-occurrence of the first row and the second column. For instance, alternative construction, or near-synonyms).
#' @param c cell \code{c} in the matrix (i.e., intersection of the second row and the first column (\code{[2, 1]}))
#' @param d cell \code{d} in the matrix (i.e., intersection of the second row and the second column (\code{[2, 2]}))
#' @param a.exp expected frequency in the reference cell \code{a} of the 2-by-2 crosstabulation matrix.
#' @param two.sided whether to perform one-sided test (\code{FALSE} -- Default) or two-sided (\code{TRUE}).
#' @param res choice of outputs: (i) the \code{p.value} ("p") or (ii) \code{odds.ratio} ("or").
#' @param float the floating digits (\code{3} -- Default).
#' @return vector of the same length as \code{a}.


fye2 <- function (a, b, c, d, a.exp, two.sided = FALSE, res = c("p", "or"), float = 3) {
  results <- c()
  for (i in seq_along(a)) {
    cross.table <- rbind(c(a[i], b[i]), c(c[i], d[i])) # create a 2-by-2 matrix for FYE input
    if (two.sided == FALSE) {
      if (a[i] >= a.exp[i]) { # check if the obs.freq is more than exp.freq; if yes, take 'greater' alternative in the FYE
        output <- stats::fisher.test(cross.table, alternative = 'greater') # one-tailed FYE for more-than-exp obs.freq
      } else { # check if the obs.freq is less than exp.freq; if yes, take 'less' alternative in the FYE
        output <- stats::fisher.test(cross.table, alternative = 'less') # one-tailed FYE for less-than-exp obs.freq
      }
    } else {
      output <- stats::fisher.test(cross.table, alternative = "two.sided")
    }
    if (res %in% c("pval", "p.value", "pvalue", "p")) {
      results <- c(results, as.numeric(format(output$p.value, digits = float + 1)))
    } else if (res %in% c("or", "odds.ratio", "OR")) {
      results <- c(results, round(output$estimate, digits = float))
    }
  }
  return(results)
}

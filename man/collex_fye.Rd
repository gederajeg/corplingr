% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corplingr_collex_fye.R
\name{collex_fye}
\alias{collex_fye}
\title{Iterative \emph{Fisher-Yates Exact} test for collexeme/collocational analysis}
\usage{
collex_fye(
  a = "frequency of co-occurrence of the collocate and the node",
  a_exp = "expected frequency",
  n_w_in_corp = "total frequency of collexemes/collocates in the whole corpus",
  corpus_size = "total size of the corpus",
  n_pattern = "total frequency of the construction/node word in the whole corpus",
  two_sided = FALSE,
  collstr_res = TRUE,
  float = 3
)
}
\arguments{
\item{a}{cell \code{a} in a 2-by-2 crosstabulation matrix (viz. representing the co-occurrence tokens of the levels of the variables. For instance, word-word co-occurrences, or word-construction co-occurrences).}

\item{a_exp}{expected frequency for cell \code{a} in the 2-by-2 crosstabulation matrix}

\item{n_w_in_corp}{the total frequency of the collexemes/collocates of the target construction/node word in the corpus.}

\item{corpus_size}{the total size (in word tokens) of the corpus.}

\item{n_pattern}{the total frequency of occurrence of the target construction/node word in the corpus.}

\item{two_sided}{logical; whether to perform one-sided test (\code{FALSE} -- Default) or two-sided (\code{TRUE}).}

\item{collstr_res}{logical; whether output the FYE \emph{p}-value as the Collostruction Strength value (\code{TRUE} -- the default) or just report the \emph{p}-value (\code{FALSE}).}

\item{float}{the floating digits of the Collostruction/Collocation Strength. The default value is \code{3}.}
}
\value{
Numeric vector of the same length as \code{a} interpreted as the Collostruction Strength of the construction/node word with the collexemes/collocates.
    Collostruction Strength is (i) the negative logarithm to the base of ten of the Fisher-Yates Exact test \emph{p}-value when \code{a} > \code{a_exp}, and (ii) the positive logarithm when \code{a} <= \code{a_exp}.
}
\description{
This is a vectorised wrapper for the \code{\link[stats]{dhyper}} function in the \code{stats} package.
    The implementation of the code is adapted from Gries (2012).
    \code{collex_fye} also provides a logical argument (i.e., \code{two_sided}) whose value is passed to the \code{alternative} argument of the embedded \code{\link[stats]{fisher.test}} if \code{two_sided} is \code{TRUE}.
}
\examples{
\dontrun{
# do the collocate search using "corpus_path" input-option
library(tidyverse)
df <- colloc_default(corpus_path = orti_bali_path,
                     pattern = "^nuju$",
                     window = "b", # focusing on both left and right context window
                     span = 3) # retrieve 3 collocates to the left and right of the node
# prepare the collexeme analysis input tibble
# and select to focus on R1 and R2 collocates.
collex_tb <- collex_prepare(df, span = c("r1", "r2"))

# run the Fisher-Yates Exact (FYE) Test in vectorised fashion with the help of purrr's pmap
# the example below runs the one-tailed FYE and output the p-value in log10 of CollStr value
collex_tb <- mutate(collex_tb,
                    collstr = purrr::pmap_dbl(list(a, a_exp, n_w_in_corp, corpus_size, n_pattern),
                                   collex_fye, two_sided = FALSE, collstr_res = TRUE))
# preview the results
collex_tb
}
}

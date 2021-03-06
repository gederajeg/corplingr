% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corplingr_freqlist_leipzig_all.R
\name{freqlist_leipzig_all}
\alias{freqlist_leipzig_all}
\title{Frequency list of all words in a Leipzig Corpus file}
\usage{
freqlist_leipzig_all(
  split_regex = "([^a-zA-Z0-9-]+|--)",
  leipzig_path = NULL,
  case_insensitive = TRUE
)
}
\arguments{
\item{split_regex}{user-defined regular expressions to tokenise the corpus.}

\item{leipzig_path}{full filepath to one or more of the Leipzig Corpus file(s).}

\item{case_insensitive}{logical; ignoring (\code{TRUE}) or maintaining (\code{FALSE}) the case when splitting the corpus into word token.}
}
\value{
A tibble of frequency list in descending order of the frequency.
}
\description{
The function generates a frequency list of all word-tokens in a single Leipzig Corpus file.
    While users can input all filepath to all corpus files, for memory-efficiency, it is recommended that each file is processed in separate function-call.
    If it is decided to process all corpus files, the functions output a List with as many elements as the number of the input filepath.
}
\examples{
\dontrun{
wlist_all <- freqlist_leipzig_all(split_regex = "([^a-zA-Z0-9-]+|--)",
                                  leipzig_path = leipzig_corpus_path[1])
}
}

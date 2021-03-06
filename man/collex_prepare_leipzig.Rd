% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corplingr_collex_prepare_leipzig.R
\name{collex_prepare_leipzig}
\alias{collex_prepare_leipzig}
\title{Prepare Leipzig collocates data for collexemes/collocates analysis}
\usage{
collex_prepare_leipzig(
  list_output = NULL,
  leipzig_wordlist_path = leipzig_mywordlist_path,
  node_pattern = "regex for the node word",
  span = NULL,
  stopwords_list = NULL
)
}
\arguments{
\item{list_output}{The list output of \code{\link{colloc_leipzig}}.}

\item{leipzig_wordlist_path}{Full path to the wordlist table for each Leipzig Corpus File. This can be a plain text file or an .RData file.}

\item{node_pattern}{Regex patterns of the node word specified in \code{\link{colloc_leipzig}}.}

\item{span}{Character vector of the context-window span user wants to focus on for the collexeme/collocate analysis.
For instance, single span: \code{"l1"}, \code{"r1"}; or multiple spans: \code{c("r1", "r2")}.}

\item{stopwords_list}{A character vector of the stopword list.}
}
\value{
A tibble data frame
}
\description{
This function is designed to handle the output of \code{\link{colloc_leipzig}} to generate a tidy data frame required as input of \code{\link{collex_fye}}.
    The latter is used to compute collexeme/collocate strength using one-tailed \emph{Fisher-Yates Exact} test.
}
\examples{
\dontrun{
# retrieve collocates for a given word
rgx <- "\\\\bmengakhir\\\\b"
coll_df <- colloc_leipzig(leipzig_path = leipzig_corpus_path,
                          pattern = rgx,
                          window = "r",
                          span = 4,
                          save_results = FALSE,
                          to_lower_colloc = TRUE)

# get only the collocates output
list_output <- coll_df$collocates

# collstr analysis for collocates from Leipzig Corpora
### prepare input table for coll.analysis ### <--- HERE IS THE CALL FOR collex_prepare_leipzig()
collex_tb <- collex_prepare_leipzig(list_output = coll_df,
                                   leipzig_wordlist_path = leipzig_mywordlist_path,
                                   node_pattern = rgx,
                                   span = c("r1"),
                                   stopwords_list = NULL)
# remove any NA row data
collex_tb <- dplyr::filter_all(collex_tb,
                              dplyr::all_vars(!is.na(.)))

# compute one-tailed FYE for collexeme analysis
collex_tb <- dplyr::mutate(collex_tb,
                          collstr = collex_fye(a = .data$a, # here is the call to collex_fye
                                               n_corpus = .data$corpus_size,
                                               n_coll = .data$n_w_in_corp,
                                               n_cxn = .data$n_pattern))

# sort in decreasing order by collostruction strength
dplyr::arrange(collex_tb, dplyr::desc(collstr))
}
}

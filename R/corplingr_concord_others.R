#' Simple concordance function
#'
#' @description The function generates a tidy concordance for a search pattern in a (set of) corpus (files).
#'     The function requires the corpus file(s) loaded and ready in the console as a vector of text with more than one line of texts/sentences. Each line should not correspond to one sentence. See \bold{Examples} below for details.
#' @param corpus_vector the vector of corpus texts.
#' @param pattern regular expressions for the search pattern.
#' @param to_lower_corpus whether to lowercase the corpus (\code{TRUE} -- the default) first or leave it as is (\code{FALSE}).
#' @param case_insensitive whether to ignore the case for the search \code{pattern} argument (\code{TRUE} -- the default) or not (\code{FALSE}).
#' @param context_char integer vector for the specified number of character as context to the left and right of the node pattern.
#' @return A tibble/data frame for the concordance match with \code{LEFT} and \code{RIGHT} contexts.
#' @examples
#' \dontrun{
#' # Load or read in the corpus data
#' # "load" approach
#' my_corpus_data <- "/Your/Path/To/Corpus.RData"
#' load(my_corpus_data)
#'
#' # "read" approach
#' my_corpus_path <- "/Your/Path/To/Corpus.txt"
#' corp <- readr::read_lines(my_corpus_path)
#'
#' # Inspect the first two elements.
#' head(corp, 2)
#' [1] "Hari yang panas itu berangsur-angsur menjadi dingin, karena matahari,
#'      raja siang itu, akan masuk ke dalam peraduannya, ke balik Gunung Sibualbuali,
#'      yang menjadi watas dataran tinggi Sipirok yang bagus itu."
#' [2] "Langit di sebelah barat pun merah kuning rupanya, dan sinar matahari
#'      yang turun itu nampaklah di atas puncak kayu yang tinggi-tinggi, indah
#'      rupanya, sebagai disepuh dengan emas juwita."
#'
#' # OPTIONAL
#' # Trim down leading and trailing white space
#' # with str_trim from the stringr package
#' corp <- stringr::str_trim(corp)
#' # remove excessive white space in the text into just one space
#' corp <- stringr::str_replace_all(corp, "\\s{2,}", " ")
#'
#'
#' # get concordance for a pattern
#' concordance <- concord_others(corpus_vector = corp,
#'                                    pattern = "\\bmemandang\\b",
#'                                    to_lower_corpus = TRUE,
#'                                    case_insensitive = TRUE,
#'                                    context_char = 100)
#'
#' # check the output
#' str(concordance)
#' head(concordance)
#'
#' # save the output as tab-separated text file
#' # it can be opened in a spreadsheet software for further annotation
#' readr::write_delim(concordance,
#'                    path = "/Users/Primahadi/Desktop/my_concordance.txt",
#'                    delim = "\t")
#' }
#'
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom purrr map_df
#' @importFrom stringr str_c
#' @importFrom stringr str_sub
#' @importFrom stringr str_count
#' @importFrom stringr str_subset
#' @importFrom stringr regex
#' @importFrom stringr str_locate_all
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_trim
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
concord_others <- function(corpus_vector = "character vector of text loaded/read into console",
                           pattern = "regular expressions",
                           to_lower_corpus = TRUE,
                           case_insensitive = TRUE,
                           context_char = 50) {

  # subset the line/text containing the potential match
  regexpr <- pattern
  cat("Subsetting sentences containing the match...\n")
  m <- corpus_vector %>%
    stringr::str_subset(stringr::regex(regexpr, ignore_case = case_insensitive))

  if ((length(m) >= 1) == TRUE) {

    if (to_lower_corpus == TRUE) {
      cat("Lowercasing the sentences with the match...\n")
      m <- stringr::str_to_lower(m)
    }

    # detect the start and end character-location of the match
    #cat("Detecting the match/pattern...\n")
    match_location <- stringr::str_locate_all(m, stringr::regex(regexpr, ignore_case = case_insensitive)) %>%
      purrr::map(tibble::as_tibble) %>%
      purrr::map_df(dplyr::bind_rows)

    # duplicate the number of subset text as many as the number of the match
    m1 <- rep(m, stringr::str_count(m, stringr::regex(regexpr, ignore_case = case_insensitive)))

    # extract match
    cat("Generating the concordance for the match/pattern...\n")
    node <- stringr::str_sub(m1, start = match_location$start, end = match_location$end)
    node_tag <- stringr::str_c("\t<NODE>", node, "</NODE>\t", sep = "")
    left <- stringr::str_sub(m1, start = 1, end = (match_location$start - 1))
    right <- stringr::str_sub(m1, start = (match_location$end + 1), end = nchar(m1))

    # create concordance
    LEFT <- stringr::str_sub(left, start = (nchar(left) - context_char), end = nchar(left))
    LEFT <- replace(LEFT, nchar(LEFT) == 0, "~")
    NODE <- node
    RIGHT <- stringr::str_sub(right, start = 1, end = context_char)
    RIGHT <- replace(RIGHT, nchar(RIGHT) == 0, "~")
    concord_df <- tibble::tibble(LEFT, NODE, RIGHT)
    concord_df <- dplyr::mutate(concord_df,
                                LEFT = stringr::str_trim(.data$LEFT),
                                NODE = stringr::str_trim(.data$NODE),
                                RIGHT = stringr::str_trim(.data$RIGHT))
    cat("Done!\n")
    return(concord_df)
  } else {
    message("Sorry; no match found! Try another corpus/pattern!\n")
  }
}

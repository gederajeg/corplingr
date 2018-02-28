# load stopwords
stopwords1 <- "/Users/Primahadi/Downloads/stopwords_id.txt"
stopwords2 <- "/Users/Primahadi/Downloads/stopword_id_new.txt"

stopwords1 <- readr::read_lines(stopwords1, skip = 2)
stopwords2 <- readr::read_lines(stopwords2)
stopwords <- unique(c(stopwords1, stopwords2))

# store the full path of the folder containing the leipzig corpora
leipzig_corpus_directory <- "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora"
leipzig_wordlist_directory <- "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/leipzig_wordlist"

# get the complete path to each file of the leipzig corpora
# by indexing (in the "pattern" argument) the initials of the corpus files (i.e. "^ind(_|[-])")
corpus_files <- dir(path = leipzig_corpus_directory,
                   pattern = "^ind(_|[-])",
                   full.names = FALSE) # only the basename ("full.names=FALSE")
names(corpus_files) <- 1:length(corpus_files)

corpus_files_path <- dir(path = leipzig_corpus_directory,
                        pattern = "^ind(_|[-])",
                        full.names = TRUE) # full path ("full.names=TRUE")
corpus_id <- stringr::str_replace(corpus_files, "-sentences.*$", "")
names(corpus_id) <- 1:length(corpus_id)

corpus_vector_path <- dir(path = leipzig_corpus_directory,
                          pattern = "^corpus_word_vector__",
                          full.names = TRUE)
corpus_vector_id <- stringr::str_replace(basename(corpus_vector_path), "^corpus_.+?__", "")

corpus_cleaned_path <- dir(path = leipzig_corpus_directory,
                           pattern = "^corpus_sent_vector__",
                           full.names = TRUE)
corpus_cleaned_id <- stringr::str_replace(basename(corpus_cleaned_path), "^corpus_.+?__", "")

# get the complete path to each file of the leipzig corpora wordlist
wordlist_corpus_path <- dir(path = leipzig_wordlist_directory, full.names = TRUE)
wordlist_corpus_id <- stringr::str_replace(basename(wordlist_corpus_path), "-words.*$", "")
wordlist_df_path <- dir(path = leipzig_corpus_directory, pattern = "_wordlist__", full.names = TRUE)
wordlist_df_id <- stringr::str_replace(basename(wordlist_df_path), "^corpus_.+?__(?=ind)", "")

# generate internal data
devtools::use_data(corpus_files,
                   corpus_files_path,
                   corpus_vector_path,
                   corpus_vector_id,
                   corpus_cleaned_path,
                   corpus_cleaned_id,
                   corpus_id,
                   leipzig_corpus_directory,
                   leipzig_wordlist_directory,
                   wordlist_corpus_id,
                   wordlist_corpus_path,
                   wordlist_df_path,
                   wordlist_df_id,
                   stopwords,
                   stopwords1,
                   stopwords2,
                   internal = TRUE,
                   overwrite = TRUE)

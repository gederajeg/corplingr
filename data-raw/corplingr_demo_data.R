n_sample <- 10

## Balinese raw texts
demo_corpus_bali <- vector(mode = "list", length = n_sample)
sample_corpus <- orti_bali_path[sample(seq_along(orti_bali_path), n_sample)]
for (i in seq_along(sample_corpus)) {
  demo_corpus_bali[[i]] <- readr::read_lines(sample_corpus[i])
  names(demo_corpus_bali)[i] <- stringr::str_replace(basename(sample_corpus[i]), "\\.txt$", "")
}
demo_corpus_bali <- purrr::map(demo_corpus_bali, stringr::str_trim)
demo_corpus_bali <- purrr::map(demo_corpus_bali, ~.[nzchar(.)])

## Indonesian raw texts
blog_cerpen_dir <- "/Users/Primahadi/Documents/Corpora/CORPUS/NKumpulan Cerpen.blogspot.com"
blog_cerpen_path <- dir(path = blog_cerpen_dir, pattern = "^(?i)kum.+\\.txt$", full.names = TRUE)
demo_corpus_id <- vector(mode = "list", length = n_sample)
sample_corpus <- blog_cerpen_path[sample(seq_along(blog_cerpen_path), n_sample)]
for (i in seq_along(sample_corpus)) {
  demo_corpus_id[[i]] <- readr::read_lines(file = sample_corpus[i], locale = readr::locale(encoding = "cp1252"))
  names(demo_corpus_id)[i] <- stringr::str_replace(basename(sample_corpus[i]), "\\.txt$", "")
}
demo_corpus_id <- purrr::map(demo_corpus_id, stringr::str_trim)
demo_corpus_id <- purrr::map(demo_corpus_id, ~.[nzchar(.)])


## Indonesian Leipzig Corpora subsample
sent_sample <- 250
demo_corpus_leipzig <- vector(mode = "list", length = length(leipzig_corpus_path))
for (i in seq_along(leipzig_corpus_path)) {
  counter <- readr::read_lines(file = leipzig_corpus_path[i])
  demo_corpus_leipzig[[i]] <- counter[sample(seq_along(counter), sent_sample)]
  names(demo_corpus_leipzig)[i] <- stringr::str_replace_all(basename(leipzig_corpus_path[i]), "(-sentences\\.txt)$", "")
}

devtools::use_data(demo_corpus_bali, demo_corpus_id, demo_corpus_leipzig, overwrite = TRUE)




# test colloc_default
dfid <- colloc_default(corpus_list = demo_corpus_id,
                       pattern = "jalan",
                       tokenise_corpus_to_sentence = TRUE)
collex_tb <- collex_prepare(dfid, "r1")
collex_tb <- dplyr::mutate(collex_tb, collstr = collex_fye(a, corpus_size, n_w_in_corp, n_pattern))


df <- colloc_default(corpus_path = orti_bali_path[1:5], pattern = "^nuju$")
typeof(df)
length(df)

df1 <- colloc_default(corpus_list = demo_corpus_bali, pattern = "^nuju$")
typeof(df1)
length(df1)

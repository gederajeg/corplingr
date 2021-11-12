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

## Demo data for unit testing

flist <- freqlist_leipzig_all(leipzig_path = leipzig_corpus_path[2:3])
flist_mini <- lapply(flist, function(x) x[1:5,])

mini_leipzig <- sample(readr::read_lines(file = "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_news_2008_300K-sentences.txt"), 30)
readr::write_lines(mini_leipzig, file = "data-raw/mini_leipzig.txt")

mini_leipzig <- sample(readr::read_lines(file = "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_news_2008_300K-sentences.txt"), 30)
readr::write_lines(mini_leipzig, file = "data-raw/mini_leipzig.txt")

mini_leipzig_1 <- sample(readr::read_lines(file = "/Users/Primahadi/Documents/Corpora/_corpusindo/Leipzig Corpora/ind_news_2009_300K-sentences.txt"), 30)
readr::write_lines(mini_leipzig_1, file = "data-raw/mini_leipzig_01.txt")

obali_colloc_output_test <- corplingr::colloc_default(corpus_path = orti_bali_path[1:200],
                                    pattern = "^nuju$",
                                    window = "b",
                                    span = 3)

usethis::use_data(demo_corpus_bali, demo_corpus_id, demo_corpus_leipzig, overwrite = FALSE)

usethis::use_data(flist_mini, obali_colloc_output_test, mini_leipzig, mini_leipzig_1, overwrite = FALSE)

# code for creating the sticker
imgurl <- system.file('man/figures/conc-logo.png', package = "corplingr")

sticker(imgurl,
        package = "corplingr",
        p_size = 9,
        s_x = 1,
        s_y = .78,
        s_width = .85,
        p_x = 1,
        p_y = 1.4,
        p_color = 'royalblue',
        p_family = "sans",
        h_fill = 'beige',
        spotlight = FALSE,
        h_color = 'gold',
        url = "https://gederajeg.github.io/corplingr/",
        u_size = 1.275,
        filename = "man/figures/corplingr-logo.png")

# test colloc_default
#dfid <- colloc_default(corpus_list = demo_corpus_id,
 #                      pattern = "jalan",
  #                     tokenise_corpus_to_sentence = TRUE)
#collex_tb <- collex_prepare(dfid, "r1")
#collex_tb <- dplyr::mutate(collex_tb, collstr = collex_fye(a, corpus_size, n_w_in_corp, n_pattern))


#df <- colloc_default(corpus_path = orti_bali_path[1:5], pattern = "^nuju$")
#typeof(df)
#length(df)

#df1 <- colloc_default(corpus_list = demo_corpus_bali, pattern = "^nuju$")
#typeof(df1)
#length(df1)

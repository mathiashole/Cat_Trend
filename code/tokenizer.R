# module tokenizer text

suppressPackageStartupMessages({
    library(tidyverse)
    library(tidytext)
    library(stopwords)
})
# call the clean text module to use the custom stopwords path
source("code/clean_text.R")

load_stopwords <- function(language, custom_stopwords_path = NULL) {
    language <- tolower(language)
    # Charge stopwords list based on the language
    if (tolower(language) == "sp") {
        stopwords_list <- stopwords(language = "es", source = "stopwords-iso")
    }  else if (tolower(language) == "en") {
        stopwords_list <- stopwords(language = "en", source = "stopwords-iso")
    } else {
        stop("ERROR [Tokenizer]: Languague invalid 'SP' o 'EN'.")
    }

    # Integrate personalized stopwords
    if (!is.null(custom_stopwords_path) && nzchar(trimws(custom_stopwords_path)) && custom_stopwords_path != "null") {
        if (file.exists(custom_stopwords_path)) {
            cat("[Tokenizer] Charge personalized stopword:", custom_stopwords_path, "\n")

            # Read file and process custom stopwords
            custom_words <- readLines(custom_stopwords_path, warn = FALSE) %>%
                vapply(clean_general_text, FUN.VALUE = character(1)) %>%
                # sapply(clean_general_text) %>%
                # tolower() %>%
                trimws()

            # Merge with default stopwords and ensure uniqueness
            stopwords_list <- unique(c(stopwords_list, custom_words))
        } else {
            warning(paste("Don't find personalized stopword file:", custom_stopwords_path, ". We use stopword default."))
        }
    }
    return(stopwords_list)
}

# # Internal function to proccess complex n-grams
# generate_ngrams <- function(df, n) {
#     df %>%
#         unnest_tokens(word, text, token = "ngrams", n = n) %>%
#         separate(word, into = paste0("word", 1:n), sep = " ", fill = "right") %>%
#         filter(across(starts_with("word"), ~ !is.na(.) & !grepl("\\d+", .) & !(tolower(.) %in% stopwords_list))) %>%
#         unite(word, starts_with("word"), sep = " ")
# }

# Orchestrate the tokenization process
tokenize_corpus <- function(paragraphs_df, language, ngram_number, custom_stopwords_path = NULL) {
    stopwords_list <- load_stopwords(language, custom_stopwords_path)

    # Internal function to proccess complex n-grams
    generate_ngrams <- function(df, n) {
        df %>%
            unnest_tokens(word, text, token = "ngrams", n = n) %>%
            separate(word, into = paste0("word", 1:n), sep = " ", fill = "right") %>%
            filter(across(starts_with("word"), ~ !is.na(.) & !grepl("\\d+", .) & !(tolower(.) %in% stopwords_list))) %>%
            unite(word, starts_with("word"), sep = " ")
    }

    ngram_str <- as.character(ngram_number)

    if (ngram_str == "1" || ngram_str == "") {
        tokens_df <- paragraphs_df %>%
            unnest_tokens(word, text) %>%
            filter(!grepl("\\d+", word) & !(tolower(word) %in% stopwords_list))
    } else if (ngram_str %in% c("2", "3", "4")) {
        tokens_df <- generate_ngrams(paragraphs_df, as.numeric(ngram_str))
    } else {
        stop("ERROR [Tokenizer]: Invalid ngram value. Use 1, 2, 3 or 4.")
    }

    tokens_df <- tokens_df %>% filter(!is.na(word) & word != "")

    return(tokens_df)
}
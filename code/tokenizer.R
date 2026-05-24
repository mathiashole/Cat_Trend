# module tokenizer text

suppressPackageStartupMessages({
    library(tidyverse)
    library(tidytext)
    library(stopwords)
})

tokenize_corpus <- function(paragraphs_df, language, ngram_number) {
# Charge stopwords list based on the language
if (tolower(language) == "sp") {
    stopwords_list <- stopwords(language = "es", source = "stopwords-iso")
}  else if (tolower(language) == "en") {
    stopwords_list <- stopwords(language = "en", source = "stopwords-iso")
} else {
    stop("ERROR [Tokenizer]: Languague invalid 'SP' o 'EN'.")
}

# Integrate personalized stopwords
if (!is.null(custom_stopwords_path) && custom_stopwords_path != "" && custom_stopwords_path != "null") {
    if (file.exists(custom_stopwords_path)) {
        cat("[Tokenizer] Charge personalized stopword:", custom_stopwords_path, "\n")

        # Read file and process custom stopwords
        custom_words <- readLines(custom_stopwords_path, warn = FALSE) %>%
            tolower() %>%
            trimws()

        # Merge with default stopwords and ensure uniqueness
        stopwords_list <- unique(c(stopwords_list, custom_words))
    } else {
        warning(paste("Don't find personalized stopword file:", custom_stopwords_path, ". We use stopword default."))
    }
}


}
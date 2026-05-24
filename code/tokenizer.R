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
}

}
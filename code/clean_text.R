# Module for cleaning text data, such as removing punctuation, converting to lowercase, etc.

library(stringr)

remove_accents <- function(text) {
    if (is.na(text) || text == "") return(text)
    cleaned <- iconv(text, to = "ASCII//TRANSLIT")

    return(cleaned)
}
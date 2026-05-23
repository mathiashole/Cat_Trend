# Module for cleaning text data, such as removing punctuation, converting to lowercase, etc.

library(stringr)

remove_accents <- function(text) {
    if (is.na(text) || text == "") return(text)
    cleaned <- iconv(text, to = "ASCII//TRANSLIT")

    return(cleaned)
}

# Normalize plural forms to singular (e.g., "gatos" -> "gato", "peces" -> "pez")
normalize_document_name <- function(name) {
    if (is.na(name) || name == "") return(NA_character_)



    return(longest_word)
}
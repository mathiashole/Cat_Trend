# Module for cleaning text data, such as removing punctuation, converting to lowercase, etc.

library(stringr)

normalize_encoding <- function(text) {


    return(text)
}

remove_accents <- function(text) {
    if (is.na(text) || text == "") return(text)
    cleaned <- iconv(text, to = "ASCII//TRANSLIT")

    return(cleaned)
}

# Normalize plural forms to singular (e.g., "gatos" -> "gato", "peces" -> "pez")
normalize_document_name <- function(name) {
    if (is.na(name) || name == "") return(NA_character_)

    #name <- basename(name)
    name <- tolower(name)
    name <- tools::file_path_sans_ext(name) # Remove extension txt, csv, etc.
    name <- remove_accents(name)
    name <- gsub("[^a-z]", " ", name) # Change non-letters to space
    #name <- gsub("[^a-z0-9]+", "_", name)
    #name <- gsub("^_|_$", "", name)
    words <- unlist(strsplit(name, "\\s+")) # Split by spaces
    words <- words[nchar(words) > 2] # Filter out short words (2 or fewer characters)

    if (length(words) == 0) return(NA_character_)

    # Keep the longest word as the normalized name
    longest_word <- words[which.max(nchar(words))]

    return(longest_word)
}

# Usful for removing noise before tokenizing or generating embeddings
clean_general_text <- function(text) {
    if (is.na(text) || text == "") return(text)

    text <- tolower(text)
    text <- remove_accents(text)
    text <- gsub("[^a-z0-9\\s]", " ", text) # Keep letters, numbers and spaces
    text <- str_squish(text) # Remove extra spaces

    return(text)
}
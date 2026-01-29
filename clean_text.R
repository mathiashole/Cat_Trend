normalizar_plural <- function(palabra) {

  if (is.na(palabra) || palabra == "")
    return(NA_character_)

  palabra <- tolower(trimws(palabra))

  # peces -> pez
  palabra <- sub("ces$", "z", palabra)

  n <- nchar(palabra)

  if (n > 4) {
    palabra <- sub("es$", "", palabra)
  } else if (n > 3) {
    palabra <- sub("s$", "", palabra)
  }

  palabra
}

clean_text <- function(text, config) {

  if (is.na(text) || text == "") return(NA_character_)

  if (isTRUE(config$cleaning$remove_accents))
    text <- iconv(text, to = "ASCII//TRANSLIT")

  if (isTRUE(config$cleaning$to_lower))
    text <- tolower(text)

  if (isTRUE(config$cleaning$remove_urls))
    text <- gsub("http[s]?://\\S+|www\\S+", " ", text)

  if (isTRUE(config$cleaning$remove_numbers))
    text <- gsub("[0-9]+", " ", text)

  if (isTRUE(config$cleaning$keep_only_letters))
    text <- gsub("[^a-z\\s]", " ", text)

  text <- gsub("\\s+", " ", text)
  text <- trimws(text)

  # palabras <- strsplit(text, "\\s+")[[1]]

  palabras <- strsplit(text, "\\s+")[[1]]

palabras <- palabras[!is.na(palabras) & palabras != ""]

if (isTRUE(config$normalization$plural)) {
  palabras <- vapply(palabras, normalizar_plural, character(1))
}

  if (!is.null(config$synonyms)) {
    for (canonico in names(config$synonyms)) {
      palabras[palabras %in% config$synonyms[[canonico]]] <- canonico
    }
  }

  paste(palabras, collapse = " ")
}
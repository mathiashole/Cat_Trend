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

  if (!is.null(config$synonyms)) {
    for (canonico in names(config$synonyms)) {
      palabras[palabras %in% config$synonyms[[canonico]]] <- canonico
    }
  }

  paste(palabras, collapse = " ")
}
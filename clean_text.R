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

  if (isTRUE(config$normalization$plural)) {

    if (!exists("normalizar_plural", mode = "function")) {
      stop("normalizar_plural() no está definida. ¿Olvidaste source()?")
    }

    palabras <- vapply(palabras, normalizar_plural, character(1))
  }

  if (!is.null(config$synonyms)) {
    for (canonico in names(config$synonyms)) {
      palabras[palabras %in% config$synonyms[[canonico]]] <- canonico
    }
  }

  paste(palabras, collapse = " ")
}
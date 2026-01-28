normalizar_plural <- function(palabra) {

  if (is.na(palabra) || palabra == "")
    return(NA_character_)

  palabra <- tolower(trimws(palabra))

  # peces -> pez
  palabra <- sub("ces$", "z", palabra)

  n <- nchar(palabra)


}

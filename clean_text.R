# normalizar_plural <- function(palabra) {

#   palabra <- tolower(trimws(palabra))

#   # peces -> pez
#   palabra <- sub("ces$", "z", palabra)

#   if (nchar(palabra) > 4) {
#     palabra <- sub("es$", "", palabra)
#   } else if (nchar(palabra) > 3) {
#     palabra <- sub("s$", "", palabra)
#   }

#   palabra
# }
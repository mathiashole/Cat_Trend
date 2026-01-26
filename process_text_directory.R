process_text_directory <- function(input_dir, output_dir, config) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  files <- list.files(input_dir, full.names = TRUE)

  actas_df     <- list()
  parrafos_df  <- list()

  for (file in files) {

    message("→ Procesando: ", basename(file))

    texto_crudo <- readLines(file, warn = FALSE)
    # normalized encoding to UTF-8
    texto_crudo <- iconv(texto_crudo, from = "", to = "UTF-8", sub = "")

    texto_limpio <- vapply(
      texto_crudo,
      clean_text,
      character(1),
      config = config
    )

    message(mean(is.na(texto_limpio)))

    writeLines(
      texto_limpio,
      file.path(output_dir, basename(file))
    )
  }
}



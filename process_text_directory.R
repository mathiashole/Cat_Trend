process_text_directory <- function(input_dir, output_dir, config) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  files <- list.files(input_dir, full.names = TRUE)

  actas_df     <- list()
  parrafos_df  <- list()

  for (file in files) {

    message("→ Procesando: ", basename(file))

    # --- metadata --- ### but in the feature checking phase
    fname <- basename(file)
    base  <- tools::file_path_sans_ext(fname)

    municipio <- strsplit(base, "_")[[1]][1]
    fecha_raw <- strsplit(base, "_")[[1]][2]
    fecha     <- as.Date(fecha_raw, format = "%d.%m.%Y")

    # --- read text --- ###
    texto_crudo <- readLines(file, warn = FALSE)
    # normalized encoding to UTF-8
    texto_crudo <- iconv(texto_crudo, from = "", to = "UTF-8", sub = "")

    # --- clean NA lines --- ###
    texto_crudo <- texto_crudo[!is.na(texto_crudo)]

    # --- split into blocks by empty lines. Detect paragraphs --- ###
    bloques <- split(
      texto_crudo,
      cumsum(trimws(texto_crudo) == "")
    )

    bloques <- bloques[sapply(bloques, function(x)
      any(trimws(x) != ""))
    ]
    # --- initialize cleaned text vector --- ###
    texto_acta_limpio <- character(0)

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



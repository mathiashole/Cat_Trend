#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(yaml)
})

# Load functions from functions.R
source("code/init_utilitis.R")
source("code/get_time_v1.R")
source("code/txt_ngram_to_tidy.R")
source("code/txt_frequency.R")

# Read configuration from config.yaml

args <- commandArgs(trailingOnly = TRUE)

config_file <- "config/default.yaml"

if ("--config" %in% args) {

  config_index <- which(args == "--config") + 1

  if (config_index > length(args)) {
    stop("ERROR: Missing config file after --config")
  }

  config_file <- args[config_index]
}

# Validate config file

if (!file.exists(config_file)) {
  stop(paste("ERROR: Config file does not exist:", config_file))
}

cat("📄 Loading config:", config_file, "\n")

config <- yaml::read_yaml(config_file)

# ---------- Read config values ----------

directory      <- config$input$directory
keywords_file  <- config$input$keywords
language       <- config$language$selected
ngram_number   <- config$text_processing$ngram

# ---------- Validate required values ----------

if (is.null(directory)) {
  stop("ERROR: input$directory missing in YAML")
}

if (is.null(language)) {
  stop("ERROR: language$selected missing in YAML")
}

if (is.null(ngram_number)) {
  stop("ERROR: text_processing$ngram missing in YAML")
}

# ---------- Load packages ----------

required_packages <- c("stringr",
  "lubridate",
  "tidytext",
  "stopwords",
  "tidyverse",
  "ggplot2",
  "yaml")

manage_packages(required_packages)

# ---------- Create output directory ----------

project_name <- config$project$name

if (is.null(project_name)) {
  project_name <- "default_project"
}

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

directory <- NULL
keywords_file <- NULL
language <- "SP"  
ngram_number <- 2
help <- FALSE

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  help <- TRUE
}

for (i in seq_along(args)) {
  if (args[i] %in% c("--help", "-h")) {
    help <- TRUE
  } else if (args[i] %in% c("--directory", "-d")) {
    directory <- args[i + 1]
  } else if (args[i] %in% c("--keywords", "-k")) {
    keywords_file <- args[i + 1]
  } else if (args[i] %in% c("--language", "-l")) {
    language <- args[i + 1]
  } else if (args[i] %in% c("--ngram", "-n")) {
    ngram_number <- as.integer(args[i + 1])
  }
}

if (help || is.null(directory) || is.null(keywords_file)) {
  cat("
Uso:
  script.R --directory <ruta> --keywords <archivo> [--language <es|en>] [--ngram <1|2|3>] [--help]

Opciones:
  -d, --directory   Ruta a la carpeta con los textos
  -k, --keywords    Archivo con palabras clave
  -l, --language    Idioma (es o en). Por defecto: es
  -n, --ngram       Tamaño del ngrama (1, 2 o 3). Por defecto: 2
  -h, --help        Mostrar esta ayuda

Ejemplo:
  ./script.R --directory texts --keywords palabras.txt --language es --ngram 2

")
  quit(save = "no")
}

cat("🔄 Start of processing________________________________________________________\n")

cat("📖 Installing and loading libraries...\n")
required_packages <- c("stringr", "lubridate", "tidytext", "stopwords", "tidyverse", "ggplot2")
manage_packages(required_packages)

# Fecha y hora actual
date_hour <- format(Sys.time(), "%d-%m-%Y_%H:%M")

# Procesamiento principal
cat("\n📊 Preparing data...\n\n")
main_get_time(directory, date_hour) # get date
main_process_texts(directory, language, ngram_number, date_hour)

cat("\n🧮 Performing analysis...\n\n")
analyze_frequency(date_hour)

if (!is.null(keywords_file)) {
  cat("\n🧮 Performing category analysis...\n\n")
  # analyze_frequency(date_hour)
}

cat("\n✅ Processing completed successfully.\n\n")


#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(yaml)
})

# ------------------------------ Load modules ------------------------------

source("code/init_utilitis.R")

# Core pipeline
source("code/get_time_v1.R")
source("code/document_parser.R")
source("code/clean_text.R")
source("code/tokenizer.R")
source("code/txt_ngram_to_tidy.R")

# Analysis
source("code/txt_frequency.R")

# ------------------------------ Read command line arguments ------------------------------

args <- commandArgs(trailingOnly = TRUE)

config_file <- "config/default.yaml"

if ("--config" %in% args) {

    config_index <- which(args == "--config") + 1

    if (config_index > length(args)) {
        stop("ERROR: Missing config file after --config")
    }

    config_file <- args[config_index]
}

# ------------------------------ Validate config file ------------------------------

if (!file.exists(config_file)) {
    stop(paste("ERROR: Config file does not exist:", config_file))
}

cat("📄 Loading config:", config_file, "\n")

config <- yaml::read_yaml(config_file)

# ------------------------------ Read configuration values ------------------------------

directory <- config$input$directory

language <- config$language$selected

ngram_number <- config$text_processing$ngram

custom_stopwords <- config$text_processing$custom_stopwords

project_name <- config$project$name

# ------------------------------ Validate required configuration ------------------------------

if (is.null(directory)) {
    stop("ERROR: input$directory missing in YAML")
}

if (is.null(language)) {
    stop("ERROR: language$selected missing in YAML")
}

if (is.null(ngram_number)) {
    stop("ERROR: text_processing$ngram missing in YAML")
}

# ------------------------------ Load packages ------------------------------

required_packages <- c(
    "stringr",
    "lubridate",
    "tidytext",
    "stopwords",
    "tidyverse",
    "ggplot2",
    "yaml"
)

manage_packages(required_packages)


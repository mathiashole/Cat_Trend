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

# ------------------------------ Create project structure ------------------------------

if (is.null(project_name) || project_name == "") {
    project_name <- "default_project"
}

timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

output_dir <- file.path(
    "results",
    project_name,
    timestamp
)

metadata_dir <- file.path(output_dir, "metadata")
processed_dir <- file.path(output_dir, "processed")
analysis_dir <- file.path(output_dir, "analysis")
plots_dir <- file.path(output_dir, "plots")

dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

cat("📂 Output directory:", output_dir, "\n")

# ------------------------------ Save config used ------------------------------

file.copy(
    config_file,
    file.path(output_dir, "config_used.yaml")
)

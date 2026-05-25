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


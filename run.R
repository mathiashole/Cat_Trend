#!/usr/bin/env Rscript

library(yaml)

config <- yaml::read_yaml("config.yml")

source("clean_text.R")
source("process_text_directory.R")
# source("tokenize.R")
# source("analysis.R")

process_text_directory(
  input_dir  = config$paths$input_raw,
  output_dir = config$paths$output_clean,
  config     = config
)


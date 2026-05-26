#!/usr/bin/env Rscript

# Load specialized modules
source("code/clean_text.R")
source("code/document_parser.R")
source("code/tokenizer.R")

main_process_texts <- function(directory, language, ngram_number, date_hour, output_dir, custom_stopwords = NULL) {
  cat("[1/3] Parsing documents into structured tables...\n")

    metadata_file <- file.path(output_dir, paste0("data_table_", date_hour, ".txt"))

}
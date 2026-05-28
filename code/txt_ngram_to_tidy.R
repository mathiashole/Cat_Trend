#!/usr/bin/env Rscript

# Load specialized modules
source("code/clean_text.R")
source("code/document_parser.R")
source("code/tokenizer.R")

main_process_texts <- function(directory, language, ngram_number, date_hour, output_dir, custom_stopwords = NULL) {
  cat("[1/3] Parsing documents into structured tables...\n")

    metadata_file <- file.path(output_dir, paste0("data_table_", date_hour, ".txt"))

    # Step A: Generate structured paragraph dataframe
    paragraphs_df <- parse_documents_to_paragraphs(directory, metadata_file)

    # Save intermediate paragraph structure
    paragraphs_output <- file.path(output_dir, "paragraphs.tsv")

    write.table(paragraphs_df, file = paragraphs_output, row.names = FALSE, sep = "\t", quote = FALSE)

  cat("[2/3] Tokenizing and removing stopwords (N-grams:", ngram_number,")...\n")


}

# Other function
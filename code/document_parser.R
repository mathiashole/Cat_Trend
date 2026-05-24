# Module document parser

suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
})

parse_documents_to_paragraphs <- function(directory, data_table_path) {

    if (!file.exists(data_table_path)) {
        stop(paste("ERROR [Parser]: No se encuentra la tabla de metadatos en:", data_table_path))
    }

    # Read the metadata table with document paths and dates
    data_table <- read.table(data_table_path, sep = "\t", header = FALSE, col.names = c("document", "date"))
    data_table$date <- gsub("/", "-", data_table$date)

    # Normaliztion of document names with clean.R functions
    data_table$origin_document <- basename(data_table$document)
    data_table$document <- sapply(data_table$origin_document, normalize_document_name)

    # Initialize empty tibble to store paragraphs
    infoText <- tibble(document = character(), date = character(), paragraph_id = numeric(), text = character())

    # Loop per document to read and parse into paragraphs


    return(infoText)
}
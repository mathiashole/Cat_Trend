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
    data_table <- read.table(data_table_path, sep = "\t", header = FALSE, col.names = c("document", "date"), stringsAsFactors = FALSE)
    data_table$date <- gsub("/", "-", data_table$date)

    # Normaliztion of document names with clean.R functions
    data_table$origin_document <- basename(data_table$document)
    data_table$document <- sapply(data_table$origin_document, normalize_document_name)

    # Initialize empty tibble to store paragraphs
    infoText <- tibble(document = character(), date = character(), paragraph_id = numeric(), text = character())

    documents_list <- vector(mode = "list", length = nrow(data_table))

    # Loop per document to read and parse into paragraphs
    # for (i in seq_along(data_table$origin_document)) {
    for (i in seq_len(nrow(data_table))) {
        file_path <- file.path(directory, data_table$origin_document[i])
    
        if (!file.exists(file_path)) {
            message("Don't find the file: ", file_path) # Don't find the file, skip to next
            next
        }
    
        speech <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
        
        if (length(speech) == 0) {
            message("File empty: ", file_path)
            next
        }
        
        if (any(is.na(speech))) {
            message("Line with NA in: ", file_path)
            speech <- na.omit(speech)
        }
    
    # Create cleaned tibble for the current document
        # temporal <- tibble(document = data_table$document[i], date = as.character(data_table$date[i]), paragraph_id = seq_along(speech), text = speech)
        temporal <- tibble(para_id = paste0(data_table$document[i], "_p", seq_along(speech)),document = data_table$document[i], original_file = data_table$origin_document[i], date = as.character(data_table$date[i]), paragraph_id = seq_along(speech), text = speech)
    
    # infoText <- bind_rows(infoText, temporal)
        documents_list[[i]] <- temporal
    }

    # Force factors with correct levels
    infoText <- infoText %>%
        mutate(document = factor(document, levels = unique(document)), date = factor(date))

    return(infoText)
}
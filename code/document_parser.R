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



    return(infoText)
}
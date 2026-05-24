# Module document parser

suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
})

parse_documents_to_paragraphs <- function(directory, data_table_path) {

    if (!file.exists(data_table_path)) {
        stop(paste("ERROR [Parser]: No se encuentra la tabla de metadatos en:", data_table_path))
    }



    return(infoText)
}
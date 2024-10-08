#!/usr/bin/env Rscript

# Load functions from functions.R
source("code/init_utilitis.R")
source("code/get_time_v1.R")
source("code/txt_ngram_to_tidy.R")
source("code/txt_frequency.R")

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if arguments were provided or if help is requested
if (length(args) == 0 || "--help" %in% args || "-h" %in% args) {
  help_function()
  quit(save = "no")
}

# Validate and parse arguments
validated_args <- tryCatch(validate_arguments(args), error = function(e) {
  cat("Error in validate_arguments:", e$message, "\n")
  help_function()
  quit(save = "no")
})

# Parse the arguments ## CHECK
#parsed_args <- parse_args(args)

cat("🔄 Start of processing________________________________________________________\n")

cat("📖 Installing and loading libraries...\n")

required_packages <- c("stringr", "lubridate", "tidytext", "tidyverse", "ggplot2")
manage_packages(required_packages)

# Get current date and time (only once)
date_hour <- format(Sys.time(), "%d-%m-%Y_%H:%M")

# # Validate arguments and get validated values
# validated_args <- validate_arguments(args)

# # Assign validated values ​​to variables for later use
#mode <- validated_args$mode
directory <- validated_args$directory
keywords_file <- validated_args$keywords_file
language <- validated_args$language
ngram_number <- validated_args$ngram_number

# Llamar a las funciones principales
cat("\n📊 Preparing data...\n\n")

# Call main function of external script
main_get_time(directory, date_hour)
main_process_texts(directory, language, ngram_number, date_hour)

cat("\n🧮 Performing analysis...\n\n")

analyze_frequency(date_hour)


cat("\n✅ Processing completed successfully.\n\n")
# #!/usr/bin/env Rscript

# cat("🔄 Start of processing________________________________________________________\n")

# # Load functions from functions.R
# source("code/init_utilitis.R")
# source("code/get_time_v1.R")
# source("code/txt_analysis_v1.R")
# source("code/txt_frequency.R")

# cat("📖 Installing and loading libraries...\n")

# required_packages <- c("stringr", "lubridate", "tidytext", "tidyverse", "ggplot2", "argparse")
# manage_packages(required_packages)

# # Load the argparse library
# library(argparse)

# # Create a parser object
# parser <- ArgumentParser(description='Process some text files.')

# # Add arguments
# parser$add_argument("--file", required=TRUE, help="Directory containing text files")
# parser$add_argument("--language", required=TRUE, help="Language of the text files")
# parser$add_argument("--ngram", type="integer", required=TRUE, help="Number of n-gram analysis")

# # Parse the arguments
# args <- parser$parse_args()

# # Get current date and time (only once)
# date_hour <- format(Sys.time(), "%d-%m-%Y_%H:%M")

# # Assign parsed values to variables for later use
# directory <- args$file
# language <- args$language
# ngram_number <- args$ngram

# # Print arguments for debugging
# cat("\n\t📂 Directory:", directory, "\n")
# cat("\t🗣️ Language:", language, "\n")
# cat("\t🔢 N-gram number:", ngram_number, "\n")


# # Llamar a las funciones principales
# cat("\n📊 Preparing data...\n\n")

# # Call main function of external script
# main_get_time(directory, date_hour)
# main_process_texts(directory, language, ngram_number, date_hour)

# cat("\n🧮 Performing analysis...\n\n")

# analyze_frequency(date_hour)

# cat("\n✅ Processing completed successfully.\n\n")



# # Separar los argumentos dependiendo del modo
# if (mode == "manual") {

#   # Llamar a la función principal del script externo
#   main_get_time(directory, date_hour)
#   main_process_texts(directory, language, ngram_number, date_hour)
  
# } else {
  
#   # Llamar a la función principal del script externo
#   main_get_time(directory, date_hour)
#   main_process_texts(directory, language, ngram_number, date_hour)
  
# }



  # # Ejecutar otros programas en diferentes directorios
  # if (condicion_otro_programa1) {
  #   # Directorio del otro programa 1
  #   otro_directorio1 <- "/ruta/al/otro/programa1"
    
  #   # Comando para ejecutar el otro programa 1
  #   comando1 <- paste("Rscript", shQuote(file.path(otro_directorio1, "otro_programa1.R")), "--opcion", "valor1", sep = " ")
    
  #   # Ejecutar el comando en el sistema operativo
  #   system(comando1)
  # }
  
  # if (condicion_otro_programa2) {
  #   # Directorio del otro programa 2
  #   otro_directorio2 <- "/ruta/al/otro/programa2"
    
  #   # Comando para ejecutar el otro programa 2
  #   comando2 <- paste("python", shQuote(file.path(otro_directorio2, "otro_programa2.py")), "--option", "value2", sep = " ")
    
  #   # Ejecutar el comando en el sistema operativo
  #   system(comando2)
  # }
  
  # if (tolower(args[x]) == "stemmp") {
    
  #   # Cargar el resultado desde el archivo .rdata
  #   load(file = file.path(ruta_directorio_, "txt_analysis.rdata"))
    
  #   ruta_directorio <- "/ruta/completa/a/mi_directorio/"
  #   source(file.path(ruta_directorio, "stemming_porter.R"))
    
  #   result_stem <- stem_column("aca va salida del analisis de texto", "word")
    
  #   textWord_stem <- paste("datos/outputData/stem_words", "_", date_hour, ".txt", sep = "")
  #   write.table(result_stem, file = textWord_stem, row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE, qmethod = "double")
    
  # }
  
  # Agrega más bloques condicionales para otros programas si es necesario


#### GUIA DE EJECUCION DE LOS SCRIPT
## get_time_v1.R
## txt_analysis_v1.R
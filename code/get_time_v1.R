#!/usr/bin/env Rscript

library(tidyverse)
library(stringr)
library(lubridate)

# Load functions from functions.R
timeline_script <- file.path("code", "timeline_plot.R")
plotting_script <- file.path("code", "plotting.R")
if (file.exists(timeline_script)) source(timeline_script)
if (file.exists(plotting_script)) source(plotting_script)

# 1. ---------- Search date in file names ----------
extract_date_from_filename <- function(file_path) {
  file_name <- basename(file_path)

  #Posible regax patterns date
  sep <- "[\\s_\\-\\/]+" 
  day_month <- "\\d{1,2}"
  year_4 <- "\\d{4}"
  year_2 <- "\\d{2}"

  # Only two master patterns to capture the date components
  patterns <- c(
    # Patterns A: format DD/MM/YYYY o D/M/YY (day/month goes first)
    paste0("\\b", day_month, sep, day_month, sep, "(?:", year_4, "|", year_2, ")\\b"),
    
    # Patterns B: international type format YYYY/MM/DD o YYYY/M/D (Year goes first)
    paste0("\\b", year_4, sep, day_month, sep, day_month, "\\b")
  )

  for (pattern in patterns) {
    hit <- stringr::str_extract(file_name, pattern)
    
    if (!is.na(hit)) {
      # 💡 normalized all string
      hit_normalized <- gsub("[\\s_\\-\\/]+", "/", hit)
      
      # Parsing with lubridate trying both formats
      parsed <- suppressWarnings(lubridate::dmy(hit_normalized))
      if (is.na(parsed)) parsed <- suppressWarnings(lubridate::ymd(hit_normalized))
      
      if (!is.na(parsed)) {
        return(format(parsed, "%d/%m/%Y"))
      }
    }
  }
  return(NA_character_)
}

extract_date_from_content <- function(file_path, max_lines = 10) {
  # Date patterns in Spanish and English
  date_pattern_es <- "\\b(?:\\d{1,2}\\s+(?:de\\s+)?(?:enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)\\s+(?:de\\s+)?(?:\\d{2})?\\d{2}|\\d{1,2}[\\/\\-]\\d{1,2}[\\/\\-](?:\\d{2})?\\d{2})\\b"
  date_pattern_en <- "\\b(?:\\d{1,2}\\s+(?:January|February|March|April|May|June|July|August|September|October|November|December),?\\s+(?:\\d{2})?\\d{2})\\b|\\b(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},?\\s+(?:\\d{2})?\\d{2}\\b|\\b\\d{1,2}-(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-\\d{2,4}\\b|\\b\\d{1,2}[\\/\\-]\\d{1,2}[\\/\\-](?:\\d{2})?\\d{2}\\b"

  normalize_months <- function(text) {
    text <- gsub("setiembre", "septiembre", text, ignore.case = TRUE)
    text <- gsub("\\bsep\\.?\\b", "septiembre", text, ignore.case = TRUE)
    return(text)
  }

  lines <- readLines(file_path, n = max_lines, warn = FALSE)
  lines <- iconv(lines, from = "latin1", to = "UTF-8", sub = "")

  for (line in lines) {
    line_cleaned <- gsub("(?<=[0-9])\\,|\\,(?=[0-9])", ", ", line, perl = TRUE)
    line_cleaned <- gsub("(?<=[A-Za-z])\\:+|\\:+(?=[A-Za-z])", "", line_cleaned, perl = TRUE)
    line_cleaned <- gsub(":", ": ", line_cleaned)
    line_cleaned <- gsub("(?<=\\D)\\bde\\b(?=\\d)", "", line_cleaned, perl = TRUE)

    date_in_line_es <- str_extract(line_cleaned, date_pattern_es)
    date_in_line_en <- str_extract(line_cleaned, date_pattern_en)

    if (!is.na(date_in_line_es)) {
      date_in_line_es <- normalize_months(date_in_line_es)
      parsed_date <- dmy(date_in_line_es, quiet = TRUE)
      if (!is.na(parsed_date)) return(format(parsed_date, "%d/%m/%Y"))
    } else if (!is.na(date_in_line_en)) {
      parsed_date <- mdy(date_in_line_en, quiet = TRUE)
      if (!is.na(parsed_date)) return(format(parsed_date, "%d/%m/%Y"))
    }

  }

  return(NA_character_)
}

extract_document_date <- function(file_path) {

  date_found <- extract_date_from_filename(file_path)
  
  if (!is.na(date_found)) {
        return(date_found)
  }

  date_found <- extract_date_from_content(file_path, max_lines = 10)

  if (!is.na(date_found)) {
        return(date_found)
  }

  return(NA_character_)
}

main_get_time <- function(directory, date_hour, output_dir, max_lines_to_scan = 15) {

  files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)

  results <- lapply(files, function(file) {
    date_res <- extract_document_date(file, max_lines = max_lines_to_scan)

    tibble(document = file, date = date_res)

  })

  metadata_df <- bind_rows(results)

  output_file <- file.path(output_dir, paste0("data_table_", date_hour, ".txt"))

  write.table(metadata_df, output_file, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

  invisible(metadata_df)

}

# # Encapsulate program in a function
# main_get_time <- function(directory, date_hour) {

# # Function to search the date in a file
# find_date_in_file <- function(file_path) {
#   # Date patterns in Spanish and English
#   date_pattern_es <- "\\b(?:\\d{1,2}\\s+(?:de\\s+)?(?:enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)\\s+(?:de\\s+)?(?:\\d{2})?\\d{2}|\\d{1,2}[\\/\\-]\\d{1,2}[\\/\\-](?:\\d{2})?\\d{2})\\b"
#   #date_pattern_en <- "\\b(?:\\d{1,2}\\s+(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{4})\\b"
#   date_pattern_en <- "\\b(?:\\d{1,2}\\s+(?:January|February|March|April|May|June|July|August|September|October|November|December),?\\s+(?:\\d{2})?\\d{2})\\b|\\b(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},?\\s+(?:\\d{2})?\\d{2}\\b|\\b\\d{1,2}-(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-\\d{2,4}\\b|\\b\\d{1,2}[\\/\\-]\\d{1,2}[\\/\\-](?:\\d{2})?\\d{2}\\b"

#   # Month normalization function
#   normalize_months <- function(text) {
#     text <- gsub("setiembre", "septiembre", text, ignore.case = TRUE)
#     text <- gsub("\\bsep\\.?\\b", "septiembre", text, ignore.case = TRUE)
#     return(text)
#   }

# lines <- readLines(file_path, n = 10, warn = FALSE)
# lines <- iconv(lines, from = "latin1", to = "UTF-8", sub = "�")
  
#   date_found <- NA

#   # Find dates in the first 10 lines of the file
#   for (line in lines) {
#     # Clean the line by removing commas attached to the numbers and adding a space after the comma
#     line_cleaned <- gsub("(?<=[0-9])\\,|\\,(?=[0-9])", ", ", line, perl = TRUE)
    
#     # Then, clean the colons attached to letters or words (not numbers)
#     line_cleaned <- gsub("(?<=[A-Za-z])\\:+|\\:+(?=[A-Za-z])", "", line_cleaned, perl = TRUE)
    
#     # Add a space after removing the colon
#     line_cleaned <- gsub(":", ": ", line_cleaned)
    
#     # Remove "de"s that are between a word and a number, but not between a number and a word
#     line_cleaned <- gsub("(?<=\\D)\\bde\\b(?=\\d)", "", line_cleaned, perl = TRUE)
    
#     date_in_line_es <- str_extract(line_cleaned, date_pattern_es)
#     date_in_line_en <- str_extract(line_cleaned, date_pattern_en)
    
#     if (!is.na(date_in_line_es)) {
#       date_in_line_es <- normalize_months(date_in_line_es)
#       parsed_date <- dmy(date_in_line_es, quiet = TRUE)
#       if (!is.na(parsed_date)) {
#         date_found <- format(parsed_date, "%d/%m/%Y")
#         break
#       }
#     } else if (!is.na(date_in_line_en)) {
#       parsed_date <- mdy(date_in_line_en, quiet = TRUE)
#       if (!is.na(parsed_date)) {
#         date_found <- format(parsed_date, "%d/%m/%Y")
#         break
#       }
#     }
#   }

#   return(ifelse(is.na(date_found), "Date not found", date_found))
# }

# # Function to generate the data table
# generate_table <- function(dir_path) {
#   files <- list.files(path = dir_path, pattern = "\\.txt$", full.names = TRUE)
#   table <- sapply(files, function(file) {
#     date_in_file <- find_date_in_file(file)
#     c(file, date_in_file)
#   })
#   return(t(table))
# }

# # Check if the directory exists
# if (!file.exists(directory)) {
#   stop("The specified directory does not exist.")
# }

# # Check if there are .txt files in the directory
# if (length(list.files(directory, pattern = "\\.txt$", full.names = TRUE)) == 0) {
#   stop("No .txt files were found in the specified directory.")
# }

# # Generate and save data table
# tabla_datos <- generate_table(directory)

# # Save the result to a file
# output_dir <- "output"
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }

# # Create full file name
# file_name <- file.path("output/", paste0("data_table_", date_hour, ".txt"))

# write.table(tabla_datos, file = file_name, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

# # Plot word by year
# timeline_plot <- plot_timeline(file_name, title="Document per site", xlab="", ylab="Site") # these script need path not data frame!
# save_plot_to_pdf(timeline_plot, paste0("output/timeline_", date_hour, ".pdf"))
# }

# comment old code
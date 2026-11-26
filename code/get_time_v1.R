#!/usr/bin/env Rscript

library(tidyverse)
library(stringr)
library(lubridate)

# Load functions from functions.R
timeline_script <- file.path("code", "timeline_plot.R")
plotting_script <- file.path("code", "plotting.R")
if (file.exists(timeline_script)) source(timeline_script)
if (file.exists(plotting_script)) source(plotting_script)

extract_date_from_filename <- function(file_path) {
  # Quitar extensión (.txt) para evitar interferencias del punto antes de 'txt'
  stem <- tools::file_path_sans_ext(basename(file_path))

  # Buscar patrón DD.MM.YYYY o DD/MM/YYYY o DD-MM-YYYY o DD_MM_YYYY (día y mes 1-2 dígitos, año 2 o 4 dígitos)
  # Separado por límites no numéricos o inicio/fin de cadena
  m <- str_match(
    stem,
    "(?:^|[^0-9])([0-3]?[0-9])[._/-]([01]?[0-9])[._/-]([0-9]{4}|[0-9]{2})(?:$|[^0-9])"
  )

  if (all(is.na(m))) {
    return(NA_character_)
  }

  day_val   <- as.integer(m[, 2])
  month_val <- as.integer(m[, 3])
  year_raw  <- m[, 4]

  year_val <- if (nchar(year_raw) == 2L) {
    yr_num <- as.integer(year_raw)
    if (yr_num <= 50L) 2000L + yr_num else 1900L + yr_num
  } else {
    as.integer(year_raw)
  }

  # Validar rangos calendario reales (evita falsos positivos como mes > 12 o día > 31)
  if (is.na(day_val) || is.na(month_val) || is.na(year_val)) {
    return(NA_character_)
  }
  if (month_val < 1L || month_val > 12L || day_val < 1L || day_val > 31L) {
    return(NA_character_)
  }

  fecha_str <- sprintf("%02d/%02d/%04d", day_val, month_val, year_val)
  parsed <- suppressWarnings(dmy(fecha_str, quiet = TRUE))

  if (is.na(parsed)) {
    return(NA_character_)
  }

  format(parsed, "%d/%m/%Y")
}

parse_date_components <- function(day, month, year_raw) {
  d <- suppressWarnings(as.integer(day))
  m <- suppressWarnings(as.integer(month))
  y <- suppressWarnings(as.integer(year_raw))

  if (is.na(d) || is.na(m) || is.na(y)) {
    return(NA_character_)
  }

  if (nchar(as.character(year_raw)) == 2L) {
    y <- if (y <= 50L) 2000L + y else 1900L + y
  }

  if (m < 1L || m > 12L || d < 1L || d > 31L || y < 1900L || y > 2100L) {
    return(NA_character_)
  }

  real_date <- tryCatch(
    lubridate::make_date(year = y, month = m, day = d),
    error = function(e) as.Date(NA)
  )

  if (is.na(real_date)) {
    return(NA_character_)
  }

  format(real_date, "%d/%m/%Y")
}

clean_date_line <- function(line) {
  out <- stringi::stri_trans_general(line, "Latin-ASCII")
  out <- gsub("[\u2014\u2013\\-]+", " ", out, perl = TRUE)
  out <- gsub("(?<=[0-9]),|,(?=[0-9])", ", ", out, perl = TRUE)
  out <- gsub(":+", ": ", out, perl = TRUE)
  out <- gsub("\\s+", " ", trimws(out))
  out
}

# extract_date_from_content <- function(file_path, max_lines = max_lines) {
#   # Date patterns in Spanish and English
#   date_pattern_es <- "\\b(?:\\d{1,2}\\s+(?:de\\s+)?(?:enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)\\s+(?:de\\s+)?(?:\\d{2})?\\d{2}|\\d{1,2}[\\/\\-]\\d{1,2}[\\/\\-](?:\\d{2})?\\d{2})\\b"
#   date_pattern_en <- "\\b(?:\\d{1,2}\\s+(?:January|February|March|April|May|June|July|August|September|October|November|December),?\\s+(?:\\d{2})?\\d{2})\\b|\\b(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},?\\s+(?:\\d{2})?\\d{2}\\b|\\b\\d{1,2}-(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-\\d{2,4}\\b|\\b\\d{1,2}[\\/\\-]\\d{1,2}[\\/\\-](?:\\d{2})?\\d{2}\\b"

#   normalize_months <- function(text) {
#     text <- gsub("setiembre", "septiembre", text, ignore.case = TRUE)
#     text <- gsub("\\bsep\\.?\\b", "septiembre", text, ignore.case = TRUE)
#     return(text)
#   }

#   lines <- readLines(file_path, n = max_lines, warn = FALSE)
#   lines <- iconv(lines, from = "latin1", to = "UTF-8", sub = "")

#   for (line in lines) {
#     line_cleaned <- gsub("(?<=[0-9])\\,|\\,(?=[0-9])", ", ", line, perl = TRUE)
#     line_cleaned <- gsub("(?<=[A-Za-z])\\:+|\\:+(?=[A-Za-z])", "", line_cleaned, perl = TRUE)
#     line_cleaned <- gsub(":", ": ", line_cleaned)
#     line_cleaned <- gsub("(?<=\\D)\\bde\\b(?=\\d)", "", line_cleaned, perl = TRUE)

#     # date_in_line_es <- str_extract(line_cleaned, date_pattern_es)
#     # date_in_line_en <- str_extract(line_cleaned, date_pattern_en)
#     # First, search for numeric dates (DD/MM/YYYY, DD.MM.YYYY, DD-MM-YYYY)
#     date_numeric <- str_extract(
#       line_cleaned,
#       "\\b\\d{1,2}[./-]\\d{1,2}[./-]\\d{2,4}\\b"
#     )

#     if (!is.na(date_numeric)) {
#       date_normalized <- gsub("[./-]", "/", date_numeric)
#       parsed_date <- suppressWarnings(dmy(date_normalized))

#       if (!is.na(parsed_date)) {
#         return(format(parsed_date, "%d/%m/%Y"))
#       }
#     }

#     # If no numeric date was found, try dates with month names
#     date_in_line_es <- str_extract(line_cleaned, date_pattern_es)
#     date_in_line_en <- str_extract(line_cleaned, date_pattern_en)

#     if (!is.na(date_in_line_es)) {
#       date_in_line_es <- normalize_months(date_in_line_es)
#       parsed_date <- dmy(date_in_line_es, quiet = TRUE)
#       if (!is.na(parsed_date)) return(format(parsed_date, "%d/%m/%Y"))
#     } else if (!is.na(date_in_line_en)) {
#       parsed_date <- mdy(date_in_line_en, quiet = TRUE)
#       if (!is.na(parsed_date)) return(format(parsed_date, "%d/%m/%Y"))
#     }

#   }

#   return(NA_character_)
# }

extract_date_from_content <- function(file_path, max_lines = 15L) {
  if (!file.exists(file_path)) {
    return(NA_character_)
  }

  raw_lines <- readLines(file_path, n = max_lines, warn = FALSE)
  if (length(raw_lines) == 0L) {
    return(NA_character_)
  }

  lines_utf8 <- iconv(raw_lines, from = "", to = "UTF-8", sub = " ")
  na_idx <- is.na(lines_utf8)
  if (any(na_idx)) {
    lines_utf8[na_idx] <- iconv(raw_lines[na_idx], from = "latin1", to = "UTF-8", sub = " ")
  }

  months_es <- c(
    enero = 1L, febrero = 2L, marzo = 3L, abril = 4L, mayo = 5L, junio = 6L,
    julio = 7L, agosto = 8L, septiembre = 9L, setiembre = 9L, sep = 9L,
    octubre = 10L, noviembre = 11L, diciembre = 12L
  )

  months_en <- c(
    january = 1L, jan = 1L, february = 2L, feb = 2L, march = 3L, mar = 3L,
    april = 4L, apr = 4L, may = 5L, june = 6L, jun = 6L,
    july = 7L, jul = 7L, august = 8L, aug = 8L, september = 9L, sep = 9L,
    october = 10L, oct = 10L, november = 11L, nov = 11L, december = 12L, dec = 12L
  )

  pat_num <- "(?:^|[^0-9])([0-3]?[0-9])[./]([01]?[0-9])[./]([0-9]{4}|[0-9]{2})(?:$|[^0-9])"

  pat_es_month <- paste0(
    "(?i)(?:^|[^[:alnum:]])([0-3]?[0-9])\\s+(?:de\\s+)?",
    "(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|sep\\.?",
    "|octubre|noviembre|diciembre)\\s+(?:de\\s+)?([0-9]{4}|[0-9]{2})(?:$|[^[:alnum:]])"
  )

  pat_en_dmy <- paste0(
    "(?i)(?:^|[^[:alnum:]])([0-3]?[0-9])\\s+",
    "(january|jan|february|feb|march|mar|april|apr|may|june|jun|july|jul|august|aug|",
    "september|sep|october|oct|november|nov|december|dec)\\.?\\s*,?\\s*([0-9]{4}|[0-9]{2})(?:$|[^[:alnum:]])"
  )

  pat_en_mdy <- paste0(
    "(?i)(?:^|[^[:alnum:]])",
    "(january|jan|february|feb|march|mar|april|apr|may|june|jun|july|jul|august|aug|",
    "september|sep|october|oct|november|nov|december|dec)\\.?\\s+([0-3]?[0-9]),?\\s+([0-9]{4}|[0-9]{2})(?:$|[^[:alnum:]])"
  )

  for (line in lines_utf8) {
    cleaned_line <- clean_date_line(line)

    # 1. Numeric date DD/MM/YYYY or DD.MM.YYYY
    m_num <- str_match(cleaned_line, pat_num)
    if (!all(is.na(m_num))) {
      parsed_res <- parse_date_components(m_num[, 2], m_num[, 3], m_num[, 4])
      if (!is.na(parsed_res)) return(parsed_res)
    }

    # 2. Spanish date with month name
    m_es <- str_match(cleaned_line, pat_es_month)
    if (!all(is.na(m_es))) {
      es_key <- gsub("\\.", "", tolower(m_es[, 3]))
      month_val <- unname(months_es[es_key])
      if (!is.na(month_val)) {
        parsed_res <- parse_date_components(m_es[, 2], month_val, m_es[, 4])
        if (!is.na(parsed_res)) return(parsed_res)
      }
    }

    # 3. English date (day month year)
    m_en_dmy <- str_match(cleaned_line, pat_en_dmy)
    if (!all(is.na(m_en_dmy))) {
      en_key <- gsub("\\.", "", tolower(m_en_dmy[, 3]))
      month_val <- unname(months_en[en_key])
      if (!is.na(month_val)) {
        parsed_res <- parse_date_components(m_en_dmy[, 2], month_val, m_en_dmy[, 4])
        if (!is.na(parsed_res)) return(parsed_res)
      }
    }

    # 4. English date (month day, year)
    m_en_mdy <- str_match(cleaned_line, pat_en_mdy)
    if (!all(is.na(m_en_mdy))) {
      en_key <- gsub("\\.", "", tolower(m_en_mdy[, 2]))
      month_val <- unname(months_en[en_key])
      if (!is.na(month_val)) {
        parsed_res <- parse_date_components(m_en_mdy[, 3], month_val, m_en_mdy[, 4])
        if (!is.na(parsed_res)) return(parsed_res)
      }
    }
  }

  NA_character_
}

extract_document_date <- function(file_path, max_lines = 15) {

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

timeline_plot <- plot_timeline(output_file)

save_plot_to_pdf(
    timeline_plot,
    file.path(output_dir, "timeline.pdf"),
    width = 14,
    height = 10
)


  invisible(metadata_df)

}

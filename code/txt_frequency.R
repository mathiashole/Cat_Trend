#!/usr/bin/env Rscript

# ------------------------------ Load libraries ------------------------------
library(tidyverse)
library(tidytext)
library(lubridate)

# Source the plotting script
source("code/plotting.R")

#------------------------------ Main functions -------------------------------

analyze_frequency <- function(tokens_df, output_dir, plots_dir) {

  tokens_df <- tokens_df %>%
  mutate(
    site = stringr::str_remove(
      document,
      "_\\d{2}_\\d{2}_\\d{4}$"
    )
  )

  plots_document_dir <- file.path(plots_dir, "by_document")
  plots_site_dir <- file.path(plots_dir, "by_site")


  dir.create(plots_document_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(plots_site_dir, recursive = TRUE, showWarnings = FALSE)

  cat("→ [1/3] Calculating term frequencies, totals and Zipf rankings...\n")

  #1.Count term frequency in each book/document
  # docs_words <- tokens_df %>% 
  #   count(document, word, sort = TRUE)
  docs_words_document <- tokens_df %>% 
    count(document, word, sort = TRUE)
  
  #2.Count total number of terms in each book/document
  # total_words <- docs_words %>% 
  #   group_by(document) %>% 
  #   summarize(total = sum(n))
  total_document <- docs_words_document %>% 
    group_by(document) %>% 
    summarize(total = sum(n), .groups = "drop")
  
  #Join both structures
  # docs_words <- left_join(docs_words, total_words, by = "document")
  docs_words_document <- left_join(docs_words_document, total_document, by = "document")

  docs_words_site <- tokens_df %>%
    count(site, word, sort = TRUE)

  total_site <- docs_words_site %>%
    group_by(site) %>%
    summarise(total = sum(n), .groups = "drop")

  docs_words_site <- left_join(docs_words_site, total_site, by = "site")

  #3.Top 10 words by document (Fixing deprecated top_n -> slice_max)
  # top_words_by_doc <- docs_words %>%
  #   group_by(document) %>%
  #   slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
  #   ungroup()
  top_words_by_doc <- docs_words %>%
    group_by(site) %>%
    slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
    ungroup()

  top_words_document <- docs_words_document %>%
    group_by(document) %>%
    slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
    ungroup()

  top_words_site <- docs_words_site %>%
    group_by(site) 

  # ------------------------------ Plotting Section ------------------------------

  cat("→ [2/3] Generating frequency and TF-IDF visualizations...\n")

  # Plot Top 10 of words (Fixed typos in filenames)
  top_10_plot <- plot_top_10(
    top_words_by_doc, 
    title = "Top 10 Most Frequent Words by Document", 
    xlab = "Words", 
    ylab = "Frequency"
  )
  
  save_plot_to_pdf(top_10_plot, file.path(plots_dir, "most_frequent_words.pdf"))

  # Plot term frequency distribution
  term_frequency_plot <- plot_term_frequency(
    docs_words, 
    title = "Term frequency distribution", 
    xlab = "Term frequency n/total", 
    ylab = "Count"
  )
  
  save_plot_to_pdf(term_frequency_plot, file.path(plots_dir, "term_frequency.pdf"))

  # Apply Zipf's Law and plot
  zipfs_law_plot <- plot_zipfs_law(
    docs_words, 
    title = "Zipfs law", 
    xlab = "Rank", 
    ylab = "Term frequency n/total"
  )
  
  save_plot_to_pdf(zipfs_law_plot, file.path(plots_dir, "zipfs_law.pdf"))

  # Calculate TF-IDF and plot
  # docs_words <- docs_words %>%
  #   bind_tf_idf(word, document, n)
  docs_words <- docs_words %>%
    bind_tf_idf(word, site, n)
    
  tf_idf_plot <- plot_tf_idf(
    docs_words, 
    title = "Top 10 Words by TF-IDF", 
    xlab = NULL, 
    ylab = "TF-IDF"
  )
  
  save_plot_to_pdf(tf_idf_plot, file.path(plots_dir, "tf_idf.pdf"))

  # ------------------------------ Temporal Section ------------------------------
  
  cat("→ [3/3] Exporting temporal analysis data and tables...\n")

  # MMake a temporal dataframe to plot words by year and by document
  # temporal_df <- tokens_df
  # if ("date" %in% colnames(temporal_df)) {
  #   # Extract year from date if it's in a recognizable format, otherwise try to parse it
  #   temporal_df <- temporal_df %>%
  #     mutate(year = case_when(
  #       is.numeric(date) ~ as.integer(date),
  #       TRUE ~ as.integer(lubridate::year(lubridate::dmy(as.character(date))))
  #     ))
  # }

  temporal_df <- tokens_df %>%
    mutate(year = lubridate::year(lubridate::dmy(as.character(date)))) %>%
    filter(!is.na(year))

  # Plot words by year
  word_per_year_plot <- plot_words_per_year(
    temporal_df, 
    title = "Frequency Distribution of Terms by Year", 
    xlab = "Year", 
    ylab = "Total Words"
  )
  
  save_plot_to_pdf(word_per_year_plot, file.path(plots_dir, "total_word_by_year.pdf"))

  # Plot words by year and by document
  word_per_year_and_doc_plot <- plot_words_per_year_and_doc(
    temporal_df, 
    title = "Term Frequency Distribution by Year and by Document", 
    xlab = "Year", 
    ylab = "Total Words per documents"
  )
  
  save_plot_to_pdf(word_per_year_and_doc_plot, file.path(plots_dir, "total_word_by_year_and_documents.pdf"))

  # ------------------------------ Export Data ------------------------------
  # Save the result to a file
  write.table(docs_words, file = file.path(output_dir, "documents_words_metrics.tsv"),
    row.names = FALSE, sep = "\t", quote = FALSE)

  write.table(top_words_by_doc, file = file.path(output_dir, "top_words_by_document.tsv"),
    row.names = FALSE, sep = "\t", quote = FALSE)

  # Return the data frames for potential further use
  return(invisible(list(
    docs_words = docs_words,
    top_words = top_words_by_doc,
    tf_idf = docs_words,
    frequency_table = docs_words
  )))

}

# # Main function of frequency analyze
# analyze_frequency <- function(date_hour) {
  
#   # Read data words
#   file_name <- paste0("output/words_", date_hour, ".txt")
  
#   # Load the table generated by the second script
#   data_words <- read.table(file_name, sep = "\t", header = TRUE)

#   # Inform the user about creating the output directory
#   cat("🔨 Creating output directory if it doesn't exist...\n")

#   # Save the result to a file
#   freq_dir <- "output/frequency"
#   if (!dir.exists(freq_dir)) {
#     dir.create(freq_dir, recursive = TRUE)
#   }
  
#   # Count term frequency in each book
#   docs_words <- data_words %>% 
#     count(document, word, sort = TRUE)
  
#   # Count number of terms in each book
#   total_words <- docs_words %>% 
#     group_by(document) %>% 
#     summarize(total = sum(n))
  
#   # Join both
#   docs_words <- left_join(docs_words, total_words, by = "document")

#   # Top 10 of word
#   top_words_by_doc <- docs_words %>%
#     group_by(document) %>%
#     top_n(10, wt = n) %>%
#     ungroup()

#   # Plot Top 10 of word
#   top_10_plot <- plot_top_10(top_words_by_doc, title="Top 10 Most Frequent Words by Document", xlab="Words", ylab ="Frequency")
#   save_plot_to_pdf(top_10_plot, paste0("output/frequency/most_frequen_words", date_hour, ".pdf"))

#   # Plot global word
#   top_global_plot <- plot_top_global(docs_words, title="Top 10 Most Frequent Words (All Documents)", xlab=NULL, ylab="Number of words")
#   save_plot_to_pdf(top_global_plot, paste0("output/frequency/global_most_frequen_words", date_hour, ".pdf"))

#   # Plot term frequency distribution
#   term_frequency_plot <- plot_term_frequency(docs_words, title="Term frequency distribution", xlab="Term frequency n/total", ylab="Count")
#   save_plot_to_pdf(term_frequency_plot, paste0("output/frequency/term_frequency_", date_hour, ".pdf"))
  
#   # Apply Zipf's Law and plot
#   zipfs_law_plot <- plot_zipfs_law(docs_words, title="Zipfs law", xlab="Rank", ylab="Term frequency n/total")
#   save_plot_to_pdf(zipfs_law_plot, paste0("output/frequency/zipfs_law_", date_hour, ".pdf"))
  
#   # Calculate TF-IDF and plot
#   docs_words <- docs_words %>%
#     bind_tf_idf(word, document, n)
#   tf_idf_plot <- plot_tf_idf(docs_words, title="Top 10 Words by TF-IDF", xlab=NULL, ylab="TF-IDF")
#   save_plot_to_pdf(tf_idf_plot, paste0("output/frequency/tf_idf_", date_hour, ".pdf"))

#   # Plot word by year
#   word_per_year_plot <- plot_words_per_year(data_words, title="Frequency Distribution of Terms by Year", xlab="Year", ylab="Total Words")
#   save_plot_to_pdf(word_per_year_plot, paste0("output/frequency/total_word_by_year", date_hour, ".pdf"))

#   # Plot word by year and by document
#   word_per_year_and_doc_plot <- plot_words_per_year_and_doc(data_words, title="Term Frequency Distribution by Year and by Document", xlab="Year", ylab="Total Words per documents")
#   save_plot_to_pdf(word_per_year_and_doc_plot, paste0("output/frequency/total_word_by_year_and_documents_", date_hour, ".pdf"))

# }

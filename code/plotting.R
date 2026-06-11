#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidytext)
  library(lubridate)
})

# Function to plot the frequency distribution of terms
plot_term_frequency <- function(docs_words, title="Term Frequency Distribution", xlab="Terms Frequency", ylab="Count") {
  ggplot(data = docs_words, aes(n/total, fill = document)) +
    geom_histogram(show.legend = FALSE, bins = 30) +
    facet_wrap(~document, ncol = 3, scales = "free_y") +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal()
}

# Function to apply Zipf's Law and graph
plot_zipfs_law <- function(docs_words, title="Zipf's Law", xlab="Range", ylab="Term Frequency") {
  freq_by_rank <- docs_words %>% 
  group_by(document) %>% 
  mutate(rank = row_number(), frecuencia_de_termino = n/total) %>% 
  ungroup()
  
  ggplot(freq_by_rank, aes(rank, frecuencia_de_termino, color = document)) + 
    # geom_line(size = 1, alpha = 0.8, show.legend = FALSE) + 
    geom_line(linewidth = 1, alpha = 0.8, show.legend = FALSE) +
    scale_x_log10() +
    scale_y_log10() +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal()
}

# Function to graph TF-IDF
plot_tf_idf <- function(docs_words, title="Top 10 Words by TF-IDF", xlab=NULL, ylab="tf-idf") {
  docs_words %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    # group_by(document) %>% 
    # top_n(10) %>% 
    group_by(document) %>%
    slice_max(order_by = tf_idf, n = 10, with_ties = FALSE) %>%
    ungroup() %>%
    ggplot(aes(word, tf_idf, fill = document)) +
    geom_col(show.legend = FALSE) +
    labs(x = xlab, y = ylab) +
    facet_wrap(~document, ncol = 3, scales = "free") +
    coord_flip() +
    ggtitle(title) +
    theme_minimal()
}

plot_top_10 <- function(docs_words, title="Top 10 Most Frequent Words per Document", xlab=NULL, ylab="Number of words") {
  ggplot(docs_words, aes(x = reorder_within(word, n, document), y = n, fill = document)) +
    geom_bar(stat = "identity") +
    scale_x_reordered() +
    facet_wrap(~document, ncol = 3, scales = "free") +
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle(title) +
    labs(x = xlab, y = ylab) +
    coord_flip()
}

plot_top_global <- function(docs_words, title="Top 10 Most Frequent Words (All Documents)", xlab=NULL, ylab="Number of words") {
  global_top <- docs_words %>%
    group_by(word) %>%
    summarise(total_n = sum(n), .groups = 'drop') %>%
    # arrange(desc(total_n)) %>%
    # top_n(30, total_n)
    slice_max(order_by = total_n, n = 30, with_ties = FALSE)
  
  ggplot(global_top, aes(x = reorder(word, total_n), y = total_n)) +
    geom_col(fill = "#0072B2") +
    coord_flip() +
    theme_minimal() +
    ggtitle(title) +
    labs(x = xlab, y = ylab)
}

# # Funtion to extract year from date column
# extract_year <- function(df) {
#   df %>%
#     mutate(year = year(dmy(date)))
# }

# Function to graph total words per year
plot_words_per_year <- function(df, title="Frequency Distribution of Terms by Year", xlab="Year", ylab="Total Words") {
  words_per_year <- df %>%
    extract_year() %>%
    group_by(year) %>%
    summarise(total_words = n())
  
  ggplot(words_per_year, aes(year, total_words)) +
    geom_bar(stat = "identity", fill = "#FF5A5F") +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal()
}

# Function to graph total words per year and per year
plot_words_per_year_and_doc <- function(df, title="Term Frequency Distribution by Year and by Document", xlab="Year", ylab="Total Words per documents") {
  words_per_year_and_doc <- df %>%
    extract_year() %>%
    group_by(document, year) %>%
    summarise(total_words = n(), .groups = 'drop')
  
  ggplot(words_per_year_and_doc, aes(x = factor(year), y = total_words, fill = document)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ document, ncol = 3, scales = "free") +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Feature to save a chart to PDF with fixed sizes
save_plot_to_pdf <- function(plot, filename, width = 8, height = 6) {
  ggsave(filename, plot = plot, width = width, height = height, limitsize = FALSE)
}


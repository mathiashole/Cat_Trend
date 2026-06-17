#!/usr/bin/env Rscript

# Function to plot the time line documents
plot_timeline <- function(date_table, title="Document per site", xlab="", ylab="Site") {

df_actas <- read.delim(date_table, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

colnames(df_actas) <- c("file", "dates")

df_actas <- df_actas %>%
  mutate(
    site = basename(file),                           # base name
    site = tools::file_path_sans_ext(site),         # remove extension file
    site = tolower(site),                           # change words to lower
    site = str_remove(site, "_[0-9].*$"),           # removes date or numbering if it exists after "_"
    dates = dmy(dates)                                # convert dates
  ) %>%
  filter(!is.na(dates))  # remove "Date not found"

# session count
conteo_sesiones <- df_actas %>%
  group_by(site) %>%
  summarize(n_sesiones = n())

# Date range
rango_dates <- df_actas %>%
  group_by(site) %>%
  summarize(
    inicio = min(dates),
    fin    = max(dates)
  ) %>%
  left_join(conteo_sesiones, by = "site") %>%
  arrange(desc(n_sesiones)) %>%
  mutate(site = factor(site, levels = site))

# sequence of year
anios <- seq(from = year(min(df_actas$dates)), to = year(max(df_actas$dates)), by = 1)
breaks_centrados <- as.Date(paste0(anios, "-07-01"))

# Plot
ggplot() +
  geom_segment(
    data = rango_dates,
    aes(x = inicio, xend = fin, y = site, yend = site),
    size = 10,
    color = "paleturquoise3"
  ) +
  geom_point(
    data = df_actas,
    aes(x = dates, y = site),
    color = "gray1",
    size = 2.5,
    alpha = 0.7
  ) +
  scale_x_date(
    breaks = breaks_centrados,
    date_labels = "%Y"
  ) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_line(color = "gray"),
    panel.grid.major.y = element_line(color = "gray"),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15, angle = 45, hjust = 1)
  ) +
  labs(
    title = title,
    x = "",
    y = paste0(ylab, " (n sesiones)")) +
  geom_text(
    data = conteo_sesiones,
    aes(x = max(df_actas$dates) + 200, y = site,
        label = n_sesiones),
    hjust = 0,
    size = 5
  ) 
}
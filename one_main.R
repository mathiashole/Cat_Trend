# ANÁLISIS DE TEXTOS - VERSIÓN INTERACTIVA PARA RSTUDIO
# ------------------------------------------------------------------------------
# Autor: Sofia Bausero
# Fecha: Junio 2024 
# ==============================================================================
# 1. CONFIGURACIÓN INICIAL (Ejecutar primero)
# ==============================================================================

# Instalar y cargar paquetes necesarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(stringr, lubridate, tidytext, stopwords, tidyverse, ggplot2)

# Configuración manual (modifica estos valores)
config <- list(
  directory = "/home/mathias/study/bausero/actasal2024",        # Ruta a tu carpeta con archivos .txt
  keywords_file = NULL,        # Ruta a archivo de palabras clave (opcional)
  language = "SP",             # "SP" para español, "EN" para inglés
  ngram_number = 2             # 1, 2 o 3 para unigramas, bigramas o trigramas
)

# Crear marca de tiempo para los archivos de salida
date_hour <- format(Sys.time(), "%d-%m-%Y_%H:%M")

# ==============================================================================
# 2. FUNCIONES AUXILIARES (Ejecutar para cargar todas las funciones)
# ==============================================================================

# Función para normalizar nombres de documentos
normalize_document_name <- function(name) {
  name <- tolower(name)
  name <- tools::file_path_sans_ext(name)
  name <- gsub("[^a-z]", " ", name)
  name <- unlist(strsplit(name, "\\s+"))
  name <- name[nchar(name) > 2]
  if (length(name) == 0) return(NA)
  name <- name[which.max(nchar(name))]
  return(name)
}

# Función para extraer fechas de archivos
find_date_in_file <- function(file_path) {
  date_pattern_es <- "\\b(?:\\d{1,2}\\s+(?:de\\s+)?(?:enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)\\s+(?:de\\s+)?(?:\\d{2})?\\d{2}|\\d{1,2}[\\/\\-]\\d{1,2}[\\/\\-](?:\\d{2})?\\d{2})\\b"
  date_pattern_en <- "\\b(?:\\d{1,2}\\s+(?:January|February|March|April|May|June|July|August|September|October|November|December),?\\s+(?:\\d{2})?\\d{2})\\b|\\b(?:January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{1,2},?\\s+(?:\\d{2})?\\d{2}\\b|\\b\\d{1,2}-(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-\\d{2,4}\\b|\\b\\d{1,2}[\\/\\-]\\d{1,2}[\\/\\-](?:\\d{2})?\\d{2}\\b"

  lines <- readLines(file_path, n = 10, warn = FALSE)
  lines <- iconv(lines, from = "latin1", to = "UTF-8", sub = "�")
  
  date_found <- NA
  
  for (line in lines) {
    line_cleaned <- gsub("(?<=[0-9])\\,|\\,(?=[0-9])", ", ", line, perl = TRUE)
    line_cleaned <- gsub("(?<=[A-Za-z])\\:+|\\:+(?=[A-Za-z])", "", line_cleaned, perl = TRUE)
    line_cleaned <- gsub(":", ": ", line_cleaned)
    line_cleaned <- gsub("(?<=\\D)\\bde\\b(?=\\d)", "", line_cleaned, perl = TRUE)
    
    date_in_line_es <- str_extract(line_cleaned, date_pattern_es)
    date_in_line_en <- str_extract(line_cleaned, date_pattern_en)
    
    if (!is.na(date_in_line_es)) {
      parsed_date <- dmy(date_in_line_es, quiet = TRUE)
      if (!is.na(parsed_date)) {
        date_found <- format(parsed_date, "%d/%m/%Y")
        break
      }
    } else if (!is.na(date_in_line_en)) {
      parsed_date <- mdy(date_in_line_en, quiet = TRUE)
      if (!is.na(parsed_date)) {
        date_found <- format(parsed_date, "%d/%m/%Y")
        break
      }
    }
  }
  
  return(ifelse(is.na(date_found), "Date not found", date_found))
}

# Función para generar n-gramas
# generate_ngrams <- function(infoText, n, stopwords_list) {
#   tokens <- infoText %>%
#     unnest_tokens(word, text, token = "ngrams", n = n) %>%
#     separate(word, into = paste0("word", 1:n), sep = " ") %>%
#     filter(across(starts_with("word"), ~ !grepl("\\d+", .) & !(tolower(.) %in% stopwords_list))) %>%
#     unite(word, starts_with("word"), sep = " ")
  
#   return(tokens)
# }

generate_ngrams <- function(infoText, n, stopwords_list) {
  tokens <- infoText %>%
    unnest_tokens(word, text, token = "ngrams", n = n) %>%
    separate(word, into = paste0("word", 1:n), sep = " ", remove = TRUE) %>%
    filter(across(paste0("word", 1:n), ~ !is.na(.) & . != "")) %>%
    filter(across(paste0("word", 1:n), ~ !grepl("\\d+", .) & !(tolower(.) %in% stopwords_list))) %>%
    unite(word, starts_with("word"), sep = " ", remove = TRUE) %>%
    filter(!is.na(word), str_trim(word) != "")
  
  return(tokens)
}

# ==============================================================================
# 3. PROCESAMIENTO DE FECHAS (Ejecutar para extraer fechas)
# ==============================================================================

cat("\n📅 Extrayendo fechas de los documentos...\n")

# Verificar directorio
if (!file.exists(config$directory)) {
  stop("El directorio especificado no existe")
}

# Verificar archivos .txt
if (length(list.files(config$directory, pattern = "\\.txt$", full.names = TRUE)) == 0) {
  stop("No se encontraron archivos .txt en el directorio especificado")
}

# Generar tabla con fechas
files <- list.files(path = config$directory, pattern = "\\.txt$", full.names = TRUE)
tabla_datos <- t(sapply(files, function(file) {
  c(file, find_date_in_file(file))
}))

# Crear directorio de salida si no existe
if (!dir.exists("output")) {
  dir.create("output", recursive = TRUE)
}

# Guardar tabla de datos
write.table(tabla_datos, 
            file = paste0("output/data_table_", date_hour, ".txt"), 
            sep = "\t", 
            row.names = FALSE, 
            col.names = FALSE, 
            quote = FALSE)

cat("✅ Tabla de fechas generada en: output/data_table_", date_hour, ".txt\n", sep = "")

# ==============================================================================
# 4. PROCESAMIENTO DE TEXTOS (Ejecutar para tokenizar los textos)
# ==============================================================================

cat("\n📝 Procesando textos y generando n-gramas...\n")

# Leer tabla de datos generada
data_table <- read.table(paste0("output/data_table_", date_hour, ".txt"), 
                        sep = "\t", 
                        header = FALSE, 
                        col.names = c("document", "date"))

# Procesar nombres de documentos
data_table$date <- gsub("/", "-", data_table$date)
data_table$document <- basename(data_table$document)
data_table$origin_document <- data_table$document
data_table$document <- sapply(data_table$document, normalize_document_name)
data_table$document <- gsub("\\.[^.]+$", "", data_table$document)

# Crear estructura para almacenar textos
infoText <- tibble(
  document = character(),
  date = character(),
  paragraph = numeric(),
  text = character()
)

# Procesar cada documento
for (i in seq_along(data_table$origin_document)) {
  file_path <- file.path(config$directory, data_table$origin_document[i])
  speech <- readLines(file_path)
  
  temporal <- tibble(
    document = data_table$document[i],
    date = data_table$date[i],
    paragraph = seq_along(speech),
    text = speech
  )
  
  infoText <- bind_rows(infoText, temporal)
}

# Factorizar documentos y fechas
infoText <- infoText %>%
  mutate(document = factor(document, levels = unique(document)),
         date = factor(date))

# Cargar stopwords según idioma
if (tolower(config$language) == "sp") {
  stopwords_list <- stopwords(language = "es", source = "stopwords-iso") # nltk
} else if (tolower(config$language) == "en") {
  stopwords_list <- stopwords(language = "en", source = "stopwords-iso") # nltk
} else {
  stop("Idioma no válido. Usar 'SP' o 'EN'")
}

# Generar n-gramas según configuración
if (config$ngram_number == 1) {
  infoText_token <- infoText %>%
    unnest_tokens(word, text) %>%
    filter(!grepl("\\d+", word) & !(tolower(word) %in% stopwords_list))
} else if (config$ngram_number %in% 2:3) {
  infoText_token <- generate_ngrams(infoText, config$ngram_number, stopwords_list)
} else {
  stop("Número de n-grama no válido. Usar 1, 2 o 3")
}

# Guardar tokens
write.table(infoText_token, 
            file = paste0("output/words_", date_hour, ".txt"), 
            row.names = FALSE, 
            col.names = TRUE, 
            sep = "\t", 
            quote = FALSE)

cat("✅ Tokens guardados en: output/words_", date_hour, ".txt\n", sep = "")

# ==============================================================================
# 5. ANÁLISIS DE FRECUENCIAS (Ejecutar para análisis estadístico)
# ==============================================================================

cat("\n📊 Analizando frecuencias...\n")

# Leer datos tokenizados
data_words <- read.table(paste0("output/words_", date_hour, ".txt"), 
                        sep = "\t", 
                        header = TRUE)

# Crear directorio para gráficos si no existe
if (!dir.exists("output/frequency")) {
  dir.create("output/frequency", recursive = TRUE)
}

# Calcular frecuencias
docs_words <- data_words %>% 
  count(document, word, sort = TRUE)

total_words <- docs_words %>% 
  group_by(document) %>% 
  summarize(total = sum(n))

docs_words <- left_join(docs_words, total_words, by = "document")

# ==============================================================================
# 6. VISUALIZACIONES (Ejecutar para generar gráficos)
# ==============================================================================

cat("\n📈 Generando visualizaciones...\n")

# Gráfico 1: Top 10 palabras por documento
top_words_by_doc <- docs_words %>%
  group_by(document) %>%
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
  ungroup()

p1 <- ggplot(top_words_by_doc, aes(x = reorder_within(word, n, document), y = n, fill = document)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~document, ncol = 3, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Top 10 palabras más frecuentes por documento",
       x = "Palabra",
       y = "Frecuencia") +
  coord_flip()

print(p1)
ggsave(paste0("output/frequency/top10_words_", date_hour, ".pdf"), p1, width = 10, height = 7)

# Gráfico 2: Distribución de frecuencia de términos
p2 <- ggplot(docs_words, aes(n/total, fill = document)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~document, ncol = 3, scales = "free_y") +
  labs(title = "Distribución de frecuencia de términos",
       x = "Frecuencia relativa (n/total)",
       y = "Conteo") +
  theme_minimal()

print(p2)
ggsave(paste0("output/frequency/term_frequency_", date_hour, ".pdf"), p2, width = 10, height = 7)

# Gráfico 3: Ley de Zipf
freq_by_rank <- docs_words %>% 
  group_by(document) %>% 
  mutate(rank = row_number(), frecuencia_de_termino = n/total)

p3 <- ggplot(freq_by_rank, aes(rank, frecuencia_de_termino, color = document)) + 
  geom_line(size = 1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Ley de Zipf",
       x = "Rango (log)",
       y = "Frecuencia relativa (log)") +
  theme_minimal()

print(p3)
ggsave(paste0("output/frequency/zipfs_law_", date_hour, ".pdf"), p3, width = 10, height = 7)

# Gráfico 4: TF-IDF
docs_words_tfidf <- docs_words %>%
  bind_tf_idf(word, document, n)

p4 <- docs_words_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(document) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, 
       y = "TF-IDF",
       title = "Top 10 palabras por TF-IDF") +
  facet_wrap(~document, ncol = 3, scales = "free") +
  coord_flip() +
  theme_minimal()

print(p4)
ggsave(paste0("output/frequency/tf_idf_", date_hour, ".pdf"), p4, width = 10, height = 7)

# Gráfico 5: Palabras por año
data_words_with_year <- data_words %>%
  mutate(year = year(dmy(date)))

words_per_year <- data_words_with_year %>%
  group_by(year) %>%
  summarise(total_words = n())

p5 <- ggplot(words_per_year, aes(year, total_words)) +
  geom_bar(stat = "identity", fill = "#FF5A5F") +
  labs(title = "Distribución de términos por año",
       x = "Año",
       y = "Total de palabras") +
  theme_minimal()

print(p5)
ggsave(paste0("output/frequency/words_per_year_", date_hour, ".pdf"), p5, width = 8, height = 6)

# Gráfico 6: Palabras por año y documento
words_per_year_and_doc <- data_words_with_year %>%
  group_by(document, year) %>%
  summarise(total_words = n(), .groups = 'drop')

p6 <- ggplot(words_per_year_and_doc, aes(x = factor(year), y = total_words, fill = document)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ document, ncol = 3, scales = "free") +
  labs(title = "Distribución de términos por año y documento",
       x = "Año",
       y = "Total de palabras") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p6)
ggsave(paste0("output/frequency/words_per_year_doc_", date_hour, ".pdf"), p6, width = 12, height = 8)

cat("\n🎉 ¡Análisis completado con éxito!\n")
cat("📁 Los resultados se guardaron en la carpeta 'output/'\n")

####  LÍNEA DEL TIEMPO

data_linea <- data_table %>%
  mutate(
    document = str_to_lower(str_trim(document)),
    date = lubridate::dmy(date)
  ) %>%
  group_by(document, date) %>%
  slice(1) %>%  # solo la primera fila de cada grupo
  ungroup()


# Diccionario de nombres corregidos
nombres_corregidos <- c(
  "oceano" = "Zona Oceánica",
  "lagunas" = "Lagunas Costeras",
  "bellaunion" = "Bella Unión",
  "merin" = "Merín",
  "montevideo" = "Montevideo",
  "sanjose" = "San José",
  "villasoriano" = "Villa Soriano",
  "salto" = "Salto",
  "andresito" = "Andresito",
  "costa" = "La Costa",
  "piria" = "Piriápolis",
  "pde" = "Punta del Este",
  "lcbc" = "La Coronilla-Barra del Chuy",
  "bonete" = "Rincón del Bonete"
)

# Contar la cantidad de sesiones por consejo
conteo_sesiones <- data_linea %>%
  group_by(document) %>%
  summarize(n_sesiones = n())

# Obtener el primer y último registro de cada consejo
rango_fechas <- data_linea %>%
  group_by(document) %>%
  summarize(
    inicio = min(date),
    fin = max(date)
  ) %>%
  left_join(conteo_sesiones, by = "document") %>%
  arrange(desc(n_sesiones)) %>%
  mutate(document = factor(document, levels = document))  # Mantener el orden

# Reemplazar nombres en la columna 'consejo'
rango_fechas$document <- recode(rango_fechas$document, !!!nombres_corregidos)
data_linea$document <- recode(data_linea$document, !!!nombres_corregidos)

# Crear el gráfico con líneas en los ejes
ggplot() +
  # Barras horizontales desde la primera hasta la última sesión
  geom_segment(data = rango_fechas, 
               aes(x = inicio, xend = fin, y = document, yend = document),
               size = 10, color = "paleturquoise3") +
  # Puntos para cada sesión individual
  geom_point(data = data_linea, 
             aes(x = date, y = document), 
             color = "gray18", size = 2, alpha = 0.7) +
  # Configurar el eje X para mostrar todos los años
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # Agregar líneas de referencia en los ejes X e Y
  theme_light() +
  theme(
    panel.grid.major.x = element_line(color = "gray98"),
    panel.grid.major.y = element_line(color = "gray98"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(title = "",
       x = "",
       y = "Fishery Council")

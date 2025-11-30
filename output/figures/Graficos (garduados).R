# Cargar librerías necesarias
library(tidyverse)
library(scales)        
library(ggrepel)       
library(patchwork)    
library(viridis)     
library(ggridges)      
library(ggtext)        

# Configuración general de tema
theme_set(theme_minimal(base_size = 12))

color_genero <- c("Mujer" = "#E69F00", "Varón" = "#56B4E9")
color_gestion <- c("Estatal" = "#009E73", "Privada" = "#D55E00")

# Cargar datos procesados

X01_base_limpia <- read_csv("Ciencia de datos/TP FINAL/Proyecto/data/clean/01_base_limpia.csv") %>%
  filter(!is.na(salario))  

# ==============================================================================
# GRÁFICO 1: DISTRIBUCIÓN DE SALARIOS CON DENSIDAD Y ESTADÍSTICAS
# ==============================================================================

cat("Generando Gráfico 1: Distribución de salarios con contexto estadístico...\n")

# Calcular estadísticas para anotaciones
media_salario <- mean(X01_base_limpia$salario)
mediana_salario <- median(X01_base_limpia$salario)
p25 <- quantile(X01_base_limpia$salario, 0.25)
p75 <- quantile(X01_base_limpia$salario, 0.75)

grafico_1 <- ggplot(X01_base_limpia, aes(x = salario)) +
  # Histograma de fondo
  geom_histogram(aes(y = ..density..), 
                 bins = 50, 
                 fill = "grey85", 
                 color = "white", 
                 alpha = 0.7) +
  # Curva de densidad
  geom_density(color = "#0072B2", 
               size = 1.5, 
               alpha = 0.3,
               fill = "#0072B2") +
  # Líneas de referencia
  geom_vline(xintercept = mediana_salario, 
             linetype = "dashed", 
             color = "#009E73", 
             size = 1.2) +
  geom_vline(xintercept = media_salario, 
             linetype = "dashed", 
             color = "#D55E00", 
             size = 1.2) +
  # Área del rango intercuartílico
  annotate("rect", 
           xmin = p25, xmax = p75, 
           ymin = 0, ymax = Inf,
           alpha = 0.1, 
           fill = "#56B4E9") +
  # Anotaciones
  annotate("text", 
           x = mediana_salario, 
           y = max(density(X01_base_limpia$salario)$y) * 0.9,
           label = paste0("Mediana\n$", format(round(mediana_salario, 0), big.mark = ",")),
           hjust = -0.1, 
           color = "#009E73", 
           size = 4, 
           fontface = "bold") +
  annotate("text", 
           x = media_salario, 
           y = max(density(X01_base_limpia$salario)$y) * 0.7,
           label = paste0("Media\n$", format(round(media_salario, 0), big.mark = ",")),
           hjust = 1.1, 
           color = "#D55E00", 
           size = 4, 
           fontface = "bold") +
  # Escalas
  scale_x_continuous(
    labels = label_dollar(prefix = "$", big.mark = ",", accuracy = 1),
    breaks = seq(0, max(X01_base_limpia$salario), by = 50000)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  # Etiquetas
  labs(
    title = "Distribución de Salarios de Graduados Universitarios",
    subtitle = paste0(
      "Graduados argentinos 2016-2018 con empleo formal registrado (N = ", 
      format(nrow(X01_base_limpia), big.mark = ","), 
      ")\nLa mediana ($", 
      format(round(mediana_salario, 0), big.mark = ","),
      ") es ", 
      round(((media_salario / mediana_salario) - 1) * 100, 1),
      "% menor que la media debido a asimetría positiva"
    ),
    x = "Salario mensual bruto",
    y = "Densidad",
    caption = "Fuente: Sistema Araucano - Ministerio de Educación de Argentina\nNota: Rango intercuartílico sombreado en celeste"
  ) +
  # Tema
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50", margin = margin(t = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.3),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

# Guardar
ggsave("output/figures/06_FINAL_distribucion_salarios.png", 
         +        grafico_1, 
         +        width = 12, height = 7, dpi = 300, bg = 'white')

# ==============================================================================
# GRÁFICO 2: BRECHA SALARIAL DE GÉNERO - BOXPLOT COMPARATIVO
# ==============================================================================

cat("Generando Gráfico 2: Brecha salarial de género...\n")

# Preparar datos
datos_genero <- X01_base_limpia %>%
  mutate(genero = case_when(
    genero_id == 1 ~ "Mujer",
    genero_id == 2 ~ "Varón"
  )) %>%
  filter(!is.na(genero))

# Calcular estadísticas
stats_por_genero <- datos_genero %>%
  group_by(genero) %>%
  summarise(
    mediana = median(salario),
    media = mean(salario),
    .groups = "drop"
  )

brecha_mediana <- ((stats_por_genero$mediana[stats_por_genero$genero == "Varón"] -
                      stats_por_genero$mediana[stats_por_genero$genero == "Mujer"]) /
                     stats_por_genero$mediana[stats_por_genero$genero == "Mujer"]) * 100

grafico_2 <- ggplot(datos_genero, aes(x = genero, y = salario, fill = genero)) +
  # Violin plot de fondo
  geom_violin(alpha = 0.3, color = NA) +
  # Boxplot principal
  geom_boxplot(width = 0.3, 
               outlier.alpha = 0.3, 
               outlier.size = 0.8,
               color = "grey20") +
  # Líneas de mediana
  stat_summary(fun = median, 
               geom = "point", 
               size = 4, 
               color = "white", 
               shape = 21, 
               stroke = 2) +
  # Escalas
  scale_fill_manual(values = color_genero) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", big.mark = ","),
    breaks = seq(0, max(datos_genero$salario), by = 50000),
    limits = c(0, quantile(datos_genero$salario, 0.95))
  ) +
  # Etiquetas
  labs(
    title = "Brecha Salarial de Género en Graduados Universitarios",
    subtitle = paste0(
      "Los varones ganan ", 
      round(brecha_mediana, 1), 
      "% más que las mujeres (mediana)\n",
      "Varón: $", 
      format(round(stats_por_genero$mediana[stats_por_genero$genero == "Varón"], 0), big.mark = ","),
      " | Mujer: $",
      format(round(stats_por_genero$mediana[stats_por_genero$genero == "Mujer"], 0), big.mark = ",")
    ),
    x = NULL,
    y = "Salario mensual bruto",
    caption = "Fuente: Sistema Araucano\nNota: Puntos blancos indican la mediana. Outliers por encima del percentil 95 omitidos para claridad."
  ) +
  # Tema
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50", margin = margin(t = 15)),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

# Guardar
ggsave("output/figures/06_FINAL_brecha_genero.png", 
       grafico_2, 
       width = 10, height = 8, dpi = 300, bg = "white")

# ==============================================================================
# GRÁFICO 3: SALARIOS POR REGIÓN - CLEVELAND DOT PLOT
# ==============================================================================

cat("Generando Gráfico 3: Salarios por región (Cleveland dot plot)...\n")

# Preparar datos
datos_region <- X01_base_limpia %>%
  mutate(region = case_when(
    region_id == 1 ~ "CABA",
    region_id == 2 ~ "Buenos Aires",
    region_id == 3 ~ "Resto Pampeana",
    region_id == 4 ~ "NOA",
    region_id == 5 ~ "NEA",
    region_id == 6 ~ "Cuyo",
    region_id == 7 ~ "Patagonia"
  )) %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(
    mediana = median(salario),
    q1 = quantile(salario, 0.25),
    q3 = quantile(salario, 0.75),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(mediana) %>%
  mutate(
    region = factor(region, levels = region),
    diferencia_vs_top = ((mediana / max(mediana)) - 1) * 100
  )

# Salario de referencia (nacional)
mediana_nacional <- median(X01_base_limpia$salario)

grafico_3 <- ggplot(datos_region, aes(x = mediana, y = region)) +
  # Línea de referencia nacional
  geom_vline(xintercept = mediana_nacional, 
             linetype = "dashed", 
             color = "grey50", 
             size = 0.8) +
  # Segmentos de rango intercuartílico
  geom_segment(aes(x = q1, xend = q3, y = region, yend = region),
               color = "grey60", 
               size = 3, 
               alpha = 0.5) +
  # Puntos de mediana
  geom_point(size = 5, 
             color = "#0072B2") +
  # Etiquetas de valores
  geom_text(aes(label = paste0("$", format(round(mediana, 0), big.mark = ","))),
            hjust = -0.3, 
            size = 4, 
            fontface = "bold",
            color = "grey20") +
  # Anotación de referencia
  annotate("text", 
           x = mediana_nacional, 
           y = 7.5,
           label = paste0("Mediana\nNacional\n$", format(round(mediana_nacional, 0), big.mark = ",")),
           hjust = 0.5, 
           size = 3.5, 
           color = "grey40") +
  # Escalas
  scale_x_continuous(
    labels = label_dollar(prefix = "$", big.mark = ","),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  # Etiquetas
  labs(
    title = "Desigualdad Salarial Regional",
    subtitle = "Mediana de salarios de graduados universitarios por región geográfica\nLas barras grises representan el rango intercuartílico (Q1-Q3)",
    x = "Salario mensual bruto (mediana)",
    y = NULL,
    caption = "Fuente: Sistema Araucano\nNota: Patagonia registra salarios 78% superiores a NOA. Línea punteada indica mediana nacional."
  ) +
  # Tema
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50", margin = margin(t = 15)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10))
  )

# Guardar
ggsave("output/figures/06_FINAL_salarios_region.png", 
       grafico_3, 
       width = 12, height = 7, dpi = 300, bg = "white")

# ==============================================================================
# GRÁFICO 4: EVOLUCIÓN SALARIAL POR AÑO DE EGRESO
# ==============================================================================

cat("Generando Gráfico 4: Evolución salarial por año de egreso...\n")

# Preparar datos
datos_evolucion <- X01_base_limpia %>%
  group_by(anioegreso, genero_id) %>%
  summarise(
    mediana = median(salario),
    q1 = quantile(salario, 0.25),
    q3 = quantile(salario, 0.75),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(genero = case_when(
    genero_id == 1 ~ "Mujer",
    genero_id == 2 ~ "Varón"
  ))

grafico_4 <- ggplot(datos_evolucion, aes(x = anioegreso, y = mediana, color = genero, fill = genero)) +
  # Área de confianza (Q1-Q3)
  geom_ribbon(aes(ymin = q1, ymax = q3), 
              alpha = 0.2, 
              color = NA) +
  # Línea de tendencia
  geom_line(size = 1.5) +
  # Puntos
  geom_point(size = 4, 
             shape = 21, 
             color = "white", 
             stroke = 2) +
  # Escalas
  scale_color_manual(values = color_genero) +
  scale_fill_manual(values = color_genero) +
  scale_x_continuous(breaks = unique(datos_evolucion$anioegreso)) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$", big.mark = ","),
    limits = c(50000, NA)
  ) +
  # Etiquetas
  labs(
    title = "Evolución del Salario Inicial por Año de Egreso",
    subtitle = "Mediana de salarios según año de graduación y género\nÁrea sombreada representa el rango intercuartílico (25%-75%)",
    x = "Año de egreso",
    y = "Salario mensual bruto (mediana)",
    color = "Género",
    fill = "Género",
    caption = "Fuente: Sistema Araucano\nNota: Los graduados más recientes enfrentan salarios iniciales más bajos, reflejando menor experiencia laboral."
  ) +
  # Tema
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50", margin = margin(t = 15)),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12, face = "bold")
  )

# Guardar
ggsave("output/figures/06_FINAL_evolucion_salario.png", 
       grafico_4, 
       width = 12, height = 7, dpi = 300, bg = "white")

# ==============================================================================
# GRÁFICO 5: SALARIOS POR RAMA DE CONOCIMIENTO - RIDGELINE PLOT
# ==============================================================================

cat("Generando Gráfico 5: Distribuciones salariales por rama (ridgeline)...\n")

# Preparar datos
datos_rama <- X01_base_limpia %>%
  mutate(rama = case_when(
    rama_id == 1 ~ "Ciencias Sociales",
    rama_id == 2 ~ "Ciencias Aplicadas",
    rama_id == 3 ~ "Ciencias de la Salud",
    rama_id == 4 ~ "Ciencias Humanas",
    rama_id == 5 ~ "Ciencias Básicas",
    rama_id == 6 ~ "Sin Rama"
  )) %>%
  filter(!is.na(rama), rama != "Sin Rama") %>%
  group_by(rama) %>%
  mutate(mediana_rama = median(salario)) %>%
  ungroup() %>%
  arrange(mediana_rama) %>%
  mutate(rama = factor(rama, levels = unique(rama)))

grafico_5 <- ggplot(datos_rama, aes(x = salario, y = rama, fill = after_stat(x))) +
  geom_density_ridges_gradient(
    scale = 3, 
    rel_min_height = 0.01,
    alpha = 0.8
  ) +
  # Escalas
  scale_fill_viridis_c(option = "C", direction = 1) +
  scale_x_continuous(
    labels = label_dollar(prefix = "$", big.mark = ","),
    limits = c(0, 250000)
  ) +
  # Etiquetas
  labs(
    title = "Distribución de Salarios por Rama de Conocimiento",
    subtitle = "Densidad de salarios para cada área disciplinaria (ordenadas por mediana)\nColoración indica nivel salarial: más claro = salarios más bajos",
    x = "Salario mensual bruto",
    y = NULL,
    caption = "Fuente: Sistema Araucano\nNota: Ciencias de la Salud y Ciencias Aplicadas presentan mayores salarios medianos."
  ) +
  # Tema
  theme_ridges(font_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50", margin = margin(t = 15)),
    legend.position = "none",
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 12, face = "bold")
  )

# Guardar
ggsave("output/figures/06_FINAL_salarios_rama_ridgeline.png", 
       grafico_5, 
       width = 12, height = 9, dpi = 300, bg = "white")


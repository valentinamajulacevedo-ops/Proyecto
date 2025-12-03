
library(tidyverse)
library(scales)
library(patchwork)
library(ggtext)

col_mujer <- "#ff66cc"   # Rosa fuerte
col_hombre <- "#66ccff"  # Celeste claro
fondo <- "#000A23"       # Azul oscuro
texto <- "white"

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = fondo, color = NA),
      panel.background = element_rect(fill = fondo, color = NA),
      panel.grid.major = element_line(color = "gray30"),
      panel.grid.minor = element_blank(),
      text = element_text(color = texto),
      axis.text = element_text(color = texto),
      legend.text = element_text(color = texto),
      legend.title = element_text(color = texto),
      plot.title = element_text(face = "bold", color = texto),
      plot.subtitle = element_text(color = texto),
      plot.caption = element_text(color = texto)
    )
)

# Configuración de rutas
data_clean <- 'data/clean'
output_figures <- 'output/figures'

# Cargar datos limpios
base_graduados <- read_csv("Ciencia de datos/TP FINAL/Proyecto/data/clean/01_base_limpia.csv") %>%
  filter(!is.na(salario))


# Crear etiquetas descriptivas
base_graduados <- base_graduados %>%
  mutate(
    genero = case_when(
      genero_id == 1 ~ "Mujeres",
      genero_id == 2 ~ "Varones",
      TRUE ~ "Otro"
    ),
    gestion = case_when(
      gestion_id == 1 ~ "Estatal",
      gestion_id == 2 ~ "Privada",
      TRUE ~ "Otra"
    ),
    region = case_when(
      region_id == 1 ~ "CABA",
      region_id == 2 ~ "Buenos Aires",
      region_id == 3 ~ "Resto Pampeana",
      region_id == 4 ~ "NOA",
      region_id == 5 ~ "NEA",
      region_id == 6 ~ "Cuyo",
      region_id == 7 ~ "Patagonia",
      TRUE ~ "Otra"
    ),
    tamano = case_when(
      tamaño_id == 1 ~ "Micro\n(< 10)",
      tamaño_id == 2 ~ "Pequeña\n(10-49)",
      tamaño_id == 3 ~ "Mediana\n(50-199)",
      tamaño_id == 4 ~ "Grande\n(≥ 200)",
      TRUE ~ "Otra"
    )
  )

# ==============================================================================
# GRÁFICO 1 — BRECHA SALARIAL
# ==============================================================================

cat("\n=== GENERANDO GRÁFICO 1: BRECHA SALARIAL DE GÉNERO ===\n")

# Calcular estadísticas por género
stats_genero <- base_graduados %>%
  filter(genero %in% c("Mujeres", "Varones")) %>%
  group_by(genero) %>%
  summarise(
    n = n(),
    media = mean(salario),
    mediana = median(salario),
    q1 = quantile(salario, 0.25),
    q3 = quantile(salario, 0.75),
    .groups = "drop"
  )

brecha_media <- ((stats_genero$media[stats_genero$genero == "Varones"] - 
                    stats_genero$media[stats_genero$genero == "Mujeres"]) /
                   stats_genero$media[stats_genero$genero == "Mujeres"]) * 100

brecha_mediana <- ((stats_genero$mediana[stats_genero$genero == "Varones"] - 
                      stats_genero$mediana[stats_genero$genero == "Mujeres"]) /
                     stats_genero$mediana[stats_genero$genero == "Mujeres"]) * 100


# Gráfico 1A — Distribuciones
grafico_1a <- base_graduados %>%
  filter(genero %in% c("Mujeres", "Varones")) %>%
  ggplot(aes(x = salario, fill = genero)) +
  geom_density(alpha = 0.6, color = NA) +
  geom_vline(data = stats_genero, aes(xintercept = mediana, color = genero),
             linetype = "dashed", size = 1.2) +
  scale_fill_manual(values = c("Mujeres" = col_mujer, "Varones" = col_hombre)) +
  scale_color_manual(values = c("Mujeres" = col_mujer, "Varones" = col_hombre)) +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(x = NULL, y = "Densidad")


# Gráfico 1B — Barras
grafico_1b <- stats_genero %>%
  select(genero, media, mediana) %>%
  pivot_longer(cols = c(media, mediana),
               names_to = "estadistica",
               values_to = "valor") %>%
  mutate(estadistica = recode(estadistica, media = "Media", mediana = "Mediana")) %>%
  ggplot(aes(x = estadistica, y = valor, fill = genero)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = dollar(valor, prefix = "$", big.mark = ",")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3.5, fontface = "bold", color = texto) +
  scale_fill_manual(values = c("Mujeres" = col_mujer, "Varones" = col_hombre)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(x = NULL, y = "Salario mensual")


grafico_1_completo <- grafico_1a / grafico_1b +
  plot_layout(heights = c(1.5, 1)) +
  plot_annotation(
    title = "La Brecha Salarial de Género en Graduados Universitarios Argentinos",
    subtitle = sprintf(
      "Los varones ganan un %.1f%% más que las mujeres en promedio (%.1f%% en mediana)",
      brecha_media, brecha_mediana
    ),
    caption = "Fuente: Sistema Araucano (2019-2021).",
    theme = theme(
      plot.background = element_rect(fill = fondo, color = NA),
      plot.title = element_text(color = texto),
      plot.subtitle = element_text(color = texto),
      plot.caption = element_text(color = texto)
    )
  )

print(grafico_1_completo)

ggsave(
  filename = file.path(output_figures, "01_brecha_salarial_genero_editorializado.png"),
  plot = grafico_1_completo,
  width = 12,
  height = 10,
  dpi = 300,
  bg = fondo
)

cat("✓ Gráfico 1 guardado exitosamente\n\n")


# ==============================================================================
# GRÁFICO 2 — REGIONES Y TAMAÑO DE EMPRESA
# ==============================================================================

cat("=== GENERANDO GRÁFICO 2: DESIGUALDAD REGIONAL Y EMPRESARIAL ===\n")

# Medianas por región
stats_region_genero <- base_graduados %>%
  filter(genero %in% c("Mujeres", "Varones")) %>%
  group_by(region, genero) %>%
  summarise(n = n(), mediana = median(salario), .groups = "drop")


grafico_2a <- stats_region_genero %>%
  mutate(region = fct_reorder(region, mediana, .fun = mean)) %>%
  ggplot(aes(x = region, y = mediana, fill = genero)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = dollar(mediana, prefix = "$")),
            position = position_dodge(width = 0.8),
            hjust = -0.1, size = 3, fontface = "bold", color = texto) +
  scale_fill_manual(values = c("Mujeres" = col_mujer, "Varones" = col_hombre)) +
  coord_flip() +
  labs(title = "Desigualdad Salarial por Región", x = NULL, y = "Salario mediano")


# Medianas por tamaño de empresa
stats_tamano_genero <- base_graduados %>%
  filter(genero %in% c("Mujeres", "Varones"), !is.na(tamano)) %>%
  group_by(tamano, genero) %>%
  summarise(n = n(), mediana = median(salario), .groups = "drop")


grafico_2b <- stats_tamano_genero %>%
  ggplot(aes(x = tamano, y = mediana, fill = genero)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = dollar(mediana, prefix = "$")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3, fontface = "bold", color = texto) +
  scale_fill_manual(values = c("Mujeres" = col_mujer, "Varones" = col_hombre)) +
  labs(title = "Desigualdad según Tamaño de Empresa", x = NULL, y = "Salario mediano")


grafico_2_completo <- grafico_2a + grafico_2b +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    title = "Geografía y Escala Empresarial: Factores Clave en la Desigualdad Salarial",
    subtitle = "CABA y Patagonia lideran en salarios; las empresas grandes pagan más",
    caption = "Fuente: Sistema Araucano.",
    theme = theme(
      plot.background = element_rect(fill = fondo, color = NA),
      plot.title = element_text(color = texto),
      plot.subtitle = element_text(color = texto),
      plot.caption = element_text(color = texto)
    )
  )

print(grafico_2_completo)

ggsave(
  filename = file.path(output_figures, "02_desigualdad_regional_empresarial_editorializado.png"),
  plot = grafico_2_completo,
  width = 14,
  height = 8,
  dpi = 300,
  bg = fondo
)

cat("✓ Gráfico 2 guardado exitosamente\n\n")


# ==============================================================================
# GRÁFICOS COMPLEMENTARIOS
# ==============================================================================

cat("=== GENERANDO GRÁFICOS COMPLEMENTARIOS ===\n")

# ============================
# Gráfico 3 — Evolución temporal
# ============================

cat("Generando gráfico 3...\n")

grafico_3 <- base_graduados %>%
  filter(genero %in% c("Mujeres", "Varones")) %>%
  group_by(anio, genero) %>%
  summarise(mediana = median(salario), n = n(), .groups = "drop") %>%
  ggplot(aes(x = anio, y = mediana, color = genero, group = genero)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = dollar(mediana, prefix = "$")),
            vjust = -1, size = 3.5, fontface = "bold", color = texto) +
  scale_color_manual(values = c("Mujeres" = col_mujer, "Varones" = col_hombre)) +
  labs(
    title = "Evolución Temporal de Salarios",
    subtitle = "Período 2019–2021",
    x = "Año",
    y = "Salario mediano"
  )

print(grafico_3)

ggsave(
  filename = file.path(output_figures, "03_evolucion_temporal_complementario.png"),
  plot = grafico_3,
  width = 10,
  height = 7,
  dpi = 300,
  bg = fondo
)


# ============================
# Gráfico 4 — Gestión universitaria
# ============================

cat("Generando gráfico 4...\n")

grafico_4 <- base_graduados %>%
  filter(genero %in% c("Mujeres", "Varones"),
         gestion %in% c("Estatal", "Privada")) %>%
  group_by(gestion, genero) %>%
  summarise(mediana = median(salario), n = n(), .groups = "drop") %>%
  ggplot(aes(x = gestion, y = mediana, fill = genero)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = dollar(mediana, prefix = "$")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 4, fontface = "bold", color = texto) +
  scale_fill_manual(values = c("Mujeres" = col_mujer, "Varones" = col_hombre)) +
  labs(
    title = "Salarios por Tipo de Gestión Universitaria",
    subtitle = "Comparación entre estatal y privada",
    x = "Gestión",
    y = "Salario mediano"
  )

print(grafico_4)

ggsave(
  filename = file.path(output_figures, "04_gestion_universitaria_complementario.png"),
  plot = grafico_4,
  width = 9,
  height = 7,
  dpi = 300,
  bg = fondo
)

cat("✓ Todos los gráficos complementarios generados\n\n")

# FIN

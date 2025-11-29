# ==============================================================================
# 02_estadisticas_descriptivas.R
# Estadísticas Descriptivas de Datos Crudos (RAW)
# Dataset: Graduados Universitarios Sistema Araucano 2016-2018
# INPUT: data/raw/base_araucano.csv (DATOS CRUDOS)
# OUTPUT: Tablas estadísticas y visualizaciones de datos sin procesar
# ==============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(readr)
library(skimr)        # Resúmenes estadísticos completos
library(moments)      # Para asimetría y curtosis
library(ggplot2)      # Gráficos
library(gridExtra)    # Múltiples gráficos en una página
library(scales)       # Para formatear ejes

# Cargar datos CRUDOS (ruta)
base_araucano <- read_csv("data/raw/base_araucano.csv")

cat("==============================================================================\n")
cat("ESTADÍSTICAS DESCRIPTIVAS - DATOS CRUDOS (RAW)\n")
cat("Análisis ANTES de cualquier limpieza o transformación\n")
cat("==============================================================================\n\n")

# ==============================================================================
# 1. MEDIDAS DE TENDENCIA CENTRAL - SALARIOS (RAW)
# ==============================================================================

cat("=== 1. MEDIDAS DE TENDENCIA CENTRAL - VARIABLE SALARIO ===\n\n")

# Solo trabajar con salarios válidos (sin transformar)
salarios_raw <- base_araucano %>%
  filter(!is.na(salario))

cat("DATOS CRUDOS (RAW) - Sin limpieza:\n")
cat("---------------------------------------\n")
cat("N total:", nrow(base_araucano), "\n")
cat("N con salario:", nrow(salarios_raw), "\n")
cat("N sin salario (NA):", sum(is.na(base_araucano$salario)), "\n")
cat("% con salario:", round(nrow(salarios_raw) / nrow(base_araucano) * 100, 2), "%\n\n")

# Medidas de tendencia central
tendencia_central <- salarios_raw %>%
  summarise(
    # Valores extremos
    minimo = min(salario),
    maximo = max(salario),
    
    # Cuartiles
    q1 = quantile(salario, 0.25),
    mediana = median(salario),
    q3 = quantile(salario, 0.75),
    
    # Promedio
    media = mean(salario),
    
    # Análisis de asimetría
    dif_media_mediana = media - mediana,
    ratio_media_mediana = media / mediana
  )

cat("MEDIDAS DE TENDENCIA CENTRAL:\n")
cat("  Mínimo:", format(tendencia_central$minimo, big.mark = ","), "\n")
cat("  Q1 (25%):", format(round(tendencia_central$q1, 2), big.mark = ","), "\n")
cat("  Mediana (Q2):", format(round(tendencia_central$mediana, 2), big.mark = ","), "\n")
cat("  Media:", format(round(tendencia_central$media, 2), big.mark = ","), "\n")
cat("  Q3 (75%):", format(round(tendencia_central$q3, 2), big.mark = ","), "\n")
cat("  Máximo:", format(tendencia_central$maximo, big.mark = ","), "\n\n")

# Análisis de valores extremos
cat("DETECCIÓN INICIAL DE VALORES EXTREMOS:\n")
cat("  Salarios = 0:", sum(salarios_raw$salario == 0), "\n")
cat("  Salarios < $1,000:", sum(salarios_raw$salario < 1000), "\n")
cat("  Salarios < $10,000:", sum(salarios_raw$salario < 10000), "\n")
cat("  Salarios > $500,000:", sum(salarios_raw$salario > 500000), "\n")
cat("  Salarios > $1,000,000:", sum(salarios_raw$salario > 1000000), "\n\n")

cat("INTERPRETACIÓN:\n")
cat("  Diferencia Media-Mediana: $", format(round(tendencia_central$dif_media_mediana, 2), big.mark = ","), "\n", sep="")
if(tendencia_central$dif_media_mediana > 0) {
  cat("  → Distribución con asimetría positiva (cola derecha)\n")
  cat("  → La media ($", format(round(tendencia_central$media, 0), big.mark = ","), 
      ") es ", round(((tendencia_central$media / tendencia_central$mediana) - 1) * 100, 1), 
      "% mayor que la mediana\n", sep="")
  cat("  → La mediana es más representativa del salario 'típico'\n\n")
}

# Guardar tabla
write_csv(tendencia_central, "output/tables/02_tendencia_central_salarios_raw.csv")

# ==============================================================================
# 2. MEDIDAS DE DISPERSIÓN (RAW)
# ==============================================================================

cat("=== 2. MEDIDAS DE DISPERSIÓN ===\n\n")

dispersion <- salarios_raw %>%
  summarise(
    n = n(),
    media = mean(salario),
    
    # Medidas de dispersión
    rango = max(salario) - min(salario),
    iqr = IQR(salario),
    varianza = var(salario),
    desv_std = sd(salario),
    
    # Coeficiente de variación (dispersión relativa)
    coef_variacion = (sd(salario) / mean(salario)) * 100,
    
    # Error estándar de la media
    error_std = sd(salario) / sqrt(n())
  )

cat("MEDIDAS DE DISPERSIÓN (datos crudos):\n")
cat("  Rango total:", format(round(dispersion$rango, 2), big.mark = ","), "\n")
cat("  Rango intercuartílico (IQR):", format(round(dispersion$iqr, 2), big.mark = ","), "\n")
cat("  Varianza:", format(round(dispersion$varianza, 2), big.mark = ","), "\n")
cat("  Desviación estándar:", format(round(dispersion$desv_std, 2), big.mark = ","), "\n")
cat("  Coeficiente de variación:", round(dispersion$coef_variacion, 2), "%\n")
cat("  Error estándar de la media:", format(round(dispersion$error_std, 2), big.mark = ","), "\n\n")

cat("INTERPRETACIÓN:\n")
cat("  • El IQR ($", format(round(dispersion$iqr, 0), big.mark = ","), 
    ") representa el rango del 50% central de salarios\n", sep="")
cat("  • Coeficiente de variación: ", round(dispersion$coef_variacion, 1), "% → ", sep="")
if(dispersion$coef_variacion > 50) {
  cat("ALTA variabilidad relativa\n")
} else if(dispersion$coef_variacion > 30) {
  cat("variabilidad MODERADA\n")
} else {
  cat("variabilidad BAJA\n")
}
cat("  • Ratio Rango/IQR: ", round(dispersion$rango / dispersion$iqr, 2), 
    " → Indica presencia probable de outliers extremos\n\n", sep="")

# Guardar tabla
write_csv(dispersion, "output/tables/02_dispersion_salarios_raw.csv")

# ==============================================================================
# 3. ASIMETRÍA Y CURTOSIS (FORMA DE LA DISTRIBUCIÓN)
# ==============================================================================

cat("=== 3. FORMA DE LA DISTRIBUCIÓN (ASIMETRÍA Y CURTOSIS) ===\n\n")

forma_distribucion <- salarios_raw %>%
  summarise(
    media = mean(salario),
    mediana = median(salario),
    asimetria = skewness(salario),
    curtosis = kurtosis(salario),
    curtosis_exceso = kurtosis(salario) - 3  # Exceso sobre distribución normal
  )

cat("MEDIDAS DE FORMA:\n")
cat("  Asimetría (skewness):", round(forma_distribucion$asimetria, 3), "\n")
cat("  Curtosis (kurtosis):", round(forma_distribucion$curtosis, 3), "\n")
cat("  Curtosis en exceso:", round(forma_distribucion$curtosis_exceso, 3), "\n\n")

cat("INTERPRETACIÓN:\n")
cat("  Asimetría (", round(forma_distribucion$asimetria, 2), "):\n", sep="")
if(forma_distribucion$asimetria > 2) {
  cat("    → EXTREMADAMENTE asimétrica positiva (cola derecha muy larga)\n")
} else if(forma_distribucion$asimetria > 1) {
  cat("    → FUERTE asimetría positiva (cola derecha larga)\n")
} else if(forma_distribucion$asimetria > 0.5) {
  cat("    → Asimetría positiva moderada\n")
} else if(abs(forma_distribucion$asimetria) <= 0.5) {
  cat("    → Distribución aproximadamente simétrica\n")
}
cat("    → Concentración de salarios bajos con algunos muy altos\n")

cat("\n  Curtosis (", round(forma_distribucion$curtosis, 2), "):\n", sep="")
if(forma_distribucion$curtosis > 3) {
  cat("    → Distribución leptocúrtica (colas MÁS pesadas que la normal)\n")
  cat("    → Muchos más valores extremos de lo esperado\n")
  cat("    → Alto riesgo de outliers\n")
} else if(forma_distribucion$curtosis < 3) {
  cat("    → Distribución platicúrtica (colas más ligeras que la normal)\n")
} else {
  cat("    → Curtosis similar a distribución normal\n")
}

cat("\n  COMPARACIÓN CON DISTRIBUCIÓN NORMAL:\n")
cat("    • Asimetría normal = 0 | Observado =", round(forma_distribucion$asimetria, 2), "\n")
cat("    • Curtosis normal = 3 | Observado =", round(forma_distribucion$curtosis, 2), "\n")
cat("    • La distribución de salarios NO sigue una distribución normal\n\n")

cat("  IMPLICANCIAS PARA EL ANÁLISIS:\n")
cat("    • Usar mediana en lugar de media para reportar salario típico\n")
cat("    • Tests estadísticos que asumen normalidad pueden no ser apropiados\n")
cat("    • Considerar transformación logarítmica para análisis inferencial\n")
cat("    • Análisis de outliers es crítico (Script 03)\n\n")

# Guardar tabla
write_csv(forma_distribucion, "output/tables/02_forma_distribucion_raw.csv")

# ==============================================================================
# 4. PERCENTILES DETALLADOS
# ==============================================================================

cat("=== 4. ANÁLISIS DE PERCENTILES ===\n\n")

percentiles <- salarios_raw %>%
  summarise(
    p1 = quantile(salario, 0.01),
    p5 = quantile(salario, 0.05),
    p10 = quantile(salario, 0.10),
    p25 = quantile(salario, 0.25),
    p50 = quantile(salario, 0.50),
    p75 = quantile(salario, 0.75),
    p90 = quantile(salario, 0.90),
    p95 = quantile(salario, 0.95),
    p99 = quantile(salario, 0.99)
  ) %>%
  pivot_longer(everything(), names_to = "percentil", values_to = "salario") %>%
  mutate(
    percentil_num = as.numeric(str_extract(percentil, "\\d+")),
    salario = round(salario, 2)
  )

cat("DISTRIBUCIÓN DE SALARIOS POR PERCENTILES:\n\n")
print(percentiles, n = 20)

cat("\nINTERPRETACIÓN:\n")
cat("  • El 1% de graduados gana menos de $",
    format(round(percentiles$salario[percentiles$percentil == "p1"], 0), big.mark = ","), "\n", sep="")
cat("  • El 10% gana menos de $",
    format(round(percentiles$salario[percentiles$percentil == "p10"], 0), big.mark = ","), "\n", sep="")
cat("  • El 50% (mediana) gana menos de $",
    format(round(percentiles$salario[percentiles$percentil == "p50"], 0), big.mark = ","), "\n", sep="")
cat("  • El 10% mejor pagado gana más de $",
    format(round(percentiles$salario[percentiles$percentil == "p90"], 0), big.mark = ","), "\n", sep="")
cat("  • El 1% mejor pagado gana más de $",
    format(round(percentiles$salario[percentiles$percentil == "p99"], 0), big.mark = ","), "\n\n", sep="")

# Calcular desigualdad
ratio_p90_p10 <- percentiles$salario[percentiles$percentil == "p90"] / 
  percentiles$salario[percentiles$percentil == "p10"]

cat("  DESIGUALDAD SALARIAL:\n")
cat("    • Ratio P90/P10:", round(ratio_p90_p10, 2), "\n")
cat("    • El 10% mejor pagado gana", round(ratio_p90_p10, 1), 
    "veces más que el 10% peor pagado\n\n")

# Guardar tabla
write_csv(percentiles, "output/tables/02_percentiles_salarios_raw.csv")


# ==============================================================================
# 5. ESTADÍSTICAS POR GRUPOS (VARIABLES CATEGÓRICAS)
# ==============================================================================

cat("=== 5. ESTADÍSTICAS DESCRIPTIVAS POR GRUPOS ===\n\n")

# 5.1 Por Género
cat("--- 5.1 SALARIOS POR GÉNERO ---\n\n")

stats_genero <- salarios_raw %>%
  mutate(genero = case_when(
    genero_id == 1 ~ "Mujer",
    genero_id == 2 ~ "Varón",
    TRUE ~ "Desconocido"
  )) %>%
  group_by(genero) %>%
  summarise(
    n = n(),
    pct_grupo = round(n() / nrow(salarios_raw) * 100, 1),
    minimo = min(salario),
    q1 = quantile(salario, 0.25),
    mediana = median(salario),
    media = mean(salario),
    q3 = quantile(salario, 0.75),
    maximo = max(salario),
    desv_std = sd(salario),
    coef_var = (sd(salario) / mean(salario)) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric) & !c(n, pct_grupo), ~round(., 2)))

print(stats_genero)

# Calcular brecha salarial
brecha_genero <- stats_genero %>%
  filter(genero %in% c("Mujer", "Varón")) %>%
  summarise(
    brecha_media = ((media[genero == "Varón"] - media[genero == "Mujer"]) / media[genero == "Mujer"]) * 100,
    brecha_mediana = ((mediana[genero == "Varón"] - mediana[genero == "Mujer"]) / mediana[genero == "Mujer"]) * 100,
    dif_absoluta_media = media[genero == "Varón"] - media[genero == "Mujer"],
    dif_absoluta_mediana = mediana[genero == "Varón"] - mediana[genero == "Mujer"]
  )

cat("\nBRECHA SALARIAL DE GÉNERO (datos crudos):\n")
cat("  Diferencia en media: $", format(round(brecha_genero$dif_absoluta_media, 0), big.mark = ","), 
    " (", round(brecha_genero$brecha_media, 1), "%)\n", sep="")
cat("  Diferencia en mediana: $", format(round(brecha_genero$dif_absoluta_mediana, 0), big.mark = ","),
    " (", round(brecha_genero$brecha_mediana, 1), "%)\n\n", sep="")

write_csv(stats_genero, "output/tables/02_estadisticas_por_genero_raw.csv")

# 5.2 Por Gestión
cat("--- 8.2 SALARIOS POR TIPO DE GESTIÓN UNIVERSITARIA ---\n\n")

stats_gestion <- salarios_raw %>%
  mutate(gestion = case_when(
    gestion_id == 1 ~ "Estatal",
    gestion_id == 2 ~ "Privada",
    TRUE ~ "Desconocido"
  )) %>%
  group_by(gestion) %>%
  summarise(
    n = n(),
    pct_grupo = round(n() / nrow(salarios_raw) * 100, 1),
    mediana = median(salario),
    media = mean(salario),
    desv_std = sd(salario),
    coef_var = (sd(salario) / mean(salario)) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric) & !c(n, pct_grupo), ~round(., 2)))

print(stats_gestion)
cat("\n")

write_csv(stats_gestion, "output/tables/02_estadisticas_por_gestion_raw.csv")

# 5.3 Por Región
cat("--- 8.3 SALARIOS POR REGIÓN ---\n\n")

stats_region <- salarios_raw %>%
  mutate(region = case_when(
    region_id == 1 ~ "CABA",
    region_id == 2 ~ "Buenos Aires",
    region_id == 3 ~ "Resto Pampeana",
    region_id == 4 ~ "NOA",
    region_id == 5 ~ "NEA",
    region_id == 6 ~ "Cuyo",
    region_id == 7 ~ "Patagonia",
    TRUE ~ "Desconocido"
  )) %>%
  group_by(region) %>%
  summarise(
    n = n(),
    pct_grupo = round(n() / nrow(salarios_raw) * 100, 1),
    mediana = median(salario),
    media = mean(salario),
    desv_std = sd(salario),
    .groups = "drop"
  ) %>%
  arrange(desc(mediana)) %>%
  mutate(across(where(is.numeric) & !c(n, pct_grupo), ~round(., 2)))

print(stats_region, n = 20)
cat("\n")

write_csv(stats_region, "output/tables/02_estadisticas_por_region_raw.csv")

# ==============================================================================
# 6. DISTRIBUCIÓN DE FRECUENCIAS - VARIABLES CATEGÓRICAS
# ==============================================================================

cat("=== 6. DISTRIBUCIÓN DE FRECUENCIAS (TODAS LAS OBSERVACIONES) ===\n\n")

# 6.1 Género
cat("--- 6.1 DISTRIBUCIÓN POR GÉNERO ---\n\n")

freq_genero <- base_araucano %>%
  count(genero_id) %>%
  mutate(
    genero = case_when(
      genero_id == 1 ~ "Mujer",
      genero_id == 2 ~ "Varón",
      TRUE ~ "Desconocido"
    ),
    porcentaje = round(n / sum(n) * 100, 2),
    porcentaje_acum = cumsum(porcentaje)
  ) %>%
  select(genero, n, porcentaje, porcentaje_acum)

print(freq_genero)
cat("\n")

write_csv(freq_genero, "output/tables/02_frecuencia_genero_raw.csv")

# 6.2 Gestión
cat("--- 6.2 DISTRIBUCIÓN POR GESTIÓN ---\n\n")

freq_gestion <- base_araucano %>%
  count(gestion_id) %>%
  mutate(
    gestion = case_when(
      gestion_id == 1 ~ "Estatal",
      gestion_id == 2 ~ "Privada",
      TRUE ~ "Desconocido"
    ),
    porcentaje = round(n / sum(n) * 100, 2)
  ) %>%
  select(gestion, n, porcentaje)

print(freq_gestion)
cat("\n")

write_csv(freq_gestion, "output/tables/02_frecuencia_gestion_raw.csv")

# ==============================================================================
# 7. VISUALIZACIONES COMPLEMENTARIAS
# ==============================================================================

cat("=== 7. GENERANDO VISUALIZACIONES ===\n\n")

# 7.1 Histograma de salarios
cat("Generando histograma de salarios...\n")

histograma_salarios <- ggplot(salarios_raw, aes(x = salario)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(salario)), 
             color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = median(salario)), 
             color = "darkgreen", linetype = "dashed", size = 1.2) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Distribución de Salarios - Datos Crudos (RAW)",
    subtitle = paste0("Media: $", format(round(mean(salarios_raw$salario), 0), big.mark = ","), 
                      " | Mediana: $", format(round(median(salarios_raw$salario), 0), big.mark = ",")),
    x = "Salario mensual ($)",
    y = "Frecuencia",
    caption = "Línea roja: Media | Línea verde: Mediana\nFuente: Sistema Araucano 2016-2018"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11)
  )

ggsave("output/figures/02_histograma_salarios_raw.png", 
       histograma_salarios, width = 10, height = 6, dpi = 300)

# 7.2 Boxplot general
cat("Generando boxplot de salarios...\n")

boxplot_salarios <- ggplot(salarios_raw, aes(y = salario)) +
  geom_boxplot(fill = "lightblue", color = "black", 
               outlier.color = "red", outlier.size = 1.5, outlier.alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Boxplot de Salarios - Detección de Valores Atípicos",
    subtitle = paste0("IQR: $", format(round(IQR(salarios_raw$salario), 0), big.mark = ","), 
                      " | Datos crudos sin procesar"),
    y = "Salario mensual ($)",
    x = "",
    caption = "Puntos rojos = Outliers detectados por regla IQR\nFuente: Sistema Araucano 2016-2018"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_blank()
  ) +
  coord_flip()

ggsave("output/figures/02_boxplot_salarios_raw.png", 
       boxplot_salarios, width = 10, height = 6, dpi = 300)

# 7.3 Boxplot por género
cat("Generando boxplot de salarios por género...\n")

boxplot_genero <- salarios_raw %>%
  mutate(genero = case_when(
    genero_id == 1 ~ "Mujer",
    genero_id == 2 ~ "Varón"
  )) %>%
  filter(!is.na(genero)) %>%
  ggplot(aes(x = genero, y = salario, fill = genero)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1, outlier.alpha = 0.5) +
  scale_fill_manual(values = c("Mujer" = "#E69F00", "Varón" = "#56B4E9")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribución de Salarios por Género - Datos Crudos",
    subtitle = "Graduados universitarios con empleo formal registrado",
    x = "Género",
    y = "Salario mensual ($)",
    caption = "Fuente: Sistema Araucano 2016-2018"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

ggsave("output/figures/02_boxplot_salarios_genero_raw.png", 
       boxplot_genero, width = 10, height = 6, dpi = 300)

# 7.4 Boxplot por gestión
cat("Generando boxplot de salarios por gestión...\n")

boxplot_gestion <- salarios_raw %>%
  mutate(gestion = case_when(
    gestion_id == 1 ~ "Estatal",
    gestion_id == 2 ~ "Privada"
  )) %>%
  filter(!is.na(gestion)) %>%
  ggplot(aes(x = gestion, y = salario, fill = gestion)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1, outlier.alpha = 0.5) +
  scale_fill_manual(values = c("Estatal" = "#00BA38", "Privada" = "#F8766D")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribución de Salarios por Tipo de Gestión - Datos Crudos",
    subtitle = "Universidad Estatal vs Privada",
    x = "Tipo de Gestión
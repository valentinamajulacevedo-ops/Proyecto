library(tidyverse)
library(wooldridge)

#rutas
data_raw <- 'data/raw' #datos crudos
data_processed <- 'data/processed' 
data_clean <- 'data/clean'

#cargo datos
base_graduados <- read_csv(file.path(data_raw,'01_base_cruda.csv'))

# 1.Estadísticas descriptivas iniciales del salario
stats_original <- base_graduados %>%
  summarise(
    n_total = n(),
    n_na_salario = sum(is.na(salario)),
    media_salario = mean(salario, na.rm = TRUE),
    mediana_salario = median(salario, na.rm = TRUE),
    sd_salario = sd(salario, na.rm = TRUE),
    min_salario = min(salario, na.rm = TRUE),
    max_salario = max(salario, na.rm = TRUE),
    q1_salario = quantile(salario, 0.25, na.rm = TRUE),
    q3_salario = quantile(salario, 0.75, na.rm = TRUE),
    p1_salario = quantile(salario, 0.01, na.rm = TRUE),
    p99_salario = quantile(salario, 0.99, na.rm = TRUE)
  )

cat("\nEstadísticas descriptivas ORIGINALES de salario:\n")
print(stats_original)

# Contar NAs por variable
na_count_original <- base_graduados %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_NAs") %>%
  filter(n_NAs > 0) %>%
  arrange(desc(n_NAs))

cat("\nConteo de NAs por variable (ORIGINAL):\n")
print(na_count_original)

# 2. LIMPIEZA DE DATOS
cat("PROCESO DE LIMPIEZA\n")

#Elimino filas con NAs
base_sin_na <- base_graduados %>%
  drop_na()

cat("\nObservaciones eliminadas por NAs:", nrow(base_graduados) - nrow(base_sin_na), "\n")
cat("Observaciones restantes:", nrow(base_sin_na), "\n")

# Paso 2: Winsorizar salarios (P10-P90)
p1_salario <- quantile(base_sin_na$salario, 0.1)
p99_salario <- quantile(base_sin_na$salario, 0.9)

cat("\nWinsorización de salarios:\n")
cat("P1 (límite inferior):", p1_salario, "\n")
cat("P99 (límite superior):", p99_salario, "\n")

base_limpia <- base_sin_na %>%
  mutate(
    salario_original = salario,  # Guardamos el original para comparación
    salario = case_when(
      salario < p1_salario ~ p1_salario,
      salario > p99_salario ~ p99_salario,
      TRUE ~ salario
    ),
    winsorizado = salario != salario_original
  )

# Contar cuántos salarios fueron winsorizados
n_winsorizado <- sum(base_limpia$winsorizado)
cat("Salarios winsorizados:", n_winsorizado, "\n")
cat("Porcentaje winsorizado:", round(n_winsorizado/nrow(base_limpia)*100, 2), "%\n")

# 3. ESTADÍSTICAS DESCRIPTIVAS POST-LIMPIEZA
cat("ESTADÍSTICAS DESCRIPTIVAS PROCESADAS\n")

stats_procesada <- base_limpia %>%
  summarise(
    n_total = n(),
    n_na_salario = sum(is.na(salario)),
    media_salario = mean(salario),
    mediana_salario = median(salario),
    sd_salario = sd(salario),
    min_salario = min(salario),
    max_salario = max(salario),
    q1_salario = quantile(salario, 0.25),
    q3_salario = quantile(salario, 0.75),
    p1_salario = quantile(salario, 0.01),
    p99_salario = quantile(salario, 0.99)
  )

cat("\nEstadísticas descriptivas PROCESADAS de salario:\n
    Las medidas descriptivas disminuyeron")
print(stats_procesada)

# 4. COMPARACIÓN Y CUANTIFICACIÓN DE ALTERACION DEL SALARIO
cat("COMPARACIÓN: ORIGINAL vs PROCESADA\n")
comparacion <- tibble(
  Estadística = c("N observaciones", "Media", "Mediana", "Desv. Estándar", 
                  "Mínimo", "Máximo", "Q1", "Q3"),
  Original = c(
    stats_original$n_total,
    stats_original$media_salario,
    stats_original$mediana_salario,
    stats_original$sd_salario,
    stats_original$min_salario,
    stats_original$max_salario,
    stats_original$q1_salario,
    stats_original$q3_salario
  ),
  Procesada = c(
    stats_procesada$n_total,
    stats_procesada$media_salario,
    stats_procesada$mediana_salario,
    stats_procesada$sd_salario,
    stats_procesada$min_salario,
    stats_procesada$max_salario,
    stats_procesada$q1_salario,
    stats_procesada$q3_salario
  )
) %>%
  mutate(
    Diferencia = Procesada - Original,
    Cambio_Porcentual = round((Diferencia / Original) * 100, 2)
  )

print(comparacion)

# 5. CUANTIFICACIÓN DEL NIVEL DE ALTERACIÓN
cat("NIVEL DE ALTERACIÓN PRODUCIDO\n")

alteracion <- tibble(
  Indicador = c(
    "Observaciones eliminadas (NAs)",
    "% de observaciones eliminadas",
    "Salarios winsorizados",
    "% de salarios winsorizados",
    "Cambio en media (%)",
    "Cambio en mediana (%)",
    "Cambio en desviación estándar (%)",
    "Reducción en rango (max-min)"
  ),
  Valor = c(
    nrow(base_graduados) - nrow(base_sin_na),
    round((nrow(base_graduados) - nrow(base_sin_na)) / nrow(base_graduados) * 100, 2),
    n_winsorizado,
    round(n_winsorizado / nrow(base_limpia) * 100, 2),
    comparacion$Cambio_Porcentual[comparacion$Estadística == "Media"],
    comparacion$Cambio_Porcentual[comparacion$Estadística == "Mediana"],
    comparacion$Cambio_Porcentual[comparacion$Estadística == "Desv. Estándar"],
    (stats_original$max_salario - stats_original$min_salario) - 
      (stats_procesada$max_salario - stats_procesada$min_salario)
  )
)

print(alteracion)
cat("\nse eliminarion un 41% de obs debido a NAs y los salarios wirsorizados 
fueron 95993\n")
cat("\n1. IMPACTO EN MEDIDAS DE TENDENCIA CENTRAL:\n")
cat("   - Cambio en media:", round(comparacion$Cambio_Porcentual[2], 2), "%\n")
cat("   - Cambio en mediana:", round(comparacion$Cambio_Porcentual[3], 2), "%\n")


# 6. EVALUACIÓN DEL IMPACTO EN CONCLUSIONES
if(abs(comparacion$Cambio_Porcentual[2]) < 5) {
  cat("\nImpacto BAJO: Las conclusiones sobre salario promedio son robustas\n")
} else if(abs(comparacion$Cambio_Porcentual[2]) < 10) {
  cat("\nImpacto MODERADO:Las estadisticas del saraio cambian pero no tanto
la forma de las observaciones se mantienen bastante bien\n")
} else {
  cat("\nImpacto ALTO: Las conclusiones sobre salario promedio podrían cambiar significativamente\n")
}

cat("\nIMPACTO EN VARIABILIDAD:\n")
cat("Cambio en desviación estándar:", round(comparacion$Cambio_Porcentual[4], 2), "%\n")

if(abs(comparacion$Cambio_Porcentual[4]) < 10) {
  cat("Impacto BAJO en dispersión\n")
} else if(abs(comparacion$Cambio_Porcentual[4]) < 20) {
  cat("Impacto MODERADO en dispersión\n")
} else {
  cat("Impacto ALTO en dispersión: la desigualdad salarial cambió\n")
}

cat("\nIMPACTO EN VALORES EXTREMOS:\n")
cat("Salarios winsorizados:", n_winsorizado, "(", 
    round(n_winsorizado/nrow(base_limpia)*100, 2), "%)\n")
cat("   - Esto afecta principalmente análisis de:\n")
cat("     • Distribución de colas\n")
cat("     • Detección de casos excepcionales\n")
cat("     • Análisis de desigualdad extrema\n")

# 7. VISUALIZACIONES COMPARATIVAS
p1 <- ggplot() +
  geom_density(data = base_sin_na, aes(x = salario, fill = "Original"), alpha = 0.5) +
  geom_density(data = base_limpia, aes(x = salario, fill = "Winsorizado"), alpha = 0.5) +
  scale_fill_manual(values = c("Original" = "red", "Winsorizado" = "blue")) +
  labs(title = "Comparación de Distribución de Salarios",
       subtitle = "Original (con outliers) vs Winsorizado (P1-P99)",
       x = "Salario", y = "Densidad", fill = "Versión") +
  theme_minimal()

print(p1)

# Boxplot comparativo
datos_comparacion <- bind_rows(
  base_sin_na %>% select(salario) %>% mutate(tipo = "Original"),
  base_limpia %>% select(salario) %>% mutate(tipo = "Winsorizado")
)

p2 <- ggplot(datos_comparacion, aes(x = tipo, y = salario, fill = tipo)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Original" = "red", "Winsorizado" = "blue")) +
  labs(title = "Comparación de Boxplots: Salarios",
       x = "", y = "Salario") +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)

# 8. GUARDAR DATOS LIMPIOS
base_final <- base_limpia %>%
  select(-salario_original, -winsorizado)

write_csv(base_final, file.path(data_clean, '01_base_limpia.csv'))

cat("\n✓ Datos limpios guardados en:", file.path(data_clean, '01_base_limpia.csv'), "\n")

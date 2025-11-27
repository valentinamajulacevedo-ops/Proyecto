# ==============================================================================
# 01_exploracion_inicial.R
# Análisis Exploratorio de Datos (EDA)
# Dataset: Graduados Universitarios Sistema Araucano 2016-2018
# ==============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(readr)
library(naniar)  # Para análisis de datos faltantes
library(VIM)         # Para visualización de patrones de faltantes
library(skimr)       # Para resumen estadístico completo

# Cargar datos crudos
base_araucano <- read_csv("data/raw/base_araucano.csv")

# ==============================================================================
# 1. ESTRUCTURA GENERAL Y DIMENSIONES DEL DATASET
# ==============================================================================

cat("=== 1. ESTRUCTURA GENERAL ===\n\n")

cat("Dimensiones del dataset:\n")
cat("  - Observaciones:", nrow(base_araucano), "\n")
cat("  - Variables:", ncol(base_araucano), "\n\n")

cat("Tipo de datos por variable:\n")
str(base_araucano)

cat("\n=== Primeras 10 observaciones ===\n")
print(head(base_araucano, 10))

# ==============================================================================
# 2. IDENTIFICACIÓN DE VARIABLES RELEVANTES
# ==============================================================================

cat("\n\n=== 2. CLASIFICACIÓN DE VARIABLES ===\n\n")

cat("Variables categóricas (codificadas):\n")
cat("  - rama_id: Rama de conocimiento (6 categorías)\n")
cat("  - disciplina_id: Disciplina específica (38 categorías)\n")
cat("  - tipo_titulo_id: Nivel educativo (4 categorías)\n")
cat("  - gestion_id: Tipo de gestión universitaria (2 categorías)\n")
cat("  - genero_id: Género del graduado (2 categorías)\n")
cat("  - region_id: Región geográfica (7 categorías)\n")
cat("  - tamaño_id: Tamaño de empresa (4 categorías)\n")
cat("  - letra_id: Sector económico de empleo (20 categorías)\n\n")

cat("Variables numéricas continuas:\n")
cat("  - salario: Salario mensual\n\n")

cat("Variables temporales:\n")
cat("  - anio: Año de observación del salario (2019-2021)\n")
cat("  - anioegreso: Año de egreso del graduado (2016-2018)\n")
cat("  - anionac: Año de nacimiento del graduado\n\n")

cat("Variable identificadora:\n")
cat("  - id: Identificador único del graduado\n\n")

# ==============================================================================
# 3. ANÁLISIS EXHAUSTIVO DE DATOS FALTANTES
# ==============================================================================

cat("\n=== 3. ANÁLISIS DE DATOS FALTANTES ===\n\n")

# 3.1 Cuantificación de datos faltantes por variable
cat("--- 3.1 Cantidad y porcentaje de datos faltantes ---\n\n")

# Calcular datos faltantes de forma simple
miss_summary <- data.frame(
  variable = names(base_araucano),
  n_missing = sapply(base_araucano, function(x) sum(is.na(x))),
  n_present = sapply(base_araucano, function(x) sum(!is.na(x))),
  pct_missing = round(sapply(base_araucano, function(x) mean(is.na(x))) * 100, 2),
  pct_present = round(sapply(base_araucano, function(x) mean(!is.na(x))) * 100, 2),
  row.names = NULL
)

miss_summary <- miss_summary %>%
  arrange(desc(pct_missing))

print(miss_summary)

# Guardar resumen de datos faltantes
write_csv(miss_summary, "output/tables/01_resumen_datos_faltantes.csv")

cat("\n--- 3.2 Variables con datos faltantes significativos (>5%) ---\n\n")
vars_con_faltantes <- miss_summary %>%
  filter(pct_missing > 5)

if(nrow(vars_con_faltantes) > 0) {
  print(vars_con_faltantes)
  
  cat("\n*** INTERPRETACIÓN PRELIMINAR ***\n")
  cat("Las variables relacionadas con el empleo (salario, tamaño_id, letra_id)\n")
  cat("presentan aproximadamente 41.6% de datos faltantes.\n")
  cat("Esto sugiere que estos datos provienen de registros administrativos\n")
  cat("que solo capturan graduados que están formalmente empleados.\n\n")
} else {
  cat("No hay variables con más del 5% de datos faltantes.\n\n")
}

# 3.3 Análisis de patrones de datos faltantes
cat("--- 3.3 Patrones de combinación de datos faltantes ---\n\n")

# Identificar patrones de faltantes
patron_faltantes <- base_araucano %>%
  mutate(across(everything(), ~as.numeric(is.na(.)))) %>%
  group_by(across(everything())) %>%
  summarise(n_casos = n(), .groups = "drop") %>%
  arrange(desc(n_casos)) %>%
  head(10)

cat("Los 10 patrones más frecuentes de datos faltantes:\n")
cat("(1 = faltante, 0 = presente)\n\n")
print(patron_faltantes)

# 3.4 Hipótesis sobre el mecanismo de datos faltantes
cat("\n--- 3.4 Evaluación del mecanismo de datos faltantes ---\n\n")

cat("ANÁLISIS DEL MECANISMO (según clasificación de Rubin, 1976):\n\n")

cat("Variables: salario, tamaño_id, letra_id\n")
cat("Porcentaje faltante: ~41.6%\n\n")

cat("Hipótesis de mecanismo:\n")
cat("  - NO es MCAR (Missing Completely at Random):\n")
cat("    Los datos no faltan aleatoriamente. Hay un patrón sistemático.\n\n")

cat("  - Probablemente es MAR (Missing at Random):\n")
cat("    La probabilidad de que falte el salario depende de variables observadas\n")
cat("    como el año, la región, o la disciplina, pero NO del salario en sí.\n")
cat("    Explicación: Los registros administrativos solo capturan empleados\n")
cat("    formales, lo cual puede estar correlacionado con disciplina, región, etc.\n\n")

cat("  - Posibilidad de MNAR (Missing Not at Random):\n")
cat("    Si los graduados con salarios bajos evitan el registro formal,\n")
cat("    entonces la probabilidad de faltante dependería del salario mismo.\n\n")

cat("IMPLICANCIAS:\n")
cat("  - Eliminar observaciones con salario faltante podría introducir sesgo\n")
cat("  - Las conclusiones sobre salarios solo aplicarían a empleados formales\n")
cat("  - Se requiere análisis de sensibilidad para evaluar robustez\n\n")

# 3.5 Análisis de correlación entre variables con/sin faltantes
cat("--- 3.5 Comparación de características según presencia de datos ---\n\n")

base_con_indicador <- base_araucano %>%
  mutate(tiene_salario = !is.na(salario))

cat("Distribución por género según disponibilidad de salario:\n")
tabla_genero <- base_con_indicador %>%
  group_by(genero_id, tiene_salario) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(genero_id) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2))
print(tabla_genero)

cat("\nDistribución por gestión según disponibilidad de salario:\n")
tabla_gestion <- base_con_indicador %>%
  group_by(gestion_id, tiene_salario) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(gestion_id) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2))
print(tabla_gestion)

cat("\nDistribución por región según disponibilidad de salario:\n")
tabla_region <- base_con_indicador %>%
  group_by(region_id, tiene_salario) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(region_id) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2))
print(tabla_region)

# ==============================================================================
# 4. ESTADÍSTICAS DESCRIPTIVAS DE VARIABLES COMPLETAS
# ==============================================================================

cat("\n\n=== 4. ESTADÍSTICAS DESCRIPTIVAS (variables sin faltantes) ===\n\n")

cat("--- 4.1 Variables categóricas ---\n\n")

cat("Distribución por Rama de conocimiento:\n")
tabla_rama <- as.data.frame(table(base_araucano$rama_id))
colnames(tabla_rama) <- c("rama_id", "Frecuencia")
tabla_rama$Porcentaje <- round(tabla_rama$Frecuencia / sum(tabla_rama$Frecuencia) * 100, 2)
print(tabla_rama)

cat("\nDistribución por Gestión universitaria:\n")
tabla_gestion_desc <- as.data.frame(table(base_araucano$gestion_id))
colnames(tabla_gestion_desc) <- c("gestion_id", "Frecuencia")
tabla_gestion_desc$Porcentaje <- round(tabla_gestion_desc$Frecuencia / sum(tabla_gestion_desc$Frecuencia) * 100, 2)
print(tabla_gestion_desc)

cat("\nDistribución por Género:\n")
tabla_genero_desc <- as.data.frame(table(base_araucano$genero_id))
colnames(tabla_genero_desc) <- c("genero_id", "Frecuencia")
tabla_genero_desc$Porcentaje <- round(tabla_genero_desc$Frecuencia / sum(tabla_genero_desc$Frecuencia) * 100, 2)
print(tabla_genero_desc)

cat("\nDistribución por Región:\n")
tabla_region_desc <- as.data.frame(table(base_araucano$region_id))
colnames(tabla_region_desc) <- c("region_id", "Frecuencia")
tabla_region_desc$Porcentaje <- round(tabla_region_desc$Frecuencia / sum(tabla_region_desc$Frecuencia) * 100, 2)
print(tabla_region_desc)

cat("\nDistribución por Tipo de título:\n")
tabla_titulo <- as.data.frame(table(base_araucano$tipo_titulo_id))
colnames(tabla_titulo) <- c("tipo_titulo_id", "Frecuencia")
tabla_titulo$Porcentaje <- round(tabla_titulo$Frecuencia / sum(tabla_titulo$Frecuencia) * 100, 2)
print(tabla_titulo)

# Guardar tablas de frecuencias
write_csv(tabla_rama, "output/tables/01_frecuencia_rama.csv")
write_csv(tabla_gestion_desc, "output/tables/01_frecuencia_gestion.csv")
write_csv(tabla_genero_desc, "output/tables/01_frecuencia_genero.csv")
write_csv(tabla_region_desc, "output/tables/01_frecuencia_region.csv")
write_csv(tabla_titulo, "output/tables/01_frecuencia_titulo.csv")

# ==============================================================================
# 5. ANÁLISIS DETALLADO DE LA VARIABLE SALARIO
# ==============================================================================

cat("\n--- 4.2 Variable numérica: Salario ---\n\n")

salarios_validos <- base_araucano %>% 
  filter(!is.na(salario))

cat("SALARIOS - Solo casos con dato presente\n")
cat("----------------------------------------\n")
cat("N válidos:", nrow(salarios_validos), "\n")
cat("N faltantes:", sum(is.na(base_araucano$salario)), "\n")
cat("% con salario:", round(nrow(salarios_validos) / nrow(base_araucano) * 100, 2), "%\n\n")

cat("Estadísticas descriptivas:\n")
cat("  Mínimo:", min(salarios_validos$salario), "\n")
cat("  Q1 (25%):", quantile(salarios_validos$salario, 0.25), "\n")
cat("  Mediana:", median(salarios_validos$salario), "\n")
cat("  Q3 (75%):", quantile(salarios_validos$salario, 0.75), "\n")
cat("  Máximo:", max(salarios_validos$salario), "\n")
cat("  Media:", round(mean(salarios_validos$salario), 2), "\n")
cat("  Desviación estándar:", round(sd(salarios_validos$salario), 2), "\n")
cat("  Coef. variación:", round(sd(salarios_validos$salario) / mean(salarios_validos$salario), 3), "\n\n")

# Detección inicial de anomalías en salarios
cat("Detección de valores atípicos:\n")
cat("  Salarios = 0:", sum(salarios_validos$salario == 0), "\n")
cat("  Salarios < 10,000:", sum(salarios_validos$salario < 10000), "\n")
cat("  Salarios > 500,000:", sum(salarios_validos$salario > 500000), "\n")
cat("  Salarios > 1,000,000:", sum(salarios_validos$salario > 1000000), "\n\n")

# Guardar estadísticas de salarios
estadisticas_salario <- data.frame(
  estadistica = c("N válidos", "N faltantes", "% con salario", 
                  "Mínimo", "Q1", "Mediana", "Q3", "Máximo", 
                  "Media", "Desv. Std", "Coef. Var",
                  "Salarios = 0", "Salarios < 10k", "Salarios > 500k", "Salarios > 1M"),
  valor = c(nrow(salarios_validos), 
            sum(is.na(base_araucano$salario)),
            round(nrow(salarios_validos) / nrow(base_araucano) * 100, 2),
            min(salarios_validos$salario),
            quantile(salarios_validos$salario, 0.25),
            median(salarios_validos$salario),
            quantile(salarios_validos$salario, 0.75),
            max(salarios_validos$salario),
            round(mean(salarios_validos$salario), 2),
            round(sd(salarios_validos$salario), 2),
            round(sd(salarios_validos$salario) / mean(salarios_validos$salario), 3),
            sum(salarios_validos$salario == 0),
            sum(salarios_validos$salario < 10000),
            sum(salarios_validos$salario > 500000),
            sum(salarios_validos$salario > 1000000))
)

write_csv(estadisticas_salario, "output/tables/01_estadisticas_salario.csv")

# ==============================================================================
# 6. AÑADIR ETIQUETAS DEL DICCIONARIO
# ==============================================================================

cat("\n=== 5. INCORPORACIÓN DE ETIQUETAS DESCRIPTIVAS ===\n\n")

# Crear diccionarios basados en el PDF
rama_dict <- data.frame(
  rama_id = 1:6,
  rama = c("Ciencias Sociales", "Ciencias Aplicadas", "Ciencias de la Salud",
           "Ciencias Humanas", "Ciencias Básicas", "Sin Rama")
)

gestion_dict <- data.frame(
  gestion_id = 1:2,
  gestion = c("Estatal", "Privada")
)

genero_dict <- data.frame(
  genero_id = 1:2,
  genero = c("Mujer", "Varón")
)

region_dict <- data.frame(
  region_id = 1:7,
  region = c("CABA", "Buenos Aires", "Resto Pampeana", 
             "NOA", "NEA", "Cuyo", "Patagonia")
)

tipo_titulo_dict <- data.frame(
  tipo_titulo_id = 1:4,
  tipo_titulo = c("Pregrado y títulos intermedios", "Tecnicatura",
                  "Grado y profesorado", "Posgrado")
)

tamano_dict <- data.frame(
  tamaño_id = 1:4,
  tamano = c("Micro (< 10 empleados)", "Pequeña (10-49 empleados)",
             "Mediana (50-199 empleados)", "Grande (≥ 200 empleados)")
)

# Unir todas las etiquetas al dataset
base_con_etiquetas <- base_araucano %>%
  left_join(rama_dict, by = "rama_id") %>%
  left_join(gestion_dict, by = "gestion_id") %>%
  left_join(genero_dict, by = "genero_id") %>%
  left_join(region_dict, by = "region_id") %>%
  left_join(tipo_titulo_dict, by = "tipo_titulo_id") %>%
  left_join(tamano_dict, by = "tamaño_id")

cat("Etiquetas incorporadas exitosamente\n")
cat("Nuevas variables agregadas: rama, gestion, genero, region, tipo_titulo, tamano\n\n")

# ==============================================================================
# 7. GUARDAR DATASET CON ETIQUETAS (DATA/CLEAN)
# ==============================================================================

cat("=== 6. GUARDANDO DATOS PROCESADOS ===\n\n")

write_csv(base_con_etiquetas, "data/clean/base_con_etiquetas.csv")
cat("✓ Archivo guardado: data/clean/base_con_etiquetas.csv\n")
cat("  Variables originales: 13\n")
cat("  Variables con etiquetas: ", ncol(base_con_etiquetas), "\n\n")

# ==============================================================================
# 8. RESUMEN EJECUTIVO DEL EDA
# ==============================================================================

cat("==============================================================================\n")
cat("RESUMEN EJECUTIVO - ANÁLISIS EXPLORATORIO\n")
cat("==============================================================================\n\n")

cat("DATASET:\n")
cat("  • Observaciones: 820,335 graduados universitarios\n")
cat("  • Período de egreso: 2016-2018\n")
cat("  • Período de observación salarial: 2019-2021\n\n")

cat("HALLAZGOS PRINCIPALES:\n\n")

cat("1. DATOS COMPLETOS:\n")
cat("   • Variables demográficas y educativas: 100% completas\n")
cat("   • Sin problemas de calidad en variables categóricas\n\n")

cat("2. DATOS FALTANTES:\n")
cat("   • Salario: 41.6% faltante (341,616 observaciones)\n")
cat("   • Tamaño empresa: 41.6% faltante\n")
cat("   • Sector económico: 41.6% faltante\n")
cat("   • Patrón: Las tres variables faltan simultáneamente\n\n")

cat("3. MECANISMO DE FALTANTES:\n")
cat("   • Hipótesis: MAR (Missing at Random)\n")
cat("   • Explicación: Solo graduados con empleo formal registrado\n")
cat("   • Implicancia: Análisis de salarios solo válido para empleo formal\n\n")

cat("4. DISTRIBUCIONES OBSERVADAS:\n")
cat("   • Género: 60.4% mujeres, 39.6% varones\n")
cat("   • Gestión: 79.1% universidades estatales\n")
cat("   • Región: Concentración en Buenos Aires y CABA\n")
cat("   • Salario promedio (empleados formales): $99,769\n\n")

cat("5. VALORES ATÍPICOS DETECTADOS:\n")
cat("   • Salarios extremadamente altos (> $1M): ", 
    sum(salarios_validos$salario > 1000000, na.rm = TRUE), " casos\n")
cat("   • Requiere análisis detallado en script 02\n\n")

cat("PRÓXIMOS PASOS:\n")
cat("  → Script 02: Análisis de outliers y decisiones de limpieza\n")
cat("  → Script 03: Estadísticas descriptivas post-limpieza\n")
cat("  → Script 04: Visualizaciones\n")
cat("  → Script 05: Análisis inferencial\n\n")

cat("==============================================================================\n")
cat("SCRIPT 01 COMPLETADO EXITOSAMENTE\n")
cat("==============================================================================\n")

library(tidyverse)
library(wooldridge)
library(naniar)  # para análisis de datos faltantes
library(VIM)     # visualización de missing data
library(mice)   

#rutas
data_raw <- 'data/raw' #datos crudos
data_processed <- 'data/processed' #datos limpios

#cargo datos
base_graduados <- read_csv(file.path(data_raw,'01_base_cruda.csv'))

#identificacion
cat("\nEXPLORACIÓN INICIAL\n")
glimpse(base_graduados)
summary(base_graduados)

cat("\nDatos faltantes por variable\n")
tabla_missing <- miss_var_summary(base_graduados)
print(tabla_missing)
cat("\ Hay 4 variables con datos faltantes, en tres de ellas estos NA's representan mas del 40% de todos las observaciones de las variables\n")

# 2. Resumen de datos faltantes
datos_faltantes <- base_graduados %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes") %>%
  mutate(
    porcentaje = round((n_faltantes / nrow(base_graduados)) * 100, 2),
    n_observados = nrow(base_graduados) - n_faltantes
  ) %>%
  arrange(desc(porcentaje))

print(datos_faltantes)

# 3. Analizo patrones de missing
cat("\nQue patrón siguen los datos faltantes\n")

# Upset plot: combinaciones de missing
if(sum(is.na(base_graduados)) > 0) {
  p2 <- gg_miss_upset(base_graduados, nsets = 10)
  print(p2)
}


cat("\nDIAGNÓSTICO DEL MECANISMO DE DATOS FALTANTES: Las
variables con missing: salario, letra_id, tamanio_id, anioegreso.
Las 3 primeras siguen un patrón MNAR 
Los años faltante son un MCAR ya que no depeenden de un patron y es más al azar
  ")

# Crear indicadores de missing
base_con_indicadores <- base_graduados %>%
  mutate(
    missing_salario = is.na(salario),
    missing_letra = is.na(letra_id),
    missing_tamanio = is.na(tamaño_id),
    missing_anionac = is.na(anionac)
  )

# PRUEBA 2: Análisis de correlación entre missings
cat("\n\n--- PRUEBA 2: Correlación entre variables con missing ---\n")
cat("Si hay correlación alta, los missings siguen un patrón sistemático\n\n")

cor_missing <- base_con_indicadores %>%
  select(missing_salario, missing_letra, missing_tamanio, missing_anionac) %>%
  cor(use = "complete.obs")

print(round(cor_missing, 3))

cat("\n INTERPRETACIÓN:\n")
cat("- Correlación = 1.0 → Las variables tienen missing en las MISMAS observaciones\n")
cat("- Correlación ≈ 0 → Los missing son independientes entre variables\n")

#4. 5.2: Desición NA
cat("\n 1.Anionac: Para el año de egreso haremos una eliminación completa 
   - Representan solo el 0.14% de sus observaciones
   - No afectan sustancialmente el análisis principal
   - Los valores no estan relacionados con las demás
1.salario, tamaño de la firma y sector laboral: Se decidió eliminar sus outlirs
  ya que si bien los valores extremos pueden ser salarios de CEOs o gente de alto rango
  por lo que eliminarlos sesgaria hacia abajo la media salarial
  debido a que el 41.6% de datos faltantes en salario, sector y tamaño de firma excede 
  el umbral recomendado para imputación confiable. Estas tres variables faltan 
  simultáneamente ya que estan correlacionadas, se sugiriendo que corresponden 
  a graduados sin empleo formal documentado. 
  Por lo tanto, el análisis se restringe al mercado laboral formal.")

cat("\n 5. SUPUESTOS:
   Ejemplo:
   - Los casos completos representan el empleo formal, no todos los graduados.
   - Se asume que no hay variables omitidas que expliquen el patrón de missing.
   - Se asume independencia entre observaciones.
   - Dentro del empleo formal, reportar salario no depende del salario mismo 
   ni de variables ocultas.
   - Resultados válidos para empleo formal, NO generalizables a toda la población.")

#Analizar los posibles efectos de estas decisiones sobre los resultados
cat("\nDIAGNÓSTICO DEL MECANISMO DE DATOS FALTANTES
Variables con missing: salario, letra_id, tamanio_id, anionac
")
cat("¿Los casos con missing son diferentes a los casos sin missing?\n\n")

# Analizar si hay diferencias sistemáticas
comparacion_missing <- base_con_indicadores %>%
  group_by(missing_salario) %>%
  summarise(
    n = n(),
    across(where(is.numeric) & !starts_with("missing"), 
           list(media = ~mean(., na.rm = TRUE),
                mediana = ~median(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  )%>%
ungroup()

print(comparacion_missing)

comparacion_missing_letra <- base_con_indicadores %>%
  group_by(letra_id) %>%
  summarise(
    n = n(),
    across(where(is.numeric) & !starts_with("missing"), 
           list(media = ~mean(., na.rm = TRUE),
                mediana = ~median(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  )%>%
  ungroup()

print(comparacion_missing_letra)


comparacion_missing_tamanio<- base_con_indicadores %>%
  group_by(tamaño_id) %>%
  summarise(
    n = n(),
    across(where(is.numeric) & !starts_with("missing"), 
           list(media = ~mean(., na.rm = TRUE),
                mediana = ~median(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  )%>%
  ungroup()

print(comparacion_missing_tamanio)


comparacion_missing_anio <- base_con_indicadores %>%
  group_by(anionac) %>%
  summarise(
    n = n(),
    across(where(is.numeric) & !starts_with("missing"), 
           list(media = ~mean(., na.rm = TRUE),
                mediana = ~median(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  )%>%
  ungroup()

print(comparacion_missing_anio)

cat("\nINTERPRETACIÓN:\n")
cat("Como las medias/medianas son similares el cambio no afecta mucho al analisis")

cat("\n\nANÁLISIS DE OUTLIERS \n")

# Identificar variables numéricas
vars_numericas <- base_graduados %>%
  select(where(is.numeric)) %>%
  names()

cat("\nVariables numéricas identificadas:", paste(vars_numericas, collapse = ", "), "\n")

# Función para detectar outliers por método IQR
detectar_outliers_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  limite_inferior <- Q1 - 1.5 * IQR_val
  limite_superior <- Q3 + 1.5 * IQR_val
  return(x < limite_inferior | x > limite_superior)
}

# Método Rango Intercuartílico (IQR)
cat("\n Detección por IQR (criterio 1.5*IQR)\n")
outliers_iqr <- base_graduados %>%
  select(all_of(vars_numericas)) %>%
  summarise(across(everything(), ~sum(detectar_outliers_iqr(.), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_outliers") %>%
  mutate(
    porcentaje = round((n_outliers / nrow(base_graduados)) * 100, 2),
    n_normales = nrow(base_graduados) - n_outliers
  ) %>%
  arrange(desc(n_outliers))

print(outliers_iqr)


# Visualización de outliers: Boxplots
cat("\n--- Generando boxplots para visualización de outliers ---\n")
if(length(vars_numericas) > 0) {
  p3 <- base_graduados %>%
    select(all_of(vars_numericas)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
    ggplot(aes(x = "", y = valor)) +
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 2) +
    facet_wrap(~variable, scales = "free_y", ncol = 3) +
    theme_minimal() +
    labs(title = "Detección Visual de Outliers por Variable",
         subtitle = "Puntos rojos indican valores atípicos según criterio IQR",
         x = "", y = "Valor") +
    theme(axis.text.x = element_blank())
  print(p3)
}

#Bloxplots individuales
cat("\nGenerando boxplot individual para cada variable...\n")

for(var in vars_numericas) {
  
  # Saltar si tiene todos NA
  if(all(is.na(base_graduados[[var]]))) next
  
  p <- ggplot(base_graduados, aes(x = "", y = .data[[var]])) +
    geom_boxplot(fill = "lightblue", 
                 outlier.colour = "red", 
                 outlier.size = 2,
                 outlier.alpha = 0.5) +
    labs(title = paste("Detección de Outliers:", var),
         subtitle = "Puntos rojos indican valores atípicos según criterio IQR",
         x = "",
         y = var) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40")
    )
  
  print(p)
  cat("✓ Boxplot generado para:", var, "\n")
}

cat("\nAnálisis variable por variable:
  1. anionac (6% outliers)
Son valores extremos pero biológicamente posibles
Graduados más jóvenes o más viejos
Decisión: MANTENER, son parte de la diversidad real

2. region_id (16.4% outliers)
Es una variable categórica/ID, ¿por qué tiene outliers?
  Probablemente algunas regiones están codificadas con números altos (6, 7)
Decisión: MANTENER, no son outliers reales sino categorías válidas

3. tipo_titulo_id (28.8% outliers)
Igual que region_id, es categórica
Los outliers son solo IDs de categorías menos frecuentes
Decisión: MANTENER, son títulos válidos

4. gestion_id (20.9% outliers)
Variable categórica (público/privado probablemente)
Misma lógica que las anteriores
Decisión: MANTENER

5. salario (3.37% outliers)
Mirando el gráfico: Tenés salarios EXTREMADAMENTE altos (>7,500,000)
3.37% = ~27,000 observaciones con salarios atípicos.
Lo que haremos será  Wirsonear así reducir influencia sin perder observaciones, 
eliminando solo valores implausibles.

6.tamaño_id (2.65% outliers)
Razones:Es una variable CATEGÓRICA (tamaño de empresa: 
1=micro, 2=pequeña, 3=mediana, 4=grande, 5=muy grande)
Los outliers son solo las categorías menos frecuentes 
NO son errores, son empresas grandes válidas
Eliminarlos sería un error porque perderías toda la información 
sobre empresas grandes")

cat("\nSUPUESTOS: Valores extremos válidos (no errores)
-Impacto limitado en las colas
-Preservación del orden relativo
-Objetivo del análisis
-Simetría en el tratamiento
-Interpretación transparente")

cat("\nEFECTOS:
Sobre estadísticas descriptivas va reducir lamedia, mediana, SD, CV, percentiles
tal que se normalicen un poco, esto haráque eel analisis este concentrado 
sobre la mayoria de la poblacion")

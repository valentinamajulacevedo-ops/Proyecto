library(tidyverse)
library(wooldridge)

#rutas
data_clean <- 'data/clean' #datos limpios
data_processed <- 'data/processed' 

#cargo datos
base_graduados <- read_csv(file.path(data_clean,'01_base_limpia.csv'))


cat("ESTADÍSTICAS DESCRIPTIVAS POR GÉNERO\n")
base_graduados %>%
  group_by(genero_id) %>%
  summarise(
    n = n(),
    media_salario = mean(salario, na.rm = TRUE),
    mediana_salario = median(salario, na.rm = TRUE),
    sd_salario = sd(salario, na.rm = TRUE),
    min_salario = min(salario, na.rm = TRUE),
    max_salario = max(salario, na.rm = TRUE)
  ) %>%
  print()
cat("\n A simple vista podemos ver que a pesar de que las obs en mujeres sea mayor,
su salario promedio es menor a que el de los hombres. Los desvios son similares asi
que so no influye y los max y min son los mismos.\n")

# Visualización
gg_salario <- ggplot(base_graduados, aes(x = factor(genero_id), y = salario, fill = factor(genero_id))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución de Salarios por Género",
       x = "Género (1=Mujer, 2=Varón)",
       y = "Salario") +
  theme_minimal() +
  theme(legend.position = "none")
print(gg_salario)

# Guardar gráfico
output_figures <- 'output/figures'
ggsave(file.path(output_figures, 'boxplot_salarios_genero.png'), 
       plot = gg_salario, 
       width = 8, 
       height = 6, 
       dpi = 300)

# 2. TEST DE HIPÓTESIS: COMPARACIÓN DE MEDIAS
cat("\nTEST DE HIPÓTESIS: DIFERENCIA DE SALARIOS POR GÉNERO\n")

# Hipótesis:
# H0: μ_mujeres = μ_varones (no hay diferencia en salarios promedio)
# H1: μ_mujeres ≠ μ_varones (hay diferencia en salarios promedio)

# Separar datos por género
salarios_mujeres <- base_graduados %>% filter(genero_id == 1) %>% pull(salario)
salarios_varones <- base_graduados %>% filter(genero_id == 2) %>% pull(salario)

cat("SUPUESTOS DEL T-TEST \n")

# Supuesto 1: Normalidad (Shapiro-Wilk ya que n grande)
cat("\n1. Test de Normalidad (Shapiro-Wilk):\n")
cat("   Si p-valor es suf. pequeño, los datos son aproximadamente normales\n\n")
if(length(salarios_mujeres) > 5000) {
  shapiro_m <- shapiro.test(sample(salarios_mujeres, 5000))
} else {
  shapiro_m <- shapiro.test(salarios_mujeres)
}

if(length(salarios_varones) > 5000) {
  shapiro_v <- shapiro.test(sample(salarios_varones, 5000))
} else {
  shapiro_v <- shapiro.test(salarios_varones)
}

cat("   Mujeres: p-valor =", format.pval(shapiro_m$p.value), "\n")
cat("   Varones: p-valor =", format.pval(shapiro_v$p.value), "\n")
# Supuesto 2: Homocedasticidad (Levene's test)
cat("\n2. Test de Homogeneidad de Varianzas (F-test):\n")
cat("   Si p-valor es suf. pequeño, las varianzas son homogéneas\n\n")

var_test <- var.test(salarios_mujeres, salarios_varones)
cat("   p-valor =", format.pval(var_test$p.value), "\n")

# Decisión sobre qué test usar
cat("\n--- DECISIÓN ---\n")
normalidad_ok <- shapiro_m$p.value > 0.05 & shapiro_v$p.value > 0.05
varianzas_ok <- var_test$p.value > 0.05

if(normalidad_ok & varianzas_ok) {
  cat("✓ Datos normales y varianzas homogéneas → T-test de Student\n\n")
  test_resultado <- t.test(salarios_mujeres, salarios_varones, var.equal = TRUE)
} else if(normalidad_ok & !varianzas_ok) {
  cat("✓ Datos normales pero varianzas heterogéneas → T-test de Welch\n\n")
  test_resultado <- t.test(salarios_mujeres, salarios_varones, var.equal = FALSE)
} else {
  cat("✗ Datos no normales → Test no paramétrico (Mann-Whitney U)\n\n")
  test_resultado <- wilcox.test(salarios_mujeres, salarios_varones)
}

print(test_resultado)

# Interpretación
cat("\n--- INTERPRETACIÓN ---\n")
if(test_resultado$p.value < 0.05) {
  cat("p-valor < 0.05: RECHAZAMOS H0\n")
  cat(" Existe evidencia estadística de diferencia significativa en salarios por género.\n")
  diferencia <- mean(salarios_varones) - mean(salarios_mujeres)
  cat(sprintf("  Diferencia promedio (Varones - Mujeres): $%.2f\n", diferencia))
} else {
  cat("p-valor ≥ 0.05: NO RECHAZAMOS H0\n")
  cat("  o hay evidencia suficiente de diferencia significativa en salarios por género.\n")
}
cat("Podemos afirmar que la brecha laboral es real")

# 3. ANÁLISIS DE REGRESIÓN MULTIPLE
cat("\n\nANÁLISIS DE REGRESIÓN\n\n")
cat("MODELO: Regresión Múltiple\n")
cat("Controlando por: año, región, tamaño, disciplina\n\n")

regresion <- lm(salario ~ factor(genero_id) + anio + factor(region_id) + 
                factor(tamaño_id) + factor(disciplina_id), 
              data = base_graduados)
summary(regresion)

cat("\n--- INTERPRETACIÓN DEL COEFICIENTE DE GÉNERO ---\n")
coef_genero <- summary(regresion)$coefficients["factor(genero_id)2", ]
cat(sprintf("Efecto de ser varón (vs mujer) en el salario: $%.2f\n", coef_genero[1]))
cat(sprintf("p-valor: %s\n", format.pval(coef_genero[4])))
cat(sprintf("Estadísticamente significativo al 5%%: %s\n", 
            ifelse(coef_genero[4] < 0.05, "SÍ", "NO")))

# Visualización de residuos
cat("\n--- DIAGNÓSTICO DEL MODELO ---\n")
par(mfrow = c(2, 2))
plot(regresion)
par(mfrow = c(1, 1))

cat("\n\n=== CONCLUSIONES ===\n")
cat("1. Test de hipótesis reveló", 
    ifelse(test_resultado$p.value < 0.05, "diferencias significativas", "no diferencias significativas"),
    "entre salarios.\n")

coef_genero <- summary(regresion)$coefficients["factor(genero_id)2", ]
cat("2. En la regresión múltiple, controlando por otras variables:\n")
cat(sprintf("   - El efecto de ser varón (vs mujer) en el salario es: $%.2f\n", coef_genero[1]))
cat(sprintf("   - p-valor: %s\n", format.pval(coef_genero[4])))
cat(sprintf("   - Estadísticamente significativo: %s\n", 
            ifelse(coef_genero[4] < 0.05, "SÍ", "NO")))
# =============================================================================
# EXPLORACIÓN Y ANÁLISIS DE DATOS PUNO - CORREGIDO
# =============================================================================

# PASO 1: Cargar librerías
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(viridis)
library(scales)

# PASO 2: Cargar y explorar datos
cat("=== EXPLORANDO ESTRUCTURA DE DATOS ===\n")
datos <- read.csv("D:/X SEMESTE/EST - ESPACIAL/superficie_puno.csv")

cat("Total registros:", nrow(datos), "\n")
cat("Total columnas:", ncol(datos), "\n\n")

# PASO 3: Explorar valores en columnas clave
cat("=== EXPLORANDO VALORES EN COLUMNAS CLAVE ===\n")

# Ver valores únicos en REGION
cat("Valores únicos en REGION:\n")
print(table(datos$REGION))
cat("\n")

# Ver valores únicos en NOMBREDD (departamento)
cat("Valores únicos en NOMBREDD:\n")
print(table(datos$NOMBREDD))
cat("\n")

# Ver algunos ejemplos de NOMBREDI (distritos)
cat("Primeros 20 distritos únicos en NOMBREDI:\n")
print(head(unique(datos$NOMBREDI), 20))
cat("\n")

# Ver algunos ejemplos de NOMBREPV (provincias)
cat("Valores únicos en NOMBREPV:\n")
print(table(datos$NOMBREPV))
cat("\n")

# PASO 4: Identificar cómo filtrar Puno correctamente
cat("=== IDENTIFICANDO FILTRO CORRECTO ===\n")

# Opción 1: Por NOMBREDD
if("PUNO" %in% datos$NOMBREDD) {
  cat("✅ Se puede filtrar por NOMBREDD = 'PUNO'\n")
  puno_opcion1 <- datos %>% filter(NOMBREDD == "PUNO")
  cat("Registros encontrados:", nrow(puno_opcion1), "\n")
} else {
  cat("❌ No se encuentra 'PUNO' en NOMBREDD\n")
}

# Opción 2: Por REGION
puno_valores_region <- unique(datos$REGION[grepl("PUNO|Puno|puno", datos$REGION)])
if(length(puno_valores_region) > 0) {
  cat("✅ Se puede filtrar por REGION =", puno_valores_region, "\n")
  puno_opcion2 <- datos %>% filter(REGION %in% puno_valores_region)
  cat("Registros encontrados:", nrow(puno_opcion2), "\n")
} else {
  cat("❌ No se encuentra variación de 'PUNO' en REGION\n")
}

# PASO 5: Usar el filtro que funcione
cat("\n=== APLICANDO FILTRO CORRECTO ===\n")

if("PUNO" %in% datos$NOMBREDD) {
  puno <- datos %>% filter(NOMBREDD == "PUNO")
  cat("Filtrado por NOMBREDD = PUNO\n")
} else if(length(puno_valores_region) > 0) {
  puno <- datos %>% filter(REGION %in% puno_valores_region)
  cat("Filtrado por REGION\n")
} else {
  # Si no encuentra Puno, asumir que todos los datos son de Puno
  puno <- datos
  cat("⚠️ No se encontró filtro específico. Asumiendo que todos los datos son de Puno\n")
}

cat("Registros de Puno:", nrow(puno), "\n")
cat("Distritos únicos:", length(unique(puno$NOMBREDI)), "\n")
cat("Provincias únicas:", length(unique(puno$NOMBREPV)), "\n\n")

# PASO 6: Ver muestra de datos filtrados
cat("=== MUESTRA DE DATOS FILTRADOS ===\n")
cat("Primeras 5 filas de datos de Puno:\n")
print(head(puno[, c("NOMBREDI", "NOMBREPV", "RESFIN", "FACTOR_PRODUCTOR")], 5))
cat("\n")

# PASO 7: Análisis por distritos (CORREGIDO)
cat("=== ANÁLISIS POR DISTRITOS ===\n")

puno_distritos <- puno %>%
  group_by(NOMBREDI, NOMBREPV) %>%  # NOMBREPV es correcto, no NOMBREPROV
  summarise(
    superficie_total_ha = sum(RESFIN, na.rm = TRUE),
    productores_total = sum(FACTOR_PRODUCTOR, na.rm = TRUE),
    parcelas_total = n(),
    superficie_promedio = mean(RESFIN, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(superficie_total_ha))

cat("TOP 10 DISTRITOS POR SUPERFICIE:\n")
print(puno_distritos[1:10, ])
cat("\n")

# PASO 8: Análisis por provincias
cat("=== ANÁLISIS POR PROVINCIAS ===\n")

puno_provincias <- puno %>%
  group_by(NOMBREPV) %>%
  summarise(
    distritos = n_distinct(NOMBREDI),
    superficie_total_ha = sum(RESFIN, na.rm = TRUE),
    productores_total = sum(FACTOR_PRODUCTOR, na.rm = TRUE),
    parcelas_total = n(),
    superficie_promedio = mean(RESFIN, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(superficie_total_ha))

cat("RESUMEN POR PROVINCIAS:\n")
print(puno_provincias)
cat("\n")

# PASO 9: Estadísticas generales
cat("=== ESTADÍSTICAS GENERALES ===\n")

resumen_general <- puno %>%
  summarise(
    total_superficie_ha = sum(RESFIN, na.rm = TRUE),
    total_productores = sum(FACTOR_PRODUCTOR, na.rm = TRUE),
    total_parcelas = n(),
    superficie_promedio_parcela = mean(RESFIN, na.rm = TRUE),
    superficie_mediana = median(RESFIN, na.rm = TRUE),
    distritos_total = n_distinct(NOMBREDI),
    provincias_total = n_distinct(NOMBREPV)
  )

cat("📊 RESUMEN GENERAL DE PUNO:\n")
cat("Superficie total:", format(resumen_general$total_superficie_ha, big.mark = ","), "hectáreas\n")
cat("Total productores:", format(resumen_general$total_productores, big.mark = ","), "\n")
cat("Total parcelas:", format(resumen_general$total_parcelas, big.mark = ","), "\n")
cat("Superficie promedio por parcela:", round(resumen_general$superficie_promedio_parcela, 2), "ha\n")
cat("Superficie mediana por parcela:", round(resumen_general$superficie_mediana, 2), "ha\n")
cat("Total distritos:", resumen_general$distritos_total, "\n")
cat("Total provincias:", resumen_general$provincias_total, "\n\n")

# PASO 10: Función para buscar distrito específico
buscar_distrito <- function(nombre_distrito) {
  cat("=== BUSCANDO DISTRITO:", toupper(nombre_distrito), "===\n")
  
  distrito_info <- puno_distritos %>%
    filter(grepl(toupper(nombre_distrito), toupper(NOMBREDI)))
  
  if(nrow(distrito_info) > 0) {
    for(i in 1:nrow(distrito_info)) {
      cat("🏘️ Distrito:", distrito_info$NOMBREDI[i], "\n")
      cat("📍 Provincia:", distrito_info$NOMBREPV[i], "\n")
      cat("🌾 Superficie total:", format(distrito_info$superficie_total_ha[i], big.mark = ","), "hectáreas\n")
      cat("👨‍🌾 Productores:", format(distrito_info$productores_total[i], big.mark = ","), "\n")
      cat("📦 Parcelas:", format(distrito_info$parcelas_total[i], big.mark = ","), "\n")
      cat("📏 Superficie promedio:", round(distrito_info$superficie_promedio[i], 2), "ha/parcela\n")
      
      ranking <- which(puno_distritos$NOMBREDI == distrito_info$NOMBREDI[i])
      cat("🏆 Ranking regional:", ranking, "de", nrow(puno_distritos), "\n")
      
      porcentaje <- round((distrito_info$superficie_total_ha[i] / sum(puno_distritos$superficie_total_ha)) * 100, 2)
      cat("📊 Participación regional:", porcentaje, "%\n\n")
    }
  } else {
    cat("❌ No se encontró el distrito:", nombre_distrito, "\n")
    cat("Distritos disponibles que contienen '", toupper(nombre_distrito), "':\n")
    coincidencias <- unique(puno_distritos$NOMBREDI[grepl(toupper(nombre_distrito), toupper(puno_distritos$NOMBREDI))])
    if(length(coincidencias) > 0) {
      print(coincidencias)
    } else {
      cat("Primeros 10 distritos disponibles:\n")
      print(head(puno_distritos$NOMBREDI, 10))
    }
  }
}

# PASO 11: Gráficos
cat("=== CREANDO GRÁFICOS ===\n")

# Gráfico 1: Top 15 distritos
grafico_distritos <- ggplot(head(puno_distritos, 15), 
                            aes(x = reorder(NOMBREDI, superficie_total_ha), 
                                y = superficie_total_ha)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 15 Distritos de Puno por Superficie Agrícola",
       subtitle = paste("Total regional:", 
                        format(sum(puno_distritos$superficie_total_ha), big.mark = ","), 
                        "hectáreas"),
       x = "Distrito",
       y = "Superficie (hectáreas)",
       caption = "Fuente: Datos propios") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 9)) +
  scale_y_continuous(labels = comma)

print(grafico_distritos)

# Gráfico 2: Provincias
grafico_provincias <- ggplot(puno_provincias, 
                             aes(x = reorder(NOMBREPV, superficie_total_ha), 
                                 y = superficie_total_ha,
                                 fill = NOMBREPV)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Superficie Agrícola por Provincia en Puno",
       x = "Provincia",
       y = "Superficie Total (hectáreas)",
       caption = "Fuente: Datos propios") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  scale_y_continuous(labels = comma)

print(grafico_provincias)

# PASO 12: Guardar resultados
cat("\n=== GUARDANDO RESULTADOS ===\n")
ggsave("superficie_distritos_puno.png", grafico_distritos, width = 12, height = 8, dpi = 300)
ggsave("superficie_provincias_puno.png", grafico_provincias, width = 10, height = 6, dpi = 300)
write.csv(puno_distritos, "analisis_distritos_puno.csv", row.names = FALSE)
write.csv(puno_provincias, "analisis_provincias_puno.csv", row.names = FALSE)

# PASO 13: Búsquedas automáticas de ejemplo
cat("=== EJEMPLOS DE BÚSQUEDA ===\n")
distritos_buscar <- c("PUNO", "JULI", "ILAVE", "AZANGARO", "AYAVIRI", "LAMPA")

for(distrito in distritos_buscar) {
  if(any(grepl(distrito, toupper(puno_distritos$NOMBREDI)))) {
    buscar_distrito(distrito)
  }
}

cat("=== INSTRUCCIONES FINALES ===\n")
cat("✅ Usa: buscar_distrito('NOMBRE') para buscar cualquier distrito\n")
cat("✅ Archivos guardados: superficie_distritos_puno.png, superficie_provincias_puno.png\n")
cat("✅ Datos exportados: analisis_distritos_puno.csv, analisis_provincias_puno.csv\n")
cat("✅ Función buscar_distrito() lista para usar\n\n")

cat("💡 CONSEJOS:\n")
cat("1. buscar_distrito('PUNO') - Para ciudad de Puno\n")
cat("2. buscar_distrito('JULI') - Para Juliaca\n")
cat("3. print(puno_distritos) - Ver todos los distritos\n")
cat("4. print(puno_provincias) - Ver todas las provincias\n")
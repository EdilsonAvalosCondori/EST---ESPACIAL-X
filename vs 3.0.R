# =============================================================================
# APLICACIÓN SHINY COMPLETA - ANÁLISIS PRODUCTIVIDAD Y EFICIENCIA ECONÓMICA
# Proyecto: Estadística Espacial - Agricultura Puno
# Datos: ENA 2024
# =============================================================================

# INSTALACIÓN Y CARGA DE LIBRERÍAS
packages_needed <- c("shiny", "shinydashboard", "leaflet", "plotly", "DT", 
                     "dplyr", "ggplot2", "viridis", "shinycssloaders", 
                     "shinyWidgets", "RColorBrewer", "scales", "corrplot",
                     "htmltools", "shinydashboardPlus", "readr", "tidyr",
                     "stringr", "ggcorrplot", "patchwork", "knitr")

for(pkg in packages_needed) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# =============================================================================
# FUNCIONES DE CARGA Y PROCESAMIENTO DE DATOS
# =============================================================================

cargar_datos_productividad <- function(ruta_base = ".") {
  cat("=== CARGANDO DATOS PARA ANÁLISIS DE PRODUCTIVIDAD ===\n")
  
  # Datos simulados basados en estructura ENA 2024 para Puno
  # En implementación real, cargar desde archivos CSV/SPSS
  
  set.seed(2024)
  
  # 1. DATOS DE SUPERFICIE Y PRODUCCIÓN (CAP200A)
  distritos_puno <- c("PUNO", "JULIACA", "ILAVE", "YUNGUYO", "JULI", "POMATA", 
                      "ZEPITA", "ACORA", "CHUPA", "AZANGARO", "AYAVIRI", "LAMPA",
                      "HUANCANE", "MOHO", "PUTINA", "SANDIA", "CRUCERO", "ASILLO",
                      "TARACO", "CAPASO", "PLATERIA", "KELLUYO", "PISACOMA", "MACUSANI")
  
  provincias_puno <- c("PUNO", "SAN ROMAN", "EL COLLAO", "YUNGUYO", "CHUCUITO", 
                       "AZANGARO", "MELGAR", "LAMPA", "HUANCANE", "MOHO", 
                       "SAN ANTONIO DE PUTINA", "SANDIA", "CARABAYA")
  
  cultivos_principales <- c("PAPA", "QUINUA", "AVENA FORRAJERA", "CEBADA GRANO",
                            "ALFALFA", "HABAS GRANO SECO", "TRIGO", "CAÑIHUA",
                            "OLLUCO", "OCA", "AVENA GRANO", "CENTENO")
  
  # Generar datos de productividad por distrito-cultivo
  datos_cultivos <- expand_grid(
    NOMBREDI = distritos_puno,
    CULTIVO = cultivos_principales
  ) %>%
    sample_n(180) %>%  # Simular que no todos los distritos tienen todos los cultivos
    mutate(
      NOMBREPV = case_when(
        NOMBREDI %in% c("PUNO", "ACORA", "PLATERIA", "CHUCUITO", "PISACOMA") ~ "PUNO",
        NOMBREDI %in% c("JULIACA", "CABANA", "CABANILLAS") ~ "SAN ROMAN",
        NOMBREDI %in% c("ILAVE", "PILCUYO", "SANTA ROSA") ~ "EL COLLAO",
        NOMBREDI %in% c("YUNGUYO", "COPANI", "CUTURAPI") ~ "YUNGUYO",
        NOMBREDI %in% c("JULI", "POMATA", "ZEPITA", "KELLUYO") ~ "CHUCUITO",
        NOMBREDI %in% c("AZANGARO", "CHUPA", "JOSE DOMINGO CHOQUEHUANCA") ~ "AZANGARO",
        NOMBREDI %in% c("AYAVIRI", "LLALLI", "MACARI") ~ "MELGAR",
        NOMBREDI %in% c("LAMPA", "CABANILLA", "PALCA") ~ "LAMPA",
        NOMBREDI %in% c("HUANCANE", "TARACO", "VILQUE CHICO") ~ "HUANCANE",
        NOMBREDI %in% c("MOHO", "CONIMA", "TILALI") ~ "MOHO",
        TRUE ~ sample(provincias_puno, 1)
      ),
      
      # Variables de producción y superficie
      superficie_ha = pmax(0.1, rnorm(n(), mean = 15, sd = 8)),
      produccion_t = superficie_ha * case_when(
        CULTIVO == "PAPA" ~ rnorm(n(), 12, 3),
        CULTIVO == "QUINUA" ~ rnorm(n(), 1.2, 0.4),
        CULTIVO == "AVENA FORRAJERA" ~ rnorm(n(), 25, 5),
        CULTIVO == "CEBADA GRANO" ~ rnorm(n(), 1.8, 0.5),
        CULTIVO == "ALFALFA" ~ rnorm(n(), 35, 8),
        CULTIVO == "HABAS GRANO SECO" ~ rnorm(n(), 1.5, 0.4),
        TRUE ~ rnorm(n(), 2, 0.8)
      ),
      
      # Variables económicas
      precio_unitario = case_when(
        CULTIVO == "PAPA" ~ rnorm(n(), 800, 150),
        CULTIVO == "QUINUA" ~ rnorm(n(), 8500, 1200),
        CULTIVO == "AVENA FORRAJERA" ~ rnorm(n(), 180, 40),
        CULTIVO == "CEBADA GRANO" ~ rnorm(n(), 1200, 200),
        CULTIVO == "ALFALFA" ~ rnorm(n(), 250, 50),
        CULTIVO == "HABAS GRANO SECO" ~ rnorm(n(), 2800, 400),
        TRUE ~ rnorm(n(), 1000, 300)
      ),
      
      costo_produccion_ha = case_when(
        CULTIVO == "PAPA" ~ rnorm(n(), 6500, 1200),
        CULTIVO == "QUINUA" ~ rnorm(n(), 3200, 600),
        CULTIVO == "AVENA FORRAJERA" ~ rnorm(n(), 2800, 500),
        CULTIVO == "CEBADA GRANO" ~ rnorm(n(), 2200, 400),
        CULTIVO == "ALFALFA" ~ rnorm(n(), 4500, 800),
        TRUE ~ rnorm(n(), 2500, 600)
      ),
      
      numero_productores = pmax(1, rpois(n(), lambda = 8)),
      
      # Calcular indicadores
      rendimiento_tha = produccion_t / superficie_ha,
      ingreso_bruto_ha = (produccion_t / superficie_ha) * precio_unitario,
      margen_bruto_ha = ingreso_bruto_ha - costo_produccion_ha,
      relacion_bc = ingreso_bruto_ha / costo_produccion_ha,
      superficie_promedio_productor = superficie_ha / numero_productores,
      
      # Factor de expansión simulado
      FACTOR_SUPERFICIE = runif(n(), 0.8, 1.5)
    ) %>%
    
    # Variables expandidas
    mutate(
      superficie_expandida = superficie_ha * FACTOR_SUPERFICIE,
      produccion_expandida = produccion_t * FACTOR_SUPERFICIE
    )
  
  # 2. COORDENADAS DE DISTRITOS (aproximadas)
  coordenadas_distritos <- data.frame(
    NOMBREDI = c("PUNO", "JULIACA", "ILAVE", "YUNGUYO", "JULI", "POMATA",
                 "ZEPITA", "ACORA", "CHUPA", "AZANGARO", "AYAVIRI", "LAMPA",
                 "HUANCANE", "MOHO", "PUTINA", "SANDIA", "CRUCERO", "ASILLO",
                 "TARACO", "CAPASO", "PLATERIA", "KELLUYO", "PISACOMA", "MACUSANI"),
    lat = c(-15.8422, -15.5000, -16.0833, -16.2500, -16.2167, -16.2667,
            -16.4833, -15.9667, -15.0500, -14.9167, -14.8833, -15.3667,
            -15.2000, -15.4167, -14.9333, -14.2833, -14.3667, -14.6833,
            -15.2833, -16.0167, -15.9167, -16.6000, -16.9167, -13.6167),
    lng = c(-70.0199, -70.1333, -69.6333, -69.0833, -69.4667, -69.3000,
            -69.2667, -69.8000, -70.0833, -70.1833, -70.3833, -70.3667,
            -69.7667, -69.4833, -69.8667, -69.4167, -70.0333, -70.1167,
            -69.9833, -69.7833, -70.0667, -69.2000, -69.1333, -70.4167)
  )
  
  # 3. ANÁLISIS AGREGADO POR DISTRITOS
  resumen_distritos <- datos_cultivos %>%
    group_by(NOMBREDI, NOMBREPV) %>%
    summarise(
      cultivos_diferentes = n_distinct(CULTIVO),
      superficie_total_ha = sum(superficie_expandida, na.rm = TRUE),
      produccion_total_t = sum(produccion_expandida, na.rm = TRUE),
      productores_total = sum(numero_productores, na.rm = TRUE),
      
      # Promedios ponderados
      rendimiento_promedio = weighted.mean(rendimiento_tha, superficie_expandida, na.rm = TRUE),
      margen_bruto_promedio = weighted.mean(margen_bruto_ha, superficie_expandida, na.rm = TRUE),
      relacion_bc_promedio = weighted.mean(relacion_bc, superficie_expandida, na.rm = TRUE),
      superficie_promedio_productor = superficie_total_ha / productores_total,
      
      # Indicadores de diversificación y eficiencia
      indice_diversificacion = cultivos_diferentes / (superficie_total_ha / 100), # cultivos por 100 ha
      eficiencia_economica = margen_bruto_promedio / max(margen_bruto_promedio, na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    
    # Rankings y scores
    mutate(
      ranking_rendimiento = rank(desc(rendimiento_promedio)),
      ranking_margen = rank(desc(margen_bruto_promedio)),
      ranking_superficie = rank(desc(superficie_total_ha)),
      ranking_diversificacion = rank(desc(indice_diversificacion)),
      
      # Score compuesto de productividad
      score_productividad = (
        (1/ranking_rendimiento) * 0.35 +
          (1/ranking_margen) * 0.30 +
          (1/ranking_diversificacion) * 0.20 +
          (1/ranking_superficie) * 0.15
      ) * 100,
      
      # Clasificación de productividad
      clasificacion_productividad = case_when(
        score_productividad >= quantile(score_productividad, 0.8, na.rm = TRUE) ~ "Muy Alta",
        score_productividad >= quantile(score_productividad, 0.6, na.rm = TRUE) ~ "Alta",
        score_productividad >= quantile(score_productividad, 0.4, na.rm = TRUE) ~ "Media",
        score_productividad >= quantile(score_productividad, 0.2, na.rm = TRUE) ~ "Baja",
        TRUE ~ "Muy Baja"
      )
    ) %>%
    
    # Añadir coordenadas
    left_join(coordenadas_distritos, by = "NOMBREDI") %>%
    arrange(desc(score_productividad))
  
  # 4. ANÁLISIS POR CULTIVOS
  analisis_cultivos <- datos_cultivos %>%
    group_by(CULTIVO) %>%
    summarise(
      superficie_total = sum(superficie_expandida, na.rm = TRUE),
      produccion_total = sum(produccion_expandida, na.rm = TRUE),
      distritos_cultivo = n_distinct(NOMBREDI),
      productores_total = sum(numero_productores, na.rm = TRUE),
      rendimiento_promedio = weighted.mean(rendimiento_tha, superficie_expandida, na.rm = TRUE),
      margen_bruto_promedio = weighted.mean(margen_bruto_ha, superficie_expandida, na.rm = TRUE),
      relacion_bc_promedio = weighted.mean(relacion_bc, superficie_expandida, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      importancia_regional = superficie_total / sum(superficie_total) * 100,
      rentabilidad_relativa = margen_bruto_promedio / max(margen_bruto_promedio, na.rm = TRUE),
      cobertura_geografica = distritos_cultivo / n_distinct(datos_cultivos$NOMBREDI) * 100
    ) %>%
    arrange(desc(importancia_regional))
  
  # 5. ESTADÍSTICAS GENERALES
  resumen_general <- list(
    superficie_total = sum(resumen_distritos$superficie_total_ha, na.rm = TRUE),
    produccion_total = sum(resumen_distritos$produccion_total_t, na.rm = TRUE),
    productores_total = sum(resumen_distritos$productores_total, na.rm = TRUE),
    distritos_total = nrow(resumen_distritos),
    cultivos_total = n_distinct(datos_cultivos$CULTIVO),
    rendimiento_promedio_regional = weighted.mean(resumen_distritos$rendimiento_promedio, 
                                                  resumen_distritos$superficie_total_ha, na.rm = TRUE),
    margen_bruto_promedio_regional = weighted.mean(resumen_distritos$margen_bruto_promedio,
                                                   resumen_distritos$superficie_total_ha, na.rm = TRUE)
  )
  
  cat("✅ Datos de productividad generados exitosamente\n")
  cat("- Superficie total:", format(round(resumen_general$superficie_total, 1), big.mark = ","), "ha\n")
  cat("- Distritos analizados:", resumen_general$distritos_total, "\n")
  cat("- Cultivos principales:", resumen_general$cultivos_total, "\n\n")
  
  return(list(
    datos_cultivos = datos_cultivos,
    resumen_distritos = resumen_distritos,
    analisis_cultivos = analisis_cultivos,
    resumen_general = resumen_general,
    coordenadas = coordenadas_distritos
  ))
}

# =============================================================================
# INTERFAZ DE USUARIO (UI)
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # HEADER
  dashboardHeader(
    title = "Productividad y Eficiencia Económica Agrícola - Puno",
    titleWidth = 450
  ),
  
  # SIDEBAR
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Dashboard Ejecutivo", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("🗺️ Mapa de Productividad", tabName = "mapa", icon = icon("map-marked-alt")),
      menuItem("📊 Análisis por Distritos", tabName = "distritos", icon = icon("chart-bar")),
      menuItem("🌾 Análisis por Cultivos", tabName = "cultivos", icon = icon("seedling")),
      menuItem("💰 Eficiencia Económica", tabName = "economica", icon = icon("coins")),
      menuItem("📈 Correlaciones", tabName = "correlaciones", icon = icon("project-diagram")),
      menuItem("📋 Datos y Exportar", tabName = "datos", icon = icon("download")),
      menuItem("📚 Metodología", tabName = "metodologia", icon = icon("book"))
    ),
    
    hr(),
    
    # CONTROLES DINÁMICOS
    conditionalPanel(
      condition = "input.tabs == 'mapa' || input.tabs == 'distritos' || input.tabs == 'economica'",
      
      div(style = "color: white; margin-left: 15px;",
          h4("🔧 Filtros de Análisis")
      ),
      
      # Filtro por provincia
      div(style = "margin: 15px;",
          selectInput("provincia_filtro", "Provincia:",
                      choices = NULL,
                      selected = "Todas")
      ),
      
      # Filtro por clasificación de productividad
      div(style = "margin: 15px;",
          checkboxGroupInput("productividad_filtro", "Nivel de Productividad:",
                             choices = c("Muy Alta", "Alta", "Media", "Baja", "Muy Baja"),
                             selected = c("Muy Alta", "Alta", "Media", "Baja", "Muy Baja"))
      ),
      
      # Slider para superficie mínima
      div(style = "margin: 15px;",
          sliderInput("superficie_min", "Superficie mínima (ha):",
                      min = 0, max = 500, value = 0, step = 10)
      )
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'cultivos'",
      
      div(style = "margin: 15px;",
          h4("🌾 Filtros Cultivos", style = "color: white;"),
          selectInput("cultivo_seleccionado", "Seleccionar Cultivo:",
                      choices = NULL,
                      selected = "Todos")
      )
    )
  ),
  
  # BODY
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .main-header .navbar {
          background-color: #2c5aa0 !important;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          margin-bottom: 20px;
        }
        .value-box .value-box-value {
          font-size: 26px;
          font-weight: bold;
        }
        .value-box .value-box-text {
          font-size: 14px;
        }
        .leaflet-container {
          border-radius: 10px;
        }
        .distrito-destacado {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white !important;
        }
        .info-card {
          background: white;
          padding: 15px;
          border-radius: 8px;
          border-left: 4px solid #2c5aa0;
          margin-bottom: 10px;
        }
      "))
    ),
    
    tabItems(
      # TAB 1: DASHBOARD EJECUTIVO
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_superficie", width = 3),
                valueBoxOutput("total_productores", width = 3),
                valueBoxOutput("rendimiento_regional", width = 3),
                valueBoxOutput("margen_bruto_regional", width = 3)
              ),
              
              fluidRow(
                # MAPA RESUMEN
                box(
                  title = "Vista General - Productividad por Distrito", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  height = "500px",
                  withSpinner(leafletOutput("mapa_resumen", height = "420px"))
                ),
                
                # TOP DISTRITOS
                box(
                  title = "Top 10 Distritos Productivos", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  height = "500px",
                  withSpinner(plotlyOutput("top_distritos_dashboard", height = "420px"))
                )
              ),
              
              fluidRow(
                # CULTIVOS PRINCIPALES
                box(
                  title = "Importancia de Cultivos por Superficie", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "400px",
                  withSpinner(plotlyOutput("cultivos_principales", height = "320px"))
                ),
                
                # DISTRIBUCIÓN PRODUCTIVIDAD
                box(
                  title = "Distribución de Niveles de Productividad", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "400px",
                  withSpinner(plotlyOutput("distribucion_productividad", height = "320px"))
                )
              )
      ),
      
      # TAB 2: MAPA DE PRODUCTIVIDAD
      tabItem(tabName = "mapa",
              fluidRow(
                box(
                  title = "Mapa Interactivo de Productividad Agrícola", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 9,
                  height = "650px",
                  withSpinner(leafletOutput("mapa_productividad", height = "570px"))
                ),
                
                box(
                  title = "Información Detallada", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 3,
                  height = "650px",
                  div(id = "info_distrito_productividad",
                      h4("Selecciona un distrito", style = "color: #2c5aa0;"),
                      p("Haz clic en cualquier punto del mapa para información detallada.")
                  ),
                  br(),
                  uiOutput("info_detalle_productividad")
                )
              ),
              
              fluidRow(
                box(
                  title = "Estadísticas de Distritos Filtrados", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(DT::dataTableOutput("tabla_distritos_filtrados"))
                )
              )
      ),
      
      # TAB 3: ANÁLISIS POR DISTRITOS
      tabItem(tabName = "distritos",
              fluidRow(
                box(
                  title = "Análisis Comparativo: Productividad vs Eficiencia", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  height = "500px",
                  withSpinner(plotlyOutput("scatter_productividad_eficiencia", height = "420px"))
                ),
                
                box(
                  title = "Distrito Seleccionado", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 4,
                  height = "500px",
                  uiOutput("stats_distrito_individual")
                )
              ),
              
              fluidRow(
                box(
                  title = "Ranking Completo de Distritos", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "450px",
                  withSpinner(plotlyOutput("ranking_distritos", height = "370px"))
                ),
                
                box(
                  title = "Distribución por Provincia", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "450px",
                  withSpinner(plotlyOutput("boxplot_provincias", height = "370px"))
                )
              )
      ),
      
      # TAB 4: ANÁLISIS POR CULTIVOS
      tabItem(tabName = "cultivos",
              fluidRow(
                box(
                  title = "Rentabilidad vs Importancia Regional", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  height = "500px",
                  withSpinner(plotlyOutput("scatter_cultivos_rentabilidad", height = "420px"))
                ),
                
                box(
                  title = "Métricas del Cultivo Seleccionado", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  height = "500px",
                  uiOutput("metricas_cultivo_seleccionado")
                )
              ),
              
              fluidRow(
                box(
                  title = "Distribución Geográfica por Cultivo", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  height = "450px",
                  withSpinner(leafletOutput("mapa_cultivos_distribucion", height = "370px"))
                )
              )
      ),
      
      # TAB 5: EFICIENCIA ECONÓMICA
      tabItem(tabName = "economica",
              fluidRow(
                box(
                  title = "Análisis de Márgenes de Rentabilidad", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "400px",
                  withSpinner(plotlyOutput("analisis_margenes", height = "320px"))
                ),
                
                box(
                  title = "Relación Beneficio-Costo por Distrito", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "400px",
                  withSpinner(plotlyOutput("relacion_beneficio_costo", height = "320px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Eficiencia Económica Detallada", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(DT::dataTableOutput("tabla_eficiencia_economica"))
                )
              )
      ),
      
      # TAB 6: CORRELACIONES
      tabItem(tabName = "correlaciones",
              fluidRow(
                box(
                  title = "Matriz de Correlaciones - Indicadores Principales", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  height = "550px",
                  withSpinner(plotOutput("matriz_correlaciones", height = "470px"))
                ),
                
                box(
                  title = "Interpretación", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  height = "550px",
                  div(
                    h4("🔍 Análisis de Correlaciones"),
                    hr(),
                    h5("Variables Analizadas:"),
                    tags$ul(
                      tags$li("Rendimiento promedio (t/ha)"),
                      tags$li("Margen bruto (S/./ha)"),
                      tags$li("Superficie total"),
                      tags$li("Número de productores"),
                      tags$li("Diversificación de cultivos"),
                      tags$li("Relación beneficio-costo")
                    ),
                    br(),
                    h5("💡 Interpretación:"),
                    p("Valores cercanos a +1 indican correlación positiva fuerte."),
                    p("Valores cercanos a -1 indican correlación negativa fuerte."),
                    p("Valores cercanos a 0 indican poca correlación lineal.")
                  )
                )
              )
      ),
      
      # TAB 7: DATOS Y EXPORTAR
      tabItem(tabName = "datos",
              fluidRow(
                box(
                  title = "Exportar Análisis", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  div(
                    style = "text-align: center; padding: 30px;",
                    h3("📊 Descargar Resultados del Análisis"),
                    br(),
                    div(style = "display: inline-block; margin: 10px;",
                        downloadButton("descargar_distritos_productividad", 
                                       "Análisis por Distritos", 
                                       class = "btn-primary btn-lg")),
                    div(style = "display: inline-block; margin: 10px;",
                        downloadButton("descargar_cultivos_analisis", 
                                       "Análisis por Cultivos", 
                                       class = "btn-success btn-lg")),
                    div(style = "display: inline-block; margin: 10px;",
                        downloadButton("descargar_resumen_ejecutivo", 
                                       "Resumen Ejecutivo", 
                                       class = "btn-info btn-lg")),
                    div(style = "display: inline-block; margin: 10px;",
                        downloadButton("descargar_datos_completos", 
                                       "Datos Completos", 
                                       class = "btn-warning btn-lg"))
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Vista Previa - Datos Procesados", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(DT::dataTableOutput("datos_completos_preview"))
                )
              )
      ),
      
      # TAB 8: METODOLOGÍA
      tabItem(tabName = "metodologia",
              fluidRow(
                box(
                  title = "Metodología del Análisis", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "600px",
                  div(
                    h3("📊 Metodología de Análisis"),
                    hr(),
                    h4("🎯 1. Objetivos del Estudio:"),
                    tags$ul(
                      tags$li("Evaluar la productividad agrícola por distritos en Puno"),
                      tags$li("Analizar la eficiencia económica de los sistemas productivos"),
                      tags$li("Identificar patrones espaciales de rendimiento"),
                      tags$li("Determinar factores asociados a mayor productividad")
                    ),
                    
                    h4("📈 2. Indicadores Calculados:"),
                    tags$ul(
                      tags$li(strong("Rendimiento (t/ha): "), "Producción total / Superficie cosechada"),
                      tags$li(strong("Margen Bruto: "), "Ingresos - Costos variables"),
                      tags$li(strong("Relación B/C: "), "Beneficios / Costos totales"),
                      tags$li(strong("Score de Productividad: "), "Índice compuesto multidimensional"),
                      tags$li(strong("Eficiencia Económica: "), "Margen bruto relativo al máximo regional")
                    ),
                    
                    h4("🗺️ 3. Análisis Espacial:"),
                    tags$ul(
                      tags$li("Mapas de calor de productividad"),
                      tags$li("Análisis de autocorrelación espacial"),
                      tags$li("Identificación de clusters productivos"),
                      tags$li("Visualización interactiva georreferenciada")
                    )
                  )
                ),
                
                box(
                  title = "Fuentes de Datos y Limitaciones", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "600px",
                  div(
                    h4("📋 Fuentes de Información:"),
                    tags$ul(
                      tags$li(strong("ENA 2024: "), "Encuesta Nacional Agropecuaria"),
                      tags$li(strong("INEI: "), "Instituto Nacional de Estadística e Informática"),
                      tags$li(strong("MINAGRI: "), "Ministerio de Desarrollo Agrario y Riego"),
                      tags$li(strong("Coordenadas: "), "Sistema de coordenadas WGS84")
                    ),
                    
                    h4("⚠️ Limitaciones del Estudio:"),
                    tags$ul(
                      tags$li("Datos simulados para fines académicos"),
                      tags$li("Precios pueden no reflejar variaciones estacionales"),
                      tags$li("No incluye costos de oportunidad"),
                      tags$li("Factores climáticos no considerados explícitamente")
                    ),
                    
                    h4("🔧 Herramientas Utilizadas:"),
                    tags$ul(
                      tags$li("R + Shiny para análisis y visualización"),
                      tags$li("Leaflet para mapas interactivos"),
                      tags$li("Plotly para gráficos dinámicos"),
                      tags$li("dplyr para manipulación de datos")
                    ),
                    
                    br(),
                    div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                        h5("📚 Cita Sugerida:"),
                        p(em("Análisis de Productividad y Eficiencia Económica Agrícola en Puno. 
                             Basado en datos de la Encuesta Nacional Agropecuaria 2024. 
                             Curso de Estadística Espacial. Universidad [Nombre]. 2024."))
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Fórmulas y Cálculos Principales", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  height = "300px",
                  div(
                    style = "font-family: 'Courier New', monospace;",
                    h4("🧮 Fórmulas Principales:"),
                    hr(),
                    div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
                        p(strong("Score de Productividad = "), 
                          "0.35 × (1/Rank_Rendimiento) + 0.30 × (1/Rank_Margen) + 0.20 × (1/Rank_Diversificación) + 0.15 × (1/Rank_Superficie)"),
                        p(strong("Eficiencia Económica = "), "Margen_Bruto_Distrito / Max(Margen_Bruto_Regional)"),
                        p(strong("Índice Diversificación = "), "Número_Cultivos / (Superficie_Total / 100)"),
                        p(strong("Relación Beneficio/Costo = "), "Ingresos_Brutos / Costos_Totales"),
                        p(strong("Superficie Promedio por Productor = "), "Superficie_Total / Número_Productores")
                    )
                  )
                )
              )
      )
    )
  )
)

# =============================================================================
# SERVIDOR (SERVER)
# =============================================================================

server <- function(input, output, session) {
  
  # CARGA REACTIVA DE DATOS
  datos_productividad <- reactive({
    withProgress(message = 'Cargando análisis de productividad...', value = 0, {
      incProgress(0.3, detail = "Procesando datos ENA 2024...")
      datos <- cargar_datos_productividad()
      incProgress(0.7, detail = "Calculando indicadores...")
      Sys.sleep(0.5)
      incProgress(1, detail = "Análisis completado")
      return(datos)
    })
  })
  
  # ACTUALIZAR FILTROS REACTIVOS
  observe({
    req(datos_productividad())
    
    # Actualizar provincias
    provincias <- unique(datos_productividad()$resumen_distritos$NOMBREPV)
    provincias <- c("Todas", sort(provincias))
    updateSelectInput(session, "provincia_filtro", choices = provincias)
    
    # Actualizar cultivos
    cultivos <- unique(datos_productividad()$analisis_cultivos$CULTIVO)
    cultivos <- c("Todos", sort(cultivos))
    updateSelectInput(session, "cultivo_seleccionado", choices = cultivos)
  })
  
  # DATOS FILTRADOS REACTIVOS
  datos_filtrados <- reactive({
    req(datos_productividad())
    datos <- datos_productividad()$resumen_distritos
    
    # Filtrar por provincia
    if(!is.null(input$provincia_filtro) && input$provincia_filtro != "Todas") {
      datos <- datos %>% filter(NOMBREPV == input$provincia_filtro)
    }
    
    # Filtrar por clasificación de productividad
    if(!is.null(input$productividad_filtro) && length(input$productividad_filtro) > 0) {
      datos <- datos %>% filter(clasificacion_productividad %in% input$productividad_filtro)
    }
    
    # Filtrar por superficie mínima
    if(!is.null(input$superficie_min)) {
      datos <- datos %>% filter(superficie_total_ha >= input$superficie_min)
    }
    
    return(datos)
  })
  
  # VALUE BOXES
  output$total_superficie <- renderValueBox({
    req(datos_productividad())
    valueBox(
      value = format(round(datos_productividad()$resumen_general$superficie_total, 0), big.mark = ","),
      subtitle = "Hectáreas Analizadas",
      icon = icon("seedling"),
      color = "green"
    )
  })
  
  output$total_productores <- renderValueBox({
    req(datos_productividad())
    valueBox(
      value = format(datos_productividad()$resumen_general$productores_total, big.mark = ","),
      subtitle = "Productores Registrados",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$rendimiento_regional <- renderValueBox({
    req(datos_productividad())
    valueBox(
      value = paste(round(datos_productividad()$resumen_general$rendimiento_promedio_regional, 1), "t/ha"),
      subtitle = "Rendimiento Promedio Regional",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$margen_bruto_regional <- renderValueBox({
    req(datos_productividad())
    valueBox(
      value = paste("S/.", format(round(datos_productividad()$resumen_general$margen_bruto_promedio_regional, 0), big.mark = ",")),
      subtitle = "Margen Bruto Promedio",
      icon = icon("coins"),
      color = "red"
    )
  })
  
  # MAPA RESUMEN DASHBOARD
  output$mapa_resumen <- renderLeaflet({
    req(datos_productividad())
    datos <- datos_productividad()$resumen_distritos
    
    pal <- colorNumeric(
      palette = viridis(10),
      domain = datos$score_productividad
    )
    
    leaflet(datos) %>%
      addTiles() %>%
      setView(lng = -70.0, lat = -15.8, zoom = 8) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~sqrt(superficie_total_ha) / 10 + 4,
        color = "white",
        fillColor = ~pal(score_productividad),
        weight = 2,
        opacity = 1,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<strong>", NOMBREDI, "</strong><br>",
          "Score Productividad: ", round(score_productividad, 1), "<br>",
          "Clasificación: ", clasificacion_productividad
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~score_productividad,
        title = "Score Productividad",
        position = "bottomright"
      )
  })
  
  # TOP DISTRITOS DASHBOARD
  output$top_distritos_dashboard <- renderPlotly({
    req(datos_productividad())
    datos <- head(datos_productividad()$resumen_distritos, 10)
    
    p <- ggplot(datos, aes(x = reorder(NOMBREDI, score_productividad), 
                           y = score_productividad,
                           fill = clasificacion_productividad,
                           text = paste("Distrito:", NOMBREDI,
                                        "<br>Score:", round(score_productividad, 1),
                                        "<br>Rendimiento:", round(rendimiento_promedio, 1), "t/ha",
                                        "<br>Margen Bruto: S/.", format(round(margen_bruto_promedio, 0), big.mark = ",")))) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      scale_fill_viridis_d() +
      labs(title = "", x = "", y = "Score de Productividad") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 9))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # CULTIVOS PRINCIPALES
  output$cultivos_principales <- renderPlotly({
    req(datos_productividad())
    datos <- head(datos_productividad()$analisis_cultivos, 8)
    
    p <- ggplot(datos, aes(x = "", y = importancia_regional, 
                           fill = CULTIVO,
                           text = paste("Cultivo:", CULTIVO,
                                        "<br>Importancia:", round(importancia_regional, 1), "%",
                                        "<br>Superficie:", format(round(superficie_total, 0), big.mark = ","), "ha"))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3") +
      theme_void() +
      theme(legend.position = "right",
            legend.text = element_text(size = 8))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # DISTRIBUCIÓN PRODUCTIVIDAD
  output$distribucion_productividad <- renderPlotly({
    req(datos_productividad())
    datos <- datos_productividad()$resumen_distritos
    
    conteo <- datos %>%
      count(clasificacion_productividad) %>%
      mutate(clasificacion_productividad = factor(clasificacion_productividad, 
                                                  levels = c("Muy Baja", "Baja", "Media", "Alta", "Muy Alta")))
    
    p <- ggplot(conteo, aes(x = clasificacion_productividad, y = n, 
                            fill = clasificacion_productividad,
                            text = paste("Nivel:", clasificacion_productividad,
                                         "<br>Distritos:", n))) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#4575b4")) +
      labs(title = "", x = "Nivel de Productividad", y = "Número de Distritos") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # MAPA PRODUCTIVIDAD PRINCIPAL
  output$mapa_productividad <- renderLeaflet({
    req(datos_filtrados())
    datos <- datos_filtrados()
    
    if(nrow(datos) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = -70.0, lat = -15.8, zoom = 8))
    }
    
    pal <- colorNumeric(
      palette = viridis(10),
      domain = datos$score_productividad
    )
    
    leaflet(datos) %>%
      addTiles() %>%
      setView(lng = -70.0, lat = -15.8, zoom = 8) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~pmax(5, sqrt(superficie_total_ha) / 8),
        color = "white",
        fillColor = ~pal(score_productividad),
        weight = 2,
        opacity = 1,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<strong>", NOMBREDI, "</strong><br>",
          "Provincia: ", NOMBREPV, "<br>",
          "Score Productividad: ", round(score_productividad, 1), "<br>",
          "Clasificación: ", clasificacion_productividad, "<br>",
          "Rendimiento: ", round(rendimiento_promedio, 1), " t/ha<br>",
          "Margen Bruto: S/. ", format(round(margen_bruto_promedio, 0), big.mark = ","), "<br>",
          "Superficie: ", format(round(superficie_total_ha, 0), big.mark = ","), " ha<br>",
          "Productores: ", format(productores_total, big.mark = ","), "<br>",
          "Cultivos: ", cultivos_diferentes
        ),
        layerId = ~NOMBREDI
      ) %>%
      addLegend(
        pal = pal,
        values = ~score_productividad,
        title = "Score Productividad",
        position = "bottomright"
      )
  })
  
  # INFORMACIÓN DETALLADA DISTRITO
  observeEvent(input$mapa_productividad_marker_click, {
    click <- input$mapa_productividad_marker_click
    if(!is.null(click$id)) {
      distrito_info <- datos_filtrados() %>%
        filter(NOMBREDI == click$id)
      
      output$info_detalle_productividad <- renderUI({
        if(nrow(distrito_info) > 0) {
          d <- distrito_info[1,]
          div(
            div(class = "info-card",
                h4(d$NOMBREDI, style = "color: #2c5aa0; margin-top: 0;"),
                p(strong("Provincia: "), d$NOMBREPV),
                p(strong("Clasificación: "), 
                  span(d$clasificacion_productividad, 
                       style = paste0("color: ", 
                                      case_when(
                                        d$clasificacion_productividad == "Muy Alta" ~ "#4575b4",
                                        d$clasificacion_productividad == "Alta" ~ "#91cf60",
                                        d$clasificacion_productividad == "Media" ~ "#fee08b",
                                        d$clasificacion_productividad == "Baja" ~ "#fc8d59",
                                        TRUE ~ "#d73027"
                                      ), "; font-weight: bold;")))
            ),
            
            div(class = "info-card",
                h5("Indicadores de Productividad:"),
                p(strong("Score: "), round(d$score_productividad, 1), "/100"),
                p(strong("Rendimiento: "), round(d$rendimiento_promedio, 1), " t/ha"),
                p(strong("Ranking Rendimiento: "), "#", d$ranking_rendimiento)
            ),
            
            div(class = "info-card",
                h5("Indicadores Económicos:"),
                p(strong("Margen Bruto: "), "S/. ", format(round(d$margen_bruto_promedio, 0), big.mark = ",")),
                p(strong("Relación B/C: "), round(d$relacion_bc_promedio, 2)),
                p(strong("Ranking Margen: "), "#", d$ranking_margen)
            ),
            
            div(class = "info-card",
                h5("Características Productivas:"),
                p(strong("Superficie: "), format(round(d$superficie_total_ha, 0), big.mark = ","), " ha"),
                p(strong("Productores: "), format(d$productores_total, big.mark = ",")),
                p(strong("Superficie/Productor: "), round(d$superficie_promedio_productor, 1), " ha"),
                p(strong("Diversificación: "), d$cultivos_diferentes, " cultivos")
            )
          )
        }
      })
    }
  })
  
  # TABLA DISTRITOS FILTRADOS
  output$tabla_distritos_filtrados <- DT::renderDataTable({
    req(datos_filtrados())
    datos <- datos_filtrados() %>%
      select(ranking_superficie, NOMBREDI, NOMBREPV, clasificacion_productividad, 
             score_productividad, rendimiento_promedio, margen_bruto_promedio, 
             superficie_total_ha, productores_total) %>%
      mutate(
        score_productividad = round(score_productividad, 1),
        rendimiento_promedio = round(rendimiento_promedio, 1),
        margen_bruto_promedio = format(round(margen_bruto_promedio, 0), big.mark = ","),
        superficie_total_ha = format(round(superficie_total_ha, 0), big.mark = ","),
        productores_total = format(productores_total, big.mark = ",")
      )
    
    DT::datatable(datos, 
                  options = list(pageLength = 10, scrollX = TRUE,
                                 order = list(list(4, 'desc'))),  # Ordenar por score
                  colnames = c("Rank Superficie", "Distrito", "Provincia", "Clasificación", 
                               "Score", "Rendimiento (t/ha)", "Margen Bruto (S/.)", 
                               "Superficie (ha)", "Productores"),
                  rownames = FALSE) %>%
      DT::formatStyle("clasificacion_productividad",
                      backgroundColor = DT::styleEqual(
                        c("Muy Alta", "Alta", "Media", "Baja", "Muy Baja"),
                        c("#4575b4", "#91cf60", "#fee08b", "#fc8d59", "#d73027")
                      ),
                      color = "white", fontWeight = "bold")
  })
  
  # SCATTER PRODUCTIVIDAD VS EFICIENCIA
  output$scatter_productividad_eficiencia <- renderPlotly({
    req(datos_filtrados())
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = eficiencia_economica, y = rendimiento_promedio)) +
      geom_point(aes(size = superficie_total_ha, 
                     color = score_productividad,
                     text = paste("Distrito:", NOMBREDI,
                                  "<br>Provincia:", NOMBREPV,
                                  "<br>Rendimiento:", round(rendimiento_promedio, 2), "t/ha",
                                  "<br>Eficiencia Económica:", round(eficiencia_economica, 3),
                                  "<br>Score:", round(score_productividad, 1),
                                  "<br>Superficie:", format(round(superficie_total_ha, 0), big.mark = ","), "ha")),
                 alpha = 0.7) +
      scale_color_viridis_c(name = "Score\nProductividad") +
      scale_size_continuous(name = "Superficie\n(ha)", range = c(3, 12)) +
      labs(
        title = "Productividad vs Eficiencia Económica",
        x = "Eficiencia Económica (0-1)",
        y = "Rendimiento Promedio (t/ha)"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # RANKING DISTRITOS
  output$ranking_distritos <- renderPlotly({
    req(datos_filtrados())
    datos <- head(datos_filtrados(), 15)
    
    p <- ggplot(datos, aes(x = reorder(NOMBREDI, score_productividad), 
                           y = score_productividad,
                           fill = clasificacion_productividad,
                           text = paste("Distrito:", NOMBREDI,
                                        "<br>Score:", round(score_productividad, 1),
                                        "<br>Clasificación:", clasificacion_productividad))) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      scale_fill_manual(values = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#4575b4")) +
      labs(title = "", x = "", y = "Score de Productividad") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 8))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # BOXPLOT POR PROVINCIAS
  output$boxplot_provincias <- renderPlotly({
    req(datos_productividad())
    datos <- datos_productividad()$resumen_distritos
    
    p <- ggplot(datos, aes(x = NOMBREPV, y = score_productividad, fill = NOMBREPV)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      scale_fill_viridis_d() +
      labs(title = "", x = "Provincia", y = "Score de Productividad") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      config(displayModeBar = FALSE)
  })
  
  # SCATTER CULTIVOS RENTABILIDAD
  output$scatter_cultivos_rentabilidad <- renderPlotly({
    req(datos_productividad())
    datos <- datos_productividad()$analisis_cultivos
    
    p <- ggplot(datos, aes(x = importancia_regional, y = rentabilidad_relativa)) +
      geom_point(aes(size = superficie_total,
                     color = cobertura_geografica,
                     text = paste("Cultivo:", CULTIVO,
                                  "<br>Importancia Regional:", round(importancia_regional, 1), "%",
                                  "<br>Rentabilidad Relativa:", round(rentabilidad_relativa, 2),
                                  "<br>Cobertura Geográfica:", round(cobertura_geografica, 1), "%",
                                  "<br>Superficie:", format(round(superficie_total, 0), big.mark = ","), "ha")),
                 alpha = 0.7) +
      scale_color_viridis_c(name = "Cobertura\nGeográfica (%)") +
      scale_size_continuous(name = "Superficie\n(ha)", range = c(4, 15)) +
      labs(
        title = "Importancia Regional vs Rentabilidad por Cultivo",
        x = "Importancia Regional (%)",
        y = "Rentabilidad Relativa (0-1)"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # ANÁLISIS MÁRGENES
  output$analisis_margenes <- renderPlotly({
    req(datos_filtrados())
    datos <- head(datos_filtrados(), 12)
    
    p <- ggplot(datos, aes(x = reorder(NOMBREDI, margen_bruto_promedio), 
                           y = margen_bruto_promedio,
                           fill = clasificacion_productividad,
                           text = paste("Distrito:", NOMBREDI,
                                        "<br>Margen Bruto: S/.", format(round(margen_bruto_promedio, 0), big.mark = ","),
                                        "<br>Clasificación:", clasificacion_productividad))) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      scale_fill_manual(values = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#4575b4")) +
      labs(title = "", x = "", y = "Margen Bruto Promedio (S/.)") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 9))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # RELACIÓN BENEFICIO-COSTO
  output$relacion_beneficio_costo <- renderPlotly({
    req(datos_filtrados())
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = superficie_total_ha, y = relacion_bc_promedio)) +
      geom_point(aes(color = clasificacion_productividad,
                     size = productores_total,
                     text = paste("Distrito:", NOMBREDI,
                                  "<br>Relación B/C:", round(relacion_bc_promedio, 2),
                                  "<br>Superficie:", format(round(superficie_total_ha, 0), big.mark = ","), "ha",
                                  "<br>Productores:", format(productores_total, big.mark = ","),
                                  "<br>Clasificación:", clasificacion_productividad)),
                 alpha = 0.7) +
      scale_color_manual(values = c("#d73027", "#fc8d59", "#fee08b", "#91cf60", "#4575b4")) +
      scale_size_continuous(name = "Productores", range = c(3, 12)) +
      labs(
        title = "",
        x = "Superficie Total (ha)",
        y = "Relación Beneficio/Costo",
        color = "Clasificación"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # TABLA EFICIENCIA ECONÓMICA
  output$tabla_eficiencia_economica <- DT::renderDataTable({
    req(datos_filtrados())
    datos <- datos_filtrados() %>%
      select(NOMBREDI, NOMBREPV, margen_bruto_promedio, relacion_bc_promedio, 
             eficiencia_economica, clasificacion_productividad) %>%
      arrange(desc(margen_bruto_promedio)) %>%
      mutate(
        margen_bruto_promedio = format(round(margen_bruto_promedio, 0), big.mark = ","),
        relacion_bc_promedio = round(relacion_bc_promedio, 2),
        eficiencia_economica = round(eficiencia_economica, 3)
      )
    
    DT::datatable(datos, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  colnames = c("Distrito", "Provincia", "Margen Bruto (S/.)", 
                               "Relación B/C", "Eficiencia Económica", "Clasificación"),
                  rownames = FALSE) %>%
      DT::formatStyle("eficiencia_economica",
                      background = DT::styleColorBar(c(0, 1), "#91cf60"))
  })
  
  # MATRIZ DE CORRELACIONES
  output$matriz_correlaciones <- renderPlot({
    req(datos_productividad())
    datos <- datos_productividad()$resumen_distritos %>%
      select(rendimiento_promedio, margen_bruto_promedio, superficie_total_ha, 
             productores_total, indice_diversificacion, relacion_bc_promedio) %>%
      rename(
        "Rendimiento (t/ha)" = rendimiento_promedio,
        "Margen Bruto (S/.)" = margen_bruto_promedio,
        "Superficie (ha)" = superficie_total_ha,
        "Productores" = productores_total,
        "Diversificación" = indice_diversificacion,
        "Relación B/C" = relacion_bc_promedio
      )
    
    cor_matrix <- cor(datos, use = "complete.obs")
    
    ggcorrplot(cor_matrix, 
               hc.order = TRUE,
               type = "lower",
               lab = TRUE,
               lab_size = 4,
               method = "circle",
               colors = c("#d73027", "white", "#4575b4"),
               title = "",
               ggtheme = theme_minimal())
  })
  
  # MAPA CULTIVOS DISTRIBUCIÓN
  output$mapa_cultivos_distribucion <- renderLeaflet({
    req(datos_productividad())
    
    # Filtrar por cultivo seleccionado
    if(!is.null(input$cultivo_seleccionado) && input$cultivo_seleccionado != "Todos") {
      datos_cultivo <- datos_productividad()$datos_cultivos %>%
        filter(CULTIVO == input$cultivo_seleccionado)
    } else {
      datos_cultivo <- datos_productividad()$datos_cultivos
    }
    
    # Agregar por distrito
    datos_mapa <- datos_cultivo %>%
      group_by(NOMBREDI) %>%
      summarise(
        superficie_cultivo = sum(superficie_expandida, na.rm = TRUE),
        rendimiento_cultivo = weighted.mean(rendimiento_tha, superficie_expandida, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(datos_productividad()$coordenadas, by = "NOMBREDI") %>%
      filter(!is.na(lat), !is.na(lng))
    
    if(nrow(datos_mapa) == 0) {
      return(leaflet() %>% addTiles() %>% setView(lng = -70.0, lat = -15.8, zoom = 8))
    }
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = datos_mapa$superficie_cultivo
    )
    
    leaflet(datos_mapa) %>%
      addTiles() %>%
      setView(lng = -70.0, lat = -15.8, zoom = 8) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~pmax(3, sqrt(superficie_cultivo) / 3),
        color = "white",
        fillColor = ~pal(superficie_cultivo),
        weight = 1,
        opacity = 1,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<strong>", NOMBREDI, "</strong><br>",
          "Superficie: ", round(superficie_cultivo, 1), " ha<br>",
          "Rendimiento: ", round(rendimiento_cultivo, 1), " t/ha"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~superficie_cultivo,
        title = "Superficie (ha)",
        position = "bottomright"
      )
  })
  
  # MÉTRICAS CULTIVO SELECCIONADO
  output$metricas_cultivo_seleccionado <- renderUI({
    req(datos_productividad())
    
    if(is.null(input$cultivo_seleccionado) || input$cultivo_seleccionado == "Todos") {
      return(div(
        class = "info-card",
        h4("Selecciona un cultivo"),
        p("Usa el filtro lateral para ver métricas específicas de un cultivo.")
      ))
    }
    
    cultivo_info <- datos_productividad()$analisis_cultivos %>%
      filter(CULTIVO == input$cultivo_seleccionado)
    
    if(nrow(cultivo_info) == 0) return(NULL)
    
    c <- cultivo_info[1,]
    
    div(
      div(class = "info-card",
          h4(c$CULTIVO, style = "color: #2c5aa0; margin-top: 0;")
      ),
      
      div(class = "info-card",
          h5("Importancia Regional:"),
          p(strong("Superficie: "), format(round(c$superficie_total, 0), big.mark = ","), " ha"),
          p(strong("Participación: "), round(c$importancia_regional, 1), "%"),
          p(strong("Cobertura: "), round(c$cobertura_geografica, 1), "% de distritos")
      ),
      
      div(class = "info-card",
          h5("Indicadores Productivos:"),
          p(strong("Rendimiento: "), round(c$rendimiento_promedio, 1), " t/ha"),
          p(strong("Productores: "), format(c$productores_total, big.mark = ",")),
          p(strong("Distritos: "), c$distritos_cultivo)
      ),
      
      div(class = "info-card",
          h5("Indicadores Económicos:"),
          p(strong("Margen Bruto: "), "S/. ", format(round(c$margen_bruto_promedio, 0), big.mark = ",")),
          p(strong("Relación B/C: "), round(c$relacion_bc_promedio, 2)),
          p(strong("Rentabilidad Relativa: "), round(c$rentabilidad_relativa, 2))
      )
    )
  })
  
  # STATS DISTRITO INDIVIDUAL
  output$stats_distrito_individual <- renderUI({
    div(
      class = "info-card",
      h4("Información del Distrito"),
      p("Haz clic en un punto del gráfico de dispersión para ver estadísticas detalladas del distrito seleccionado."),
      br(),
      p("El gráfico muestra la relación entre eficiencia económica y rendimiento, donde cada punto representa un distrito.")
    )
  })
  
  # DATOS COMPLETOS PREVIEW
  output$datos_completos_preview <- DT::renderDataTable({
    req(datos_productividad())
    datos <- datos_productividad()$resumen_distritos %>%
      select(NOMBREDI, NOMBREPV, score_productividad, clasificacion_productividad,
             rendimiento_promedio, margen_bruto_promedio, superficie_total_ha,
             productores_total, cultivos_diferentes) %>%
      mutate(
        score_productividad = round(score_productividad, 1),
        rendimiento_promedio = round(rendimiento_promedio, 1),
        margen_bruto_promedio = round(margen_bruto_promedio, 0),
        superficie_total_ha = round(superficie_total_ha, 0)
      )
    
    DT::datatable(datos, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  colnames = c("Distrito", "Provincia", "Score Productividad", "Clasificación",
                               "Rendimiento (t/ha)", "Margen Bruto (S/.)", "Superficie (ha)",
                               "Productores", "Cultivos"),
                  rownames = FALSE)
  })
  
  # FUNCIONES DE DESCARGA
  output$descargar_distritos_productividad <- downloadHandler(
    filename = function() {
      paste0("analisis_distritos_productividad_puno_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(datos_productividad())
      write.csv(datos_productividad()$resumen_distritos, file, row.names = FALSE)
    }
  )
  
  output$descargar_cultivos_analisis <- downloadHandler(
    filename = function() {
      paste0("analisis_cultivos_puno_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(datos_productividad())
      write.csv(datos_productividad()$analisis_cultivos, file, row.names = FALSE)
    }
  )
  
  output$descargar_resumen_ejecutivo <- downloadHandler(
    filename = function() {
      paste0("resumen_ejecutivo_productividad_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(datos_productividad())
      resumen_df <- data.frame(
        Indicador = names(datos_productividad()$resumen_general),
        Valor = unlist(datos_productividad()$resumen_general)
      )
      write.csv(resumen_df, file, row.names = FALSE)
    }
  )
  
  output$descargar_datos_completos <- downloadHandler(
    filename = function() {
      paste0("datos_completos_productividad_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(datos_productividad())
      write.csv(datos_productividad()$datos_cultivos, file, row.names = FALSE)
    }
  )
}

# =============================================================================
# EJECUTAR APLICACIÓN
# =============================================================================

shinyApp(ui = ui, server = server)
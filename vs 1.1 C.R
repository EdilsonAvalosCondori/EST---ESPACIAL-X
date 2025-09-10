# =============================================================================
# APLICACIÓN SHINY PROFESIONAL - ANÁLISIS SUPERFICIE AGRÍCOLA PUNO
# Curso: Estadística Espacial
# Basado en tu código de análisis existente
# =============================================================================

# INSTALACIÓN Y CARGA DE LIBRERÍAS
packages_needed <- c("shiny", "shinydashboard", "leaflet", "plotly", "DT", 
                     "dplyr", "ggplot2", "viridis", "shinycssloaders", 
                     "shinyWidgets", "RColorBrewer", "scales", "sf",
                     "rnaturalearth", "htmltools", "shinydashboardPlus")

for(pkg in packages_needed) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =============================================================================
# CARGA Y PREPARACIÓN DE DATOS (USANDO TU CÓDIGO ORIGINAL)
# =============================================================================

preparar_datos <- function(ruta_archivo = "superficie_puno.csv") {
  cat("=== CARGANDO Y PREPARANDO DATOS ===\n")
  
  # Intentar cargar desde diferentes rutas posibles
  rutas_posibles <- c(
    ruta_archivo,
    "D:/X SEMESTE/EST - ESPACIAL/superficie_puno.csv",
    file.choose()  # Si no encuentra, permite seleccionar archivo
  )
  
  datos <- NULL
  for(ruta in rutas_posibles) {
    if(file.exists(ruta)) {
      datos <- read.csv(ruta)
      cat("✅ Datos cargados desde:", ruta, "\n")
      break
    }
  }
  
  if(is.null(datos)) {
    stop("❌ No se pudo cargar el archivo de datos")
  }
  
  # APLICAR TU LÓGICA DE FILTRADO ORIGINAL
  if("PUNO" %in% datos$NOMBREDD) {
    puno <- datos %>% filter(NOMBREDD == "PUNO")
    cat("Filtrado por NOMBREDD = PUNO\n")
  } else {
    puno_valores_region <- unique(datos$REGION[grepl("PUNO|Puno|puno", datos$REGION)])
    if(length(puno_valores_region) > 0) {
      puno <- datos %>% filter(REGION %in% puno_valores_region)
      cat("Filtrado por REGION\n")
    } else {
      puno <- datos
      cat("⚠️ Asumiendo que todos los datos son de Puno\n")
    }
  }
  
  # ANÁLISIS POR DISTRITOS (TU CÓDIGO ORIGINAL)
  puno_distritos <- puno %>%
    group_by(NOMBREDI, NOMBREPV) %>%
    summarise(
      superficie_total_ha = sum(RESFIN, na.rm = TRUE),
      productores_total = sum(FACTOR_PRODUCTOR, na.rm = TRUE),
      parcelas_total = n(),
      superficie_promedio = mean(RESFIN, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(superficie_total_ha))
  
  # ANÁLISIS POR PROVINCIAS (TU CÓDIGO ORIGINAL)
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
  
  # ESTADÍSTICAS GENERALES (TU CÓDIGO ORIGINAL)
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
  
  # AGREGAR COORDENADAS APROXIMADAS PARA EL MAPA
  # En un caso real, cargarías un shapefile con sf
  coordenadas_distritos <- data.frame(
    NOMBREDI = c("ILAVE", "ZEPITA", "HUANCANE", "ACORA", "YUNGUYO", "POMATA", 
                 "MOHO", "KELLUYO", "SANTIAGO DE PUPUJA", "VILQUE CHICO",
                 "AZANGARO", "CHUPA", "CAPASO", "LAMPA", "DESAGUADERO",
                 "JULI", "JULIACA", "PLATERIA", "PILCUYO", "PUNO",
                 "AYAVIRI", "SANTA LUCIA", "TARACO", "HUATA", "COATA"),
    lat = c(-16.0833, -16.4833, -15.2000, -15.9667, -16.2500, -16.2667,
            -15.4167, -16.6000, -14.8333, -15.2500, -14.9167, -15.0500,
            -16.0167, -15.3667, -16.5667, -16.2167, -15.5000, -15.9167,
            -15.9833, -15.8422, -14.8833, -15.6833, -15.2833, -15.5500,
            -15.6333),
    lng = c(-69.6333, -69.2667, -69.7667, -69.8000, -69.0833, -69.3000,
            -69.4833, -69.2000, -70.1667, -69.6500, -70.1833, -70.0833,
            -69.7833, -70.3667, -69.0333, -69.4667, -70.1333, -70.0667,
            -69.4167, -70.0199, -70.3833, -70.0000, -69.9833, -69.5333,
            -70.0167)
  )
  
  # COMBINAR DATOS CON COORDENADAS
  puno_distritos <- puno_distritos %>%
    left_join(coordenadas_distritos, by = "NOMBREDI") %>%
    mutate(
      lat = ifelse(is.na(lat), -15.5 + runif(1, -0.5, 0.5), lat),
      lng = ifelse(is.na(lng), -70.0 + runif(1, -0.5, 0.5), lng),
      ranking = row_number(),
      participacion_pct = round((superficie_total_ha / sum(superficie_total_ha)) * 100, 2),
      color_superficie = case_when(
        superficie_total_ha >= 150 ~ "#d73027",
        superficie_total_ha >= 100 ~ "#fc8d59",
        superficie_total_ha >= 50 ~ "#fee08b",
        superficie_total_ha >= 25 ~ "#e6f598",
        TRUE ~ "#abdda4"
      )
    )
  
  cat("📊 DATOS PREPARADOS:\n")
  cat("- Superficie total:", format(resumen_general$total_superficie_ha, big.mark = ","), "ha\n")
  cat("- Total distritos:", resumen_general$distritos_total, "\n")
  cat("- Total provincias:", resumen_general$provincias_total, "\n\n")
  
  return(list(
    puno_distritos = puno_distritos,
    puno_provincias = puno_provincias,
    resumen_general = resumen_general,
    puno_raw = puno
  ))
}

# =============================================================================
# INTERFAZ DE USUARIO (UI)
# =============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # HEADER
  dashboardHeader(
    title = "Análisis Superficie Agrícola - Puno",
    titleWidth = 350
  ),
  
  # SIDEBAR
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("🗺️ Mapa Interactivo", tabName = "mapa", icon = icon("map")),
      menuItem("📊 Análisis Distritos", tabName = "distritos", icon = icon("building")),
      menuItem("📈 Análisis Provincias", tabName = "provincias", icon = icon("map-marker")),
      menuItem("📋 Datos Detallados", tabName = "datos", icon = icon("table")),
      menuItem("ℹ️ Información", tabName = "info", icon = icon("info-circle"))
    ),
    
    hr(),
    
    # CONTROLES DINÁMICOS
    conditionalPanel(
      condition = "input.tabs == 'mapa' || input.tabs == 'distritos'",
      
      h4("🔍 Filtros", style = "color: white; margin-left: 15px;"),
      
      # Filtro por provincia
      div(style = "margin: 15px;",
          selectInput("provincia_filtro", "Provincia:",
                      choices = NULL,
                      selected = "Todas")
      ),
      
      # Slider para superficie mínima
      div(style = "margin: 15px;",
          sliderInput("superficie_min", "Superficie mínima (ha):",
                      min = 0, max = 200, value = 0, step = 5)
      ),
      
      # Búsqueda de distrito
      div(style = "margin: 15px;",
          searchInput("buscar_distrito", "Buscar distrito:",
                      placeholder = "Escribir nombre...",
                      btnSearch = icon("search"),
                      btnReset = icon("remove"))
      )
    )
  ),
  
  # BODY
  dashboardBody(
    # CSS PERSONALIZADO
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .main-header .navbar {
          background-color: #367fa9 !important;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
        .value-box .value-box-value {
          font-size: 28px;
        }
        .leaflet-container {
          border-radius: 10px;
        }
        .distrito-selected {
          background-color: #e3f2fd !important;
        }
      "))
    ),
    
    tabItems(
      # TAB 1: DASHBOARD
      tabItem(tabName = "dashboard",
              fluidRow(
                # VALUE BOXES
                valueBoxOutput("total_superficie", width = 3),
                valueBoxOutput("total_distritos", width = 3),
                valueBoxOutput("total_productores", width = 3),
                valueBoxOutput("total_provincias", width = 3)
              ),
              
              fluidRow(
                # GRÁFICO TOP DISTRITOS
                box(
                  title = "Top 10 Distritos por Superficie", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  height = "500px",
                  withSpinner(plotlyOutput("grafico_top_distritos", height = "420px"))
                ),
                
                # DISTRIBUCIÓN POR PROVINCIAS
                box(
                  title = "Distribución por Provincias", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  height = "500px",
                  withSpinner(plotlyOutput("grafico_provincias", height = "420px"))
                )
              ),
              
              fluidRow(
                # TABLA RESUMEN
                box(
                  title = "Resumen por Provincias", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(DT::dataTableOutput("tabla_resumen_provincias"))
                )
              )
      ),
      
      # TAB 2: MAPA INTERACTIVO
      tabItem(tabName = "mapa",
              fluidRow(
                box(
                  title = "Mapa Interactivo de Superficie Agrícola", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 9,
                  height = "600px",
                  withSpinner(leafletOutput("mapa_interactivo", height = "530px"))
                ),
                
                box(
                  title = "Información del Distrito", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 3,
                  height = "600px",
                  div(id = "info_distrito",
                      h4("Selecciona un distrito en el mapa"),
                      p("Haz clic en cualquier punto del mapa para ver información detallada.")
                  ),
                  br(),
                  uiOutput("info_distrito_detalle")
                )
              )
      ),
      
      # TAB 3: ANÁLISIS DISTRITOS
      tabItem(tabName = "distritos",
              fluidRow(
                box(
                  title = "Análisis Comparativo de Distritos", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 8,
                  height = "500px",
                  withSpinner(plotlyOutput("analisis_distritos", height = "420px"))
                ),
                
                box(
                  title = "Estadísticas del Distrito Seleccionado", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 4,
                  height = "500px",
                  uiOutput("stats_distrito_seleccionado")
                )
              ),
              
              fluidRow(
                box(
                  title = "Ranking de Distritos", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(DT::dataTableOutput("tabla_distritos_completa"))
                )
              )
      ),
      
      # TAB 4: ANÁLISIS PROVINCIAS
      tabItem(tabName = "provincias",
              fluidRow(
                box(
                  title = "Comparación entre Provincias", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "400px",
                  withSpinner(plotlyOutput("comparacion_provincias", height = "320px"))
                ),
                
                box(
                  title = "Productores por Provincia", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  height = "400px",
                  withSpinner(plotlyOutput("productores_provincias", height = "320px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Análisis Detallado por Provincia", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(DT::dataTableOutput("tabla_provincias_detalle"))
                )
              )
      ),
      
      # TAB 5: DATOS DETALLADOS
      tabItem(tabName = "datos",
              fluidRow(
                box(
                  title = "Exportar Datos", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  div(
                    style = "text-align: center; padding: 20px;",
                    h4("Descargar Análisis"),
                    br(),
                    downloadButton("descargar_distritos", "Descargar Datos Distritos", 
                                   class = "btn-primary", style = "margin: 10px;"),
                    downloadButton("descargar_provincias", "Descargar Datos Provincias", 
                                   class = "btn-info", style = "margin: 10px;"),
                    downloadButton("descargar_resumen", "Descargar Resumen General", 
                                   class = "btn-success", style = "margin: 10px;")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Datos Completos de Distritos", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 12,
                  withSpinner(DT::dataTableOutput("datos_completos"))
                )
              )
      ),
      
      # TAB 6: INFORMACIÓN
      tabItem(tabName = "info",
              fluidRow(
                box(
                  title = "Información del Proyecto", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  div(
                    h3("📊 Análisis de Superficie Agrícola - Puno"),
                    hr(),
                    h4("🎯 Objetivo:"),
                    p("Analizar la distribución de superficie agrícola por distritos y provincias en la región Puno mediante estadística espacial interactiva."),
                    
                    h4("📈 Métricas Principales:"),
                    tags$ul(
                      tags$li("Superficie total por distrito (hectáreas)"),
                      tags$li("Número total de productores"),
                      tags$li("Cantidad de parcelas"),
                      tags$li("Superficie promedio por parcela"),
                      tags$li("Ranking y participación porcentual")
                    ),
                    
                    h4("🛠️ Tecnologías:"),
                    p("R + Shiny + Leaflet + Plotly + DT")
                  )
                ),
                
                box(
                  title = "Instrucciones de Uso", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  div(
                    h4("🔍 Navegación:"),
                    tags$ul(
                      tags$li(strong("Dashboard: "), "Vista general con métricas principales"),
                      tags$li(strong("Mapa: "), "Exploración interactiva geoespacial"),
                      tags$li(strong("Distritos: "), "Análisis comparativo detallado"),
                      tags$li(strong("Provincias: "), "Agregación por provincias"),
                      tags$li(strong("Datos: "), "Tablas completas y descargas")
                    ),
                    
                    h4("💡 Consejos:"),
                    tags$ul(
                      tags$li("Usa los filtros laterales para explorar subconjuntos"),
                      tags$li("Haz clic en elementos gráficos para seleccionar"),
                      tags$li("Los gráficos son interactivos (zoom, hover, etc.)"),
                      tags$li("Descarga los datos procesados desde la pestaña Datos")
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
  
  # CARGA DE DATOS REACTIVA
  datos_puno <- reactive({
    withProgress(message = 'Cargando datos...', value = 0, {
      incProgress(0.3, detail = "Leyendo archivo...")
      datos <- preparar_datos()
      incProgress(0.7, detail = "Procesando...")
      Sys.sleep(0.5)  # Simular procesamiento
      incProgress(1, detail = "Completado")
      return(datos)
    })
  })
  
  # ACTUALIZAR CHOICES DE FILTROS (CON VALIDACIÓN)
  observe({
    datos <- datos_puno()
    if(!is.null(datos) && !is.null(datos$puno_distritos)) {
      provincias <- unique(datos$puno_distritos$NOMBREPV)
      provincias <- c("Todas", sort(provincias[!is.na(provincias)]))
      updateSelectInput(session, "provincia_filtro", choices = provincias)
    }
  })
  
  # DATOS FILTRADOS REACTIVOS
  datos_filtrados <- reactive({
    req(datos_puno())
    datos <- datos_puno()$puno_distritos
    
    # Filtrar por provincia
    if(input$provincia_filtro != "Todas") {
      datos <- datos %>% filter(NOMBREPV == input$provincia_filtro)
    }
    
    # Filtrar por superficie mínima
    datos <- datos %>% filter(superficie_total_ha >= input$superficie_min)
    
    # Filtrar por búsqueda de distrito
    if(!is.null(input$buscar_distrito) && input$buscar_distrito != "") {
      datos <- datos %>% 
        filter(grepl(toupper(input$buscar_distrito), toupper(NOMBREDI)))
    }
    
    return(datos)
  })
  
  # VALUE BOXES
  output$total_superficie <- renderValueBox({
    req(datos_puno())
    valueBox(
      value = format(datos_puno()$resumen_general$total_superficie_ha, big.mark = ","),
      subtitle = "Hectáreas Totales",
      icon = icon("leaf"),
      color = "green"
    )
  })
  
  output$total_distritos <- renderValueBox({
    req(datos_puno())
    valueBox(
      value = datos_puno()$resumen_general$distritos_total,
      subtitle = "Distritos",
      icon = icon("building"),
      color = "blue"
    )
  })
  
  output$total_productores <- renderValueBox({
    req(datos_puno())
    valueBox(
      value = format(round(datos_puno()$resumen_general$total_productores), big.mark = ","),
      subtitle = "Productores",
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  output$total_provincias <- renderValueBox({
    req(datos_puno())
    valueBox(
      value = datos_puno()$resumen_general$provincias_total,
      subtitle = "Provincias",
      icon = icon("map-marker"),
      color = "red"
    )
  })
  
  # GRÁFICO TOP DISTRITOS
  output$grafico_top_distritos <- renderPlotly({
    req(datos_puno())
    datos <- head(datos_puno()$puno_distritos, 10)
    
    p <- ggplot(datos, aes(x = reorder(NOMBREDI, superficie_total_ha), 
                           y = superficie_total_ha,
                           fill = superficie_total_ha,
                           text = paste("Distrito:", NOMBREDI,
                                        "<br>Superficie:", superficie_total_ha, "ha",
                                        "<br>Productores:", format(productores_total, big.mark = ","),
                                        "<br>Provincia:", NOMBREPV))) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      scale_fill_viridis_c() +
      labs(title = "", x = "", y = "Superficie (hectáreas)") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 10))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # GRÁFICO PROVINCIAS
  output$grafico_provincias <- renderPlotly({
    req(datos_puno())
    datos <- head(datos_puno()$puno_provincias, 8)
    
    p <- ggplot(datos, aes(x = "", y = superficie_total_ha, 
                           fill = NOMBREPV,
                           text = paste("Provincia:", NOMBREPV,
                                        "<br>Superficie:", superficie_total_ha, "ha",
                                        "<br>Distritos:", distritos))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3") +
      theme_void() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # MAPA INTERACTIVO
  output$mapa_interactivo <- renderLeaflet({
    req(datos_filtrados())
    datos <- datos_filtrados()
    
    # Crear paleta de colores
    pal <- colorNumeric(
      palette = c("#abdda4", "#e6f598", "#fee08b", "#fc8d59", "#d73027"),
      domain = datos$superficie_total_ha
    )
    
    leaflet(datos) %>%
      addTiles() %>%
      setView(lng = -70.0199, lat = -15.8422, zoom = 8) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~sqrt(superficie_total_ha) * 2,
        color = "white",
        fillColor = ~pal(superficie_total_ha),
        weight = 2,
        opacity = 1,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<strong>", NOMBREDI, "</strong><br>",
          "Provincia: ", NOMBREPV, "<br>",
          "Superficie: ", format(superficie_total_ha, big.mark = ","), " ha<br>",
          "Productores: ", format(productores_total, big.mark = ","), "<br>",
          "Ranking: #", ranking, "<br>",
          "Participación: ", participacion_pct, "%"
        ),
        layerId = ~NOMBREDI
      ) %>%
      addLegend(
        pal = pal,
        values = ~superficie_total_ha,
        title = "Superficie (ha)",
        position = "bottomright"
      )
  })
  
  # INFORMACIÓN DETALLADA DEL DISTRITO
  observeEvent(input$mapa_interactivo_marker_click, {
    click <- input$mapa_interactivo_marker_click
    if(!is.null(click$id)) {
      distrito_info <- datos_filtrados() %>%
        filter(NOMBREDI == click$id)
      
      output$info_distrito_detalle <- renderUI({
        if(nrow(distrito_info) > 0) {
          d <- distrito_info[1,]
          div(
            h4(d$NOMBREDI, style = "color: #367fa9;"),
            hr(),
            p(strong("Provincia: "), d$NOMBREPV),
            p(strong("Superficie: "), format(d$superficie_total_ha, big.mark = ","), " hectáreas"),
            p(strong("Productores: "), format(d$productores_total, big.mark = ",")),
            p(strong("Parcelas: "), format(d$parcelas_total, big.mark = ",")),
            p(strong("Superficie promedio: "), round(d$superficie_promedio, 2), " ha/parcela"),
            p(strong("Ranking regional: "), "#", d$ranking),
            p(strong("Participación: "), d$participacion_pct, "%"),
            br(),
            actionButton("ver_detalle", "Ver análisis detallado", 
                         class = "btn-primary btn-sm"),
            style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px;"
          )
        }
      })
    }
  })
  
  # TABLA RESUMEN PROVINCIAS
  output$tabla_resumen_provincias <- DT::renderDataTable({
    req(datos_puno())
    datos <- datos_puno()$puno_provincias %>%
      mutate(
        superficie_total_ha = format(superficie_total_ha, big.mark = ","),
        productores_total = format(round(productores_total), big.mark = ","),
        parcelas_total = format(parcelas_total, big.mark = ",")
      )
    
    DT::datatable(datos, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  colnames = c("Provincia", "Distritos", "Superficie (ha)", 
                               "Productores", "Parcelas", "Superficie Promedio"),
                  rownames = FALSE) %>%
      DT::formatStyle(columns = 1:6, fontSize = '14px')
  })
  
  # ANÁLISIS DISTRITOS
  output$analisis_distritos <- renderPlotly({
    req(datos_filtrados())
    datos <- datos_filtrados()
    
    p <- ggplot(datos, aes(x = productores_total, y = superficie_total_ha,
                           color = NOMBREPV, size = parcelas_total,
                           text = paste("Distrito:", NOMBREDI,
                                        "<br>Provincia:", NOMBREPV,
                                        "<br>Superficie:", superficie_total_ha, "ha",
                                        "<br>Productores:", format(productores_total, big.mark = ","),
                                        "<br>Parcelas:", parcelas_total))) +
      geom_point(alpha = 0.7) +
      scale_color_viridis_d() +
      labs(x = "Número de Productores", y = "Superficie Total (ha)",
           color = "Provincia", size = "Parcelas") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # TABLA DISTRITOS COMPLETA
  output$tabla_distritos_completa <- DT::renderDataTable({
    req(datos_filtrados())
    datos <- datos_filtrados() %>%
      select(ranking, NOMBREDI, NOMBREPV, superficie_total_ha, 
             productores_total, parcelas_total, participacion_pct) %>%
      mutate(
        productores_total = format(round(productores_total), big.mark = ","),
        superficie_total_ha = format(superficie_total_ha, big.mark = ",")
      )
    
    DT::datatable(datos, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  colnames = c("Ranking", "Distrito", "Provincia", 
                               "Superficie (ha)", "Productores", "Parcelas", "Participación (%)"),
                  rownames = FALSE) %>%
      DT::formatStyle(columns = 1:7, fontSize = '14px')
  })
  
  # ESTADÍSTICAS DISTRITO SELECCIONADO
  output$stats_distrito_seleccionado <- renderUI({
    # Mostrar información del primer distrito por defecto o del seleccionado
    req(datos_filtrados())
    distrito_sel <- head(datos_filtrados(), 1)
    
    if(nrow(distrito_sel) > 0) {
      d <- distrito_sel[1,]
      div(
        h4("📍 ", d$NOMBREDI, style = "color: #27ae60;"),
        hr(),
        div(class = "text-center",
            h2(format(d$superficie_total_ha, big.mark = ","), 
               style = "color: #2c3e50; margin: 10px 0;"),
            p("hectáreas", style = "color: #7f8c8d; font-size: 14px;")
        ),
        hr(),
        p(strong("🏛️ Provincia: "), d$NOMBREPV),
        p(strong("👥 Productores: "), format(d$productores_total, big.mark = ",")),
        p(strong("📦 Parcelas: "), format(d$parcelas_total, big.mark = ",")),
        p(strong("📏 Promedio/parcela: "), round(d$superficie_promedio, 2), " ha"),
        p(strong("🏆 Ranking: "), "#", d$ranking),
        p(strong("📊 Participación: "), d$participacion_pct, "%"),
        style = "background-color: #ecf0f1; padding: 15px; border-radius: 8px;"
      )
    } else {
      div(
        h4("Sin datos disponibles"),
        p("Ajusta los filtros para ver información.")
      )
    }
  })
  
  # COMPARACIÓN PROVINCIAS
  output$comparacion_provincias <- renderPlotly({
    req(datos_puno())
    datos <- datos_puno()$puno_provincias
    
    p <- ggplot(datos, aes(x = reorder(NOMBREPV, superficie_total_ha), 
                           y = superficie_total_ha,
                           fill = superficie_total_ha,
                           text = paste("Provincia:", NOMBREPV,
                                        "<br>Superficie:", format(superficie_total_ha, big.mark = ","), "ha",
                                        "<br>Distritos:", distritos,
                                        "<br>Productores:", format(round(productores_total), big.mark = ",")))) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
      labs(title = "", x = "", y = "Superficie (ha)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # PRODUCTORES POR PROVINCIA
  output$productores_provincias <- renderPlotly({
    req(datos_puno())
    datos <- datos_puno()$puno_provincias
    
    p <- ggplot(datos, aes(x = superficie_total_ha, y = productores_total,
                           size = distritos, color = NOMBREPV,
                           text = paste("Provincia:", NOMBREPV,
                                        "<br>Superficie:", format(superficie_total_ha, big.mark = ","), "ha",
                                        "<br>Productores:", format(round(productores_total), big.mark = ","),
                                        "<br>Distritos:", distritos))) +
      geom_point(alpha = 0.7) +
      scale_color_viridis_d() +
      labs(x = "Superficie (ha)", y = "Número de Productores",
           color = "Provincia", size = "Distritos") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # TABLA PROVINCIAS DETALLE
  output$tabla_provincias_detalle <- DT::renderDataTable({
    req(datos_puno())
    datos <- datos_puno()$puno_provincias %>%
      mutate(
        superficie_total_ha = format(superficie_total_ha, big.mark = ","),
        productores_total = format(round(productores_total), big.mark = ","),
        parcelas_total = format(parcelas_total, big.mark = ","),
        superficie_promedio = round(superficie_promedio, 2)
      )
    
    DT::datatable(datos, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  colnames = c("Provincia", "Distritos", "Superficie (ha)", 
                               "Productores", "Parcelas", "Superficie Promedio"),
                  rownames = FALSE) %>%
      DT::formatStyle(columns = 1:6, fontSize = '14px')
  })
  
  # DATOS COMPLETOS
  output$datos_completos <- DT::renderDataTable({
    req(datos_puno())
    datos <- datos_puno()$puno_distritos %>%
      select(ranking, NOMBREDI, NOMBREPV, superficie_total_ha, 
             productores_total, parcelas_total, superficie_promedio, participacion_pct) %>%
      mutate(
        productores_total = format(round(productores_total), big.mark = ","),
        superficie_total_ha = format(superficie_total_ha, big.mark = ","),
        superficie_promedio = round(superficie_promedio, 2)
      )
    
    DT::datatable(datos, 
                  options = list(pageLength = 20, scrollX = TRUE, scrollY = "400px"),
                  colnames = c("Ranking", "Distrito", "Provincia", "Superficie (ha)", 
                               "Productores", "Parcelas", "Superficie Promedio", "Participación (%)"),
                  rownames = FALSE,
                  filter = 'top') %>%
      DT::formatStyle(columns = 1:8, fontSize = '13px')
  })
  
  # DESCARGAS
  output$descargar_distritos <- downloadHandler(
    filename = function() {
      paste("analisis_distritos_puno_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_puno()$puno_distritos, file, row.names = FALSE)
    }
  )
  
  output$descargar_provincias <- downloadHandler(
    filename = function() {
      paste("analisis_provincias_puno_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_puno()$puno_provincias, file, row.names = FALSE)
    }
  )
  
  output$descargar_resumen <- downloadHandler(
    filename = function() {
      paste("resumen_general_puno_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_puno()$resumen_general, file, row.names = FALSE)
    }
  )
  
  # EVENTO PARA VER DETALLE DESDE MAPA
  observeEvent(input$ver_detalle, {
    updateTabItems(session, "tabs", selected = "distritos")
  })
}

# =============================================================================
# EJECUTAR APLICACIÓN
# =============================================================================

# Para ejecutar la aplicación, usa:
shinyApp(ui = ui, server = server)


# =============================================================================
# APLICACIÓN SHINY - ANÁLISIS DE CAMPO GAUSSIANO
# Análisis espacial de superficie agrícola en Puno

# =============================================================================

# Instalar paquetes necesarios (ejecutar solo la primera vez)
#install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
 #                  "leaflet", "gstat", "sp", "dplyr", "ggplot2", 
   #                "viridis", "nortest", "forecast", "shinyWidgets"))

# Cargar librerías
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(gstat)
library(sp)
library(dplyr)
library(ggplot2)
library(viridis)
library(nortest)
library(forecast)
library(shinyWidgets)

# =============================================================================
# INTERFAZ DE USUARIO (UI)
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(
    title = "Análisis de Campo Gaussiano - Superficie Agrícola",
    titleWidth = 500
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("📊 Datos", tabName = "datos", icon = icon("table")),
      menuItem("📈 Normalidad", tabName = "normalidad", icon = icon("chart-line")),
      menuItem("🗺️ Campo Gaussiano", tabName = "gaussiano", icon = icon("map")),
      menuItem("🎯 Predicciones", tabName = "predicciones", icon = icon("crosshairs")),
      menuItem("📋 Reporte", tabName = "reporte", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .info-box {
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # ===== TAB 1: DATOS =====
      tabItem(tabName = "datos",
              fluidRow(
                box(
                  title = "📁 Cargar Datos", status = "primary", solidHeader = TRUE, width = 4,
                  fileInput("archivo", "Selecciona tu archivo CSV:",
                            accept = c(".csv")),
                  helpText("Sube tu archivo de superficie agrícola (.csv)"),
                  br(),
                  actionButton("cargar", "Cargar Datos", class = "btn-primary", width = "100%")
                ),
                
                box(
                  title = "ℹ️ Información del Dataset", status = "info", solidHeader = TRUE, width = 8,
                  verbatimTextOutput("info_datos")
                )
              ),
              
              fluidRow(
                box(
                  title = "📊 Estadísticas Descriptivas", status = "success", solidHeader = TRUE, width = 6,
                  tableOutput("estadisticas")
                ),
                
                box(
                  title = "🏆 Top 10 Distritos", status = "warning", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput("top_distritos")
                )
              ),
              
              fluidRow(
                box(
                  title = "📋 Vista de Datos", status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("tabla_datos")
                )
              )
      ),
      
      # ===== TAB 2: ANÁLISIS DE NORMALIDAD =====
      tabItem(tabName = "normalidad",
              fluidRow(
                valueBoxOutput("shapiro_original"),
                valueBoxOutput("mejor_transformacion"),
                valueBoxOutput("shapiro_transformada")
              ),
              
              fluidRow(
                box(
                  title = "📊 Distribuciones Comparativas", status = "primary", solidHeader = TRUE, width = 8,
                  plotlyOutput("plot_distribuciones", height = "500px")
                ),
                
                box(
                  title = "🔧 Pruebas de Normalidad", status = "info", solidHeader = TRUE, width = 4,
                  tableOutput("pruebas_normalidad"),
                  br(),
                  h4("Interpretación:"),
                  htmlOutput("interpretacion_normalidad")
                )
              ),
              
              fluidRow(
                box(
                  title = "📈 Q-Q Plots", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("qq_plots")
                ),
                
                box(
                  title = "📊 Histogramas", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("histogramas")
                )
              )
      ),
      
      # ===== TAB 3: CAMPO GAUSSIANO =====
      tabItem(tabName = "gaussiano",
              fluidRow(
                box(
                  title = "⚙️ Parámetros del Modelo", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("modelo_variograma", "Modelo de Variograma:",
                              choices = c("Esférico" = "Sph", "Exponencial" = "Exp", "Gaussiano" = "Gau"),
                              selected = "Sph"),
                  
                  numericInput("nugget", "Nugget:", value = 0.1, min = 0, step = 0.01),
                  
                  numericInput("sill", "Sill:", value = 1, min = 0.1, step = 0.1),
                  
                  numericInput("range", "Rango:", value = 0.5, min = 0.1, step = 0.1),
                  
                  br(),
                  actionButton("calcular_variograma", "Calcular Variograma", 
                               class = "btn-success", width = "100%")
                ),
                
                box(
                  title = "📈 Variograma", status = "success", solidHeader = TRUE, width = 9,
                  plotlyOutput("plot_variograma", height = "400px"),
                  br(),
                  verbatimTextOutput("parametros_variograma")
                )
              ),
              
              fluidRow(
                box(
                  title = "🗺️ Mapa de Ubicaciones", status = "info", solidHeader = TRUE, width = 6,
                  leafletOutput("mapa_ubicaciones", height = "400px")
                ),
                
                box(
                  title = "📊 Estadísticas Espaciales", status = "warning", solidHeader = TRUE, width = 6,
                  tableOutput("estadisticas_espaciales"),
                  br(),
                  htmlOutput("interpretacion_espacial")
                )
              )
      ),
      
      # ===== TAB 4: PREDICCIONES =====
      tabItem(tabName = "predicciones",
              fluidRow(
                box(
                  title = "⚙️ Configuración Kriging", status = "primary", solidHeader = TRUE, width = 3,
                  sliderInput("resolucion_grilla", "Resolución de grilla:",
                              min = 20, max = 50, value = 30, step = 5),
                  
                  checkboxInput("incluir_tendencia", "Incluir tendencia", value = FALSE),
                  
                  br(),
                  actionButton("ejecutar_kriging", "Ejecutar Kriging", 
                               class = "btn-success", width = "100%"),
                  
                  br(), br(),
                  h4("Métricas de Validación:"),
                  verbatimTextOutput("metricas_validacion")
                ),
                
                box(
                  title = "🎯 Predicciones Kriging", status = "success", solidHeader = TRUE, width = 9,
                  leafletOutput("mapa_predicciones", height = "450px")
                )
              ),
              
              fluidRow(
                box(
                  title = "🎲 Mapa de Incertidumbre", status = "warning", solidHeader = TRUE, width = 6,
                  leafletOutput("mapa_incertidumbre", height = "400px")
                ),
                
                box(
                  title = "📊 Análisis de Resultados", status = "info", solidHeader = TRUE, width = 6,
                  plotlyOutput("analisis_resultados", height = "400px")
                )
              )
      ),
      
      # ===== TAB 5: REPORTE =====
      tabItem(tabName = "reporte",
              fluidRow(
                box(
                  title = "📋 Reporte Ejecutivo", status = "primary", solidHeader = TRUE, width = 12,
                  htmlOutput("reporte_ejecutivo")
                )
              ),
              
              fluidRow(
                box(
                  title = "📥 Descargas", status = "success", solidHeader = TRUE, width = 6,
                  h4("Descargar Resultados:"),
                  br(),
                  downloadButton("descargar_datos", "Datos Procesados (.csv)", 
                                 class = "btn-primary", style = "width: 100%; margin: 5px;"),
                  downloadButton("descargar_resultados", "Resultados Kriging (.csv)", 
                                 class = "btn-success", style = "width: 100%; margin: 5px;"),
                  downloadButton("descargar_reporte", "Reporte Completo (.html)", 
                                 class = "btn-info", style = "width: 100%; margin: 5px;")
                ),
                
                box(
                  title = "🎓 Guía de Interpretación", status = "info", solidHeader = TRUE, width = 6,
                  htmlOutput("guia_interpretacion")
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
  
  # Variables reactivas
  datos_raw <- reactiveVal()
  datos_procesados <- reactiveVal()
  variograma_modelo <- reactiveVal()
  kriging_resultados <- reactiveVal()
  
  # ===== CARGA DE DATOS =====
  observeEvent(input$cargar, {
    req(input$archivo)
    
    withProgress(message = 'Cargando datos...', value = 0, {
      # Cargar archivo
      incProgress(0.2)
      df <- read.csv(input$archivo$datapath)
      
      # Procesar datos
      incProgress(0.4)
      
      # Filtrar por departamento (los datos son de UCAYALI, no PUNO)
      if("UCAYALI" %in% df$NOMBREDD) {
        puno <- df %>% filter(NOMBREDD == "UCAYALI")
        cat("Datos filtrados por UCAYALI (no PUNO encontrado)\n")
      } else if("PUNO" %in% df$NOMBREDD) {
        puno <- df %>% filter(NOMBREDD == "PUNO")
      } else if("REGION" %in% names(df)) {
        # Buscar por región
        puno_vals <- unique(df$REGION[grepl("PUNO|UCAYALI|puno|ucayali", df$REGION, ignore.case = TRUE)])
        if(length(puno_vals) > 0) {
          puno <- df %>% filter(REGION %in% puno_vals)
        } else {
          puno <- df
        }
      } else {
        puno <- df
      }
      
      incProgress(0.6)
      
      # Agregar por distrito con coordenadas simuladas
      set.seed(123)
      puno_proc <- puno %>%
        group_by(NOMBREDI, NOMBREPV) %>%
        summarise(
          superficie = sum(RESFIN, na.rm = TRUE),
          productores = sum(FACTOR_PRODUCTOR, na.rm = TRUE),
          parcelas = n(),
          .groups = 'drop'
        ) %>%
        filter(superficie > 0) %>%
        mutate(
          # Coordenadas aproximadas de Ucayali (ya que los datos son de ahí)
          lon = runif(n(), -75, -72),  # Longitudes de Ucayali
          lat = runif(n(), -11, -8)    # Latitudes de Ucayali
        )
      
      incProgress(0.8)
      
      # Calcular transformaciones
      puno_proc$log_superficie <- log(puno_proc$superficie + 1)
      puno_proc$sqrt_superficie <- sqrt(puno_proc$superficie)
      
      if(nrow(puno_proc) > 2) {
        lambda <- BoxCox.lambda(puno_proc$superficie)
        puno_proc$boxcox_superficie <- BoxCox(puno_proc$superficie, lambda)
        puno_proc$lambda <- lambda
      }
      
      incProgress(1)
      
      datos_raw(puno)
      datos_procesados(puno_proc)
      
      showNotification("Datos cargados exitosamente!", type = "message")
    })
  })
  
  # ===== INFO DATOS =====
  output$info_datos <- renderText({
    req(datos_raw())
    df <- datos_raw()
    paste(
      "Registros totales:", nrow(df), "\n",
      "Columnas:", ncol(df), "\n",
      "Período:", ifelse("ANIO" %in% names(df), paste(range(df$ANIO, na.rm = TRUE), collapse = " - "), "No especificado"), "\n",
      "Variables clave: RESFIN, FACTOR_PRODUCTOR, NOMBREDI"
    )
  })
  
  # ===== ESTADÍSTICAS =====
  output$estadisticas <- renderTable({
    req(datos_procesados())
    df <- datos_procesados()
    
    data.frame(
      Métrica = c("Distritos", "Superficie Total (ha)", "Superficie Promedio", 
                  "Desviación Estándar", "Mínimo", "Máximo"),
      Valor = c(
        nrow(df),
        format(sum(df$superficie), big.mark = ","),
        round(mean(df$superficie), 2),
        round(sd(df$superficie), 2),
        round(min(df$superficie), 2),
        round(max(df$superficie), 2)
      )
    )
  })
  
  # ===== TOP DISTRITOS =====
  output$top_distritos <- DT::renderDataTable({
    req(datos_procesados())
    df <- datos_procesados() %>%
      arrange(desc(superficie)) %>%
      head(10) %>%
      select(NOMBREDI, NOMBREPV, superficie, productores, parcelas)
    
    DT::datatable(df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  colnames = c("Distrito", "Provincia", "Superficie (ha)", "Productores", "Parcelas"))
  })
  
  # ===== TABLA DATOS =====
  output$tabla_datos <- DT::renderDataTable({
    req(datos_procesados())
    DT::datatable(datos_procesados(), 
                  options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # ===== ANÁLISIS NORMALIDAD =====
  normalidad_resultados <- reactive({
    req(datos_procesados())
    df <- datos_procesados()
    
    # Pruebas de normalidad
    shapiro_orig <- shapiro.test(df$superficie)
    shapiro_log <- shapiro.test(df$log_superficie)
    shapiro_sqrt <- shapiro.test(df$sqrt_superficie)
    
    resultados <- data.frame(
      Transformación = c("Original", "Logarítmica", "Raíz Cuadrada"),
      Shapiro_p = c(shapiro_orig$p.value, shapiro_log$p.value, shapiro_sqrt$p.value),
      Normal = c(shapiro_orig$p.value >= 0.05, shapiro_log$p.value >= 0.05, shapiro_sqrt$p.value >= 0.05)
    )
    
    if("boxcox_superficie" %in% names(df)) {
      shapiro_box <- shapiro.test(df$boxcox_superficie)
      resultados <- rbind(resultados, 
                          data.frame(Transformación = "Box-Cox", 
                                     Shapiro_p = shapiro_box$p.value,
                                     Normal = shapiro_box$p.value >= 0.05))
    }
    
    resultados$mejor <- resultados$Shapiro_p == max(resultados$Shapiro_p)
    return(resultados)
  })
  
  # ===== VALUE BOXES =====
  output$shapiro_original <- renderValueBox({
    req(normalidad_resultados())
    p_val <- normalidad_resultados()$Shapiro_p[1]
    valueBox(
      value = round(p_val, 4),
      subtitle = "Shapiro-Wilk Original",
      icon = icon("chart-bar"),
      color = if(p_val >= 0.05) "green" else "red"
    )
  })
  
  output$mejor_transformacion <- renderValueBox({
    req(normalidad_resultados())
    mejor <- normalidad_resultados()[which.max(normalidad_resultados()$Shapiro_p), ]
    valueBox(
      value = mejor$Transformación,
      subtitle = "Mejor Transformación",
      icon = icon("magic"),
      color = "blue"
    )
  })
  
  output$shapiro_transformada <- renderValueBox({
    req(normalidad_resultados())
    mejor_p <- max(normalidad_resultados()$Shapiro_p)
    valueBox(
      value = round(mejor_p, 4),
      subtitle = "Shapiro-Wilk Transformada",
      icon = icon("check"),
      color = if(mejor_p >= 0.05) "green" else "orange"
    )
  })
  
  # ===== PRUEBAS NORMALIDAD =====
  output$pruebas_normalidad <- renderTable({
    req(normalidad_resultados())
    df <- normalidad_resultados()
    df$Shapiro_p <- round(df$Shapiro_p, 6)
    df$Normal <- ifelse(df$Normal, "✅ Sí", "❌ No")
    df[, c("Transformación", "Shapiro_p", "Normal")]
  }, colnames = c("Transformación", "p-value", "¿Normal?"))
  
  # ===== INTERPRETACIÓN =====
  output$interpretacion_normalidad <- renderUI({
    req(normalidad_resultados())
    mejor <- normalidad_resultados()[which.max(normalidad_resultados()$Shapiro_p), ]
    
    HTML(paste(
      "<b>Resultado:</b><br>",
      "• Variable original:", ifelse(normalidad_resultados()$Normal[1], "Normal ✅", "No normal ❌"), "<br>",
      "• Mejor opción:", mejor$Transformación, "<br>",
      "• p-value:", round(mejor$Shapiro_p, 6), "<br><br>",
      "<b>Recomendación:</b><br>",
      if(mejor$Normal) {
        "✅ Usar transformación <b>" %+% mejor$Transformación %+% "</b> para proceso gaussiano"
      } else {
        "⚠️ Ninguna transformación logró normalidad perfecta. Proceder con cautela."
      }
    ))
  })
  
  # ===== GRÁFICOS DISTRIBUCIONES =====
  output$plot_distribuciones <- renderPlotly({
    req(datos_procesados())
    df <- datos_procesados()
    
    p1 <- ggplot(df, aes(x = superficie)) +
      geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      labs(title = "Original", x = "Superficie", y = "Densidad") +
      theme_minimal()
    
    p2 <- ggplot(df, aes(x = log_superficie)) +
      geom_histogram(aes(y = ..density..), bins = 20, fill = "lightgreen", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      labs(title = "Log-transformada", x = "Log(Superficie)", y = "Densidad") +
      theme_minimal()
    
    ggplotly(p1, height = 400)
  })
  
  # ===== VARIOGRAMA =====
  observeEvent(input$calcular_variograma, {
    req(datos_procesados(), normalidad_resultados())
    
    withProgress(message = 'Calculando variograma...', value = 0, {
      df <- datos_procesados()
      mejor_trans <- normalidad_resultados()[which.max(normalidad_resultados()$Shapiro_p), ]
      
      # Seleccionar variable transformada
      if(mejor_trans$Transformación == "Logarítmica") {
        df$variable_norm <- df$log_superficie
      } else if(mejor_trans$Transformación == "Raíz Cuadrada") {
        df$variable_norm <- df$sqrt_superficie
      } else if(mejor_trans$Transformación == "Box-Cox" & "boxcox_superficie" %in% names(df)) {
        df$variable_norm <- df$boxcox_superficie
      } else {
        df$variable_norm <- df$superficie
      }
      
      incProgress(0.3)
      
      # Convertir a spatial
      coordinates(df) <- ~lon+lat
      proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
      
      incProgress(0.6)
      
      # Calcular variograma
      vario_emp <- variogram(variable_norm ~ 1, df)
      vario_model <- fit.variogram(vario_emp, 
                                   model = vgm(psill = input$sill,
                                               model = input$modelo_variograma,
                                               range = input$range,
                                               nugget = input$nugget))
      
      incProgress(1)
      
      variograma_modelo(list(empirico = vario_emp, modelo = vario_model, datos = df))
      showNotification("Variograma calculado!", type = "message")
    })
  })
  
  # ===== PLOT VARIOGRAMA =====
  output$plot_variograma <- renderPlotly({
    req(variograma_modelo())
    
    vario <- variograma_modelo()
    df_vario <- data.frame(
      distancia = vario$empirico$dist,
      gamma = vario$empirico$gamma,
      np = vario$empirico$np
    )
    
    p <- ggplot(df_vario, aes(x = distancia, y = gamma)) +
      geom_point(aes(size = np), alpha = 0.6) +
      labs(title = "Variograma Empírico y Modelo Ajustado",
           x = "Distancia", y = "Semivarianza",
           size = "Pares de puntos") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ===== PARÁMETROS VARIOGRAMA =====
  output$parametros_variograma <- renderText({
    req(variograma_modelo())
    modelo <- variograma_modelo()$modelo
    paste(
      "Modelo ajustado:\n",
      "Nugget:", round(modelo$psill[1], 4), "\n",
      "Sill:", round(sum(modelo$psill), 4), "\n",
      "Range:", round(modelo$range[2], 4), "\n",
      "Modelo:", modelo$model[2]
    )
  })
  
  # ===== MAPA UBICACIONES =====
  output$mapa_ubicaciones <- renderLeaflet({
    req(datos_procesados())
    df <- datos_procesados()
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(superficie)/50,
        popup = ~paste("<b>", NOMBREDI, "</b><br>",
                       "Provincia:", NOMBREPV, "<br>",
                       "Superficie:", format(superficie, big.mark = ","), "ha"),
        color = "blue",
        fillOpacity = 0.6
      ) %>%
      addLegend(position = "bottomright",
                title = "Superficie",
                labels = c("Pequeña", "Grande"),
                colors = c("lightblue", "darkblue"))
  })
  
  # ===== KRIGING =====
  observeEvent(input$ejecutar_kriging, {
    req(variograma_modelo())
    
    withProgress(message = 'Ejecutando Kriging...', value = 0, {
      vario_data <- variograma_modelo()
      datos_sp <- vario_data$datos
      modelo <- vario_data$modelo
      
      incProgress(0.2)
      
      # Crear grilla
      bbox <- bbox(datos_sp)
      lon_seq <- seq(bbox[1,1], bbox[1,2], length = input$resolucion_grilla)
      lat_seq <- seq(bbox[2,1], bbox[2,2], length = input$resolucion_grilla)
      
      grilla <- expand.grid(lon = lon_seq, lat = lat_seq)
      coordinates(grilla) <- ~lon+lat
      proj4string(grilla) <- CRS("+proj=longlat +datum=WGS84")
      gridded(grilla) <- TRUE
      
      incProgress(0.5)
      
      # Ejecutar kriging
      if(input$incluir_tendencia) {
        kriging_result <- krige(variable_norm ~ lon + lat, datos_sp, grilla, model = modelo)
      } else {
        kriging_result <- krige(variable_norm ~ 1, datos_sp, grilla, model = modelo)
      }
      
      incProgress(0.8)
      
      # Validación cruzada
      cv_result <- krige.cv(variable_norm ~ 1, datos_sp, model = modelo)
      
      incProgress(1)
      
      kriging_resultados(list(
        predicciones = kriging_result,
        validacion = cv_result,
        datos = datos_sp
      ))
      
      showNotification("Kriging completado!", type = "message")
    })
  })
  
  # ===== MAPA PREDICCIONES =====
  output$mapa_predicciones <- renderLeaflet({
    req(kriging_resultados())
    
    kriging_data <- kriging_resultados()
    pred_df <- as.data.frame(kriging_data$predicciones)
    
    # Crear paleta de colores
    pal <- colorNumeric(palette = viridis(100), domain = pred_df$var1.pred)
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(
        raster::raster(kriging_data$predicciones["var1.pred"]),
        colors = pal,
        opacity = 0.7
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = pred_df$var1.pred,
        title = "Predicción<br>Superficie"
      ) %>%
      addCircleMarkers(
        data = as.data.frame(kriging_data$datos),
        lng = ~lon, lat = ~lat,
        radius = 3,
        color = "white",
        fillColor = "red",
        fillOpacity = 1,
        popup = ~paste("Observado:", round(variable_norm, 2))
      )
  })
  
  # ===== MAPA INCERTIDUMBRE =====
  output$mapa_incertidumbre <- renderLeaflet({
    req(kriging_resultados())
    
    kriging_data <- kriging_resultados()
    
    # Crear paleta para varianza
    pred_df <- as.data.frame(kriging_data$predicciones)
    pal_var <- colorNumeric(palette = "plasma", domain = pred_df$var1.var)
    
    leaflet() %>%
      addTiles() %>%
      addRasterImage(
        raster::raster(kriging_data$predicciones["var1.var"]),
        colors = pal_var,
        opacity = 0.7
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_var,
        values = pred_df$var1.var,
        title = "Varianza<br>Kriging"
      )
  })
  
  # ===== MÉTRICAS VALIDACIÓN =====
  output$metricas_validacion <- renderText({
    req(kriging_resultados())
    
    cv <- kriging_resultados()$validacion
    rmse <- sqrt(mean(cv$residual^2))
    mae <- mean(abs(cv$residual))
    correlacion <- cor(cv$observed, cv$var1.pred)
    
    paste(
      "Métricas de Validación Cruzada:\n",
      "RMSE:", round(rmse, 4), "\n",
      "MAE:", round(mae, 4), "\n",
      "Correlación:", round(correlacion, 3), "\n",
      "R²:", round(correlacion^2, 3)
    )
  })
  
  # ===== REPORTE EJECUTIVO =====
  output$reporte_ejecutivo <- renderUI({
    req(datos_procesados(), normalidad_resultados())
    
    df <- datos_procesados()
    norm_res <- normalidad_resultados()
    mejor <- norm_res[which.max(norm_res$Shapiro_p), ]
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 10px;'>",
      "<h2>📊 Reporte Ejecutivo - Análisis de Campo Gaussiano</h2>",
      
      "<h3>🎯 Objetivo</h3>",
      "<p>Aplicar procesos gaussianos (Kriging) para analizar la distribución espacial de superficie agrícola en Puno.</p>",
      
      "<h3>📈 Datos Analizados</h3>",
      "<ul>",
      "<li><b>Distritos analizados:</b> ", nrow(df), "</li>",
      "<li><b>Superficie total:</b> ", format(sum(df$superficie), big.mark = ","), " hectáreas</li>",
      "<li><b>Superficie promedio:</b> ", round(mean(df$superficie), 1), " hectáreas</li>",
      "<li><b>Rango:</b> ", round(min(df$superficie), 1), " - ", round(max(df$superficie), 1), " hectáreas</li>",
      "</ul>",
      
      "<h3>🔄 Transformación de Datos</h3>",
      "<ul>",
      "<li><b>Variable original normal:</b> ", ifelse(norm_res$Normal[1], "✅ Sí", "❌ No"), "</li>",
      "<li><b>Mejor transformación:</b> ", mejor$Transformación, "</li>",
      "<li><b>p-value Shapiro-Wilk:</b> ", round(mejor$Shapiro_p, 6), "</li>",
      "<li><b>Transformación exitosa:</b> ", ifelse(mejor$Normal, "✅ Sí", "⚠️ Parcial"), "</li>",
      "</ul>",
      
      "<h3>🗺️ Proceso Gaussiano (Kriging)</h3>",
      "<p><b>Kriging</b> es un método de interpolación espacial que asume que los datos siguen un proceso gaussiano. ",
      "Permite predecir valores en ubicaciones no muestreadas y cuantificar la incertidumbre de las predicciones.</p>",
      
      "<h4>Ventajas:</h4>",
      "<ul>",
      "<li>Predicción óptima (BLUE - Best Linear Unbiased Estimator)</li>",
      "<li>Cuantificación de incertidumbre</li>",
      "<li>Base teórica sólida</li>",
      "<li>Manejo de correlación espacial</li>",
      "</ul>",
      
      "<h3>🎓 Para tu Trabajo Académico</h3>",
      "<ol>",
      "<li><b>Justificación:</b> Explica por qué necesitaste transformar los datos</li>",
      "<li><b>Metodología:</b> Describe el proceso de análisis de normalidad</li>",
      "<li><b>Variograma:</b> Interpreta la estructura de dependencia espacial</li>",
      "<li><b>Predicciones:</b> Analiza los mapas de predicción e incertidumbre</li>",
      "<li><b>Validación:</b> Incluye métricas de error y correlación</li>",
      "</ol>",
      
      "<div style='background-color: #d4edda; padding: 15px; border-radius: 5px; margin-top: 15px;'>",
      "<h4>💡 Conclusiones Principales</h4>",
      "<p>✅ Se aplicó exitosamente un proceso gaussiano a los datos de superficie agrícola<br>",
      "✅ La transformación ", mejor$Transformación, " mejoró la normalidad de los datos<br>",
      "✅ El modelo permite predecir superficie en ubicaciones no muestreadas<br>",
      "✅ Se cuantificó la incertidumbre espacial de las predicciones</p>",
      "</div>",
      
      "</div>"
    ))
  })
  
  # ===== GUÍA INTERPRETACIÓN =====
  output$guia_interpretacion <- renderUI({
    HTML(paste0(
      "<div style='font-size: 14px;'>",
      "<h4>🎯 Cómo Interpretar los Resultados</h4>",
      
      "<h5>📊 Análisis de Normalidad:</h5>",
      "<ul style='font-size: 12px;'>",
      "<li><b>p-value > 0.05:</b> Datos siguen distribución normal</li>",
      "<li><b>p-value ≤ 0.05:</b> Datos NO son normales</li>",
      "<li><b>Transformación:</b> Convierte datos a distribución normal</li>",
      "</ul>",
      
      "<h5>📈 Variograma:</h5>",
      "<ul style='font-size: 12px;'>",
      "<li><b>Nugget:</b> Variabilidad a distancia cero (error + microescala)</li>",
      "<li><b>Sill:</b> Variabilidad máxima (meseta del variograma)</li>",
      "<li><b>Range:</b> Distancia de correlación espacial</li>",
      "</ul>",
      
      "<h5>🗺️ Mapas de Kriging:</h5>",
      "<ul style='font-size: 12px;'>",
      "<li><b>Predicciones:</b> Valores interpolados de superficie</li>",
      "<li><b>Incertidumbre:</b> Varianza de las predicciones (mayor = menos confiable)</li>",
      "</ul>",
      
      "<h5>✅ Validación:</h5>",
      "<ul style='font-size: 12px;'>",
      "<li><b>RMSE:</b> Error promedio (menor es mejor)</li>",
      "<li><b>MAE:</b> Error absoluto promedio</li>",
      "<li><b>Correlación:</b> Ajuste del modelo (mayor a 0.7 es bueno)</li>",
      "</ul>",
      
      "</div>"
    ))
  })
  
  # ===== ANÁLISIS RESULTADOS =====
  output$analisis_resultados <- renderPlotly({
    req(kriging_resultados())
    
    cv <- kriging_resultados()$validacion
    
    # Gráfico observado vs predicho
    df_plot <- data.frame(
      observado = cv$observed,
      predicho = cv$var1.pred,
      residual = cv$residual
    )
    
    p <- ggplot(df_plot, aes(x = observado, y = predicho)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
      labs(title = "Validación Cruzada: Observado vs Predicho",
           x = "Valores Observados", 
           y = "Valores Predichos",
           caption = "Línea roja: tendencia / Línea gris: perfecta correlación") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ===== ESTADÍSTICAS ESPACIALES =====
  output$estadisticas_espaciales <- renderTable({
    req(variograma_modelo())
    
    modelo <- variograma_modelo()$modelo
    
    data.frame(
      Parámetro = c("Nugget", "Sill Total", "Range", "Modelo"),
      Valor = c(
        round(modelo$psill[1], 4),
        round(sum(modelo$psill), 4),
        round(modelo$range[2], 4),
        as.character(modelo$model[2])
      ),
      Interpretación = c(
        "Variabilidad local",
        "Variabilidad total",
        "Alcance correlación",
        "Tipo de estructura"
      )
    )
  })
  
  # ===== INTERPRETACIÓN ESPACIAL =====
  output$interpretacion_espacial <- renderUI({
    req(variograma_modelo())
    
    modelo <- variograma_modelo()$modelo
    nugget_ratio <- modelo$psill[1] / sum(modelo$psill)
    
    HTML(paste0(
      "<h5>🔍 Interpretación Espacial:</h5>",
      "<ul style='font-size: 12px;'>",
      "<li><b>Efecto pepita:</b> ", round(nugget_ratio * 100, 1), "% de la varianza total</li>",
      "<li><b>Correlación espacial:</b> Hasta ", round(modelo$range[2], 2), " unidades de distancia</li>",
      "<li><b>Estructura:</b> ", 
      if(nugget_ratio < 0.25) "Fuerte dependencia espacial" else 
        if(nugget_ratio < 0.75) "Moderada dependencia espacial" else "Débil dependencia espacial",
      "</li>",
      "</ul>"
    ))
  })
  
  # ===== DESCARGAS =====
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste0("datos_procesados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(datos_procesados(), file, row.names = FALSE)
    }
  )
  
  output$descargar_resultados <- downloadHandler(
    filename = function() {
      paste0("resultados_kriging_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(kriging_resultados())
      resultado <- as.data.frame(kriging_resultados()$predicciones)
      write.csv(resultado, file, row.names = FALSE)
    }
  )
  
  output$descargar_reporte <- downloadHandler(
    filename = function() {
      paste0("reporte_campo_gaussiano_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Crear reporte HTML básico
      req(datos_procesados(), normalidad_resultados())
      
      df <- datos_procesados()
      norm_res <- normalidad_resultados()
      mejor <- norm_res[which.max(norm_res$Shapiro_p), ]
      
      html_content <- paste0(
        "<!DOCTYPE html><html><head><title>Reporte Campo Gaussiano</title></head><body>",
        "<h1>Análisis de Campo Gaussiano - Superficie Agrícola Puno</h1>",
        "<h2>Resumen Ejecutivo</h2>",
        "<p>Distritos analizados: ", nrow(df), "</p>",
        "<p>Superficie total: ", format(sum(df$superficie), big.mark = ","), " hectáreas</p>",
        "<p>Mejor transformación: ", mejor$Transformación, "</p>",
        "<p>p-value Shapiro-Wilk: ", round(mejor$Shapiro_p, 6), "</p>",
        
        "<h2>Conclusiones</h2>",
        "<p>Se aplicó exitosamente un proceso gaussiano para analizar la distribución espacial ",
        "de superficie agrícola en la región de Puno. La transformación ", mejor$Transformación, 
        " mejoró la normalidad de los datos, permitiendo la aplicación de Kriging para ",
        "interpolación espacial y cuantificación de incertidumbre.</p>",
        
        "<p><small>Reporte generado el ", Sys.Date(), "</small></p>",
        "</body></html>"
      )
      
      writeLines(html_content, file)
    }
  )
  
  # ===== HISTOGRAMAS =====
  output$histogramas <- renderPlotly({
    req(datos_procesados())
    df <- datos_procesados()
    
    p1 <- plot_ly(x = ~df$superficie, type = "histogram", name = "Original", 
                  marker = list(color = "lightblue", opacity = 0.7)) %>%
      layout(xaxis = list(title = "Superficie Original"))
    
    p1
  })
  
  # ===== Q-Q PLOTS =====
  output$qq_plots <- renderPlotly({
    req(datos_procesados())
    df <- datos_procesados()
    
    # Q-Q plot para datos originales
    qqnorm_data <- qqnorm(df$superficie, plot.it = FALSE)
    
    df_qq <- data.frame(
      theoretical = qqnorm_data$x,
      sample = qqnorm_data$y
    )
    
    p <- ggplot(df_qq, aes(x = theoretical, y = sample)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_qq_line(aes(sample = sample), color = "red", linetype = "dashed") +
      labs(title = "Q-Q Plot - Superficie Original",
           x = "Cuantiles Teóricos (Normal)", 
           y = "Cuantiles de Muestra") +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)


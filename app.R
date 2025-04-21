# app.R - Aplicación optimizada de Shiny

library(shiny)
library(shinydashboard)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(DT)
library(nortest)
library(shinyjs)

# Funciones auxiliares
interpret_normality <- function(p_value) {
  if(p_value < 0.05) {
    return("Con un nivel de significancia de 0.05, se rechaza la hipótesis nula.\nLos datos NO siguen una distribución normal.")
  } else {
    return("Con un nivel de significancia de 0.05, no se rechaza la hipótesis nula.\nNo hay evidencia suficiente para afirmar que los datos NO siguen una distribución normal.")
  }
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Datos Estadísticos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cargar Datos", tabName = "cargar_datos", icon = icon("upload")),
      menuItem("Análisis Descriptivo", tabName = "analisis_descriptivo", icon = icon("chart-bar")),
      menuItem("Prueba de Normalidad", tabName = "normalidad", icon = icon("chart-line")),
      menuItem("Análisis Cuantitativo", tabName = "cuantitativo", icon = icon("calculator")),
      menuItem("Análisis Cualitativo", tabName = "cualitativo", icon = icon("comment-dots"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Pestaña de carga de datos
      tabItem(tabName = "cargar_datos",
              fluidRow(
                box(
                  title = "Cargar Archivo de Datos", status = "primary", solidHeader = TRUE, width = 12,
                  radioButtons("tipo_entrada", "¿Cómo deseas ingresar los datos?",
                               choices = c("Archivo" = "archivo", "Manual" = "manual"),
                               inline = TRUE),
                  conditionalPanel(
                    condition = "input.tipo_entrada == 'archivo'",
                    fileInput("archivo", "Selecciona un archivo csv o excel",
                              accept = c(".csv", ".xlsx", ".xls")),
                    radioButtons("delim", "Delimitador (para csv):", choices =c(coma=",", punto_y_coma =";", tab="\t"),
                                 selected =",", inline= TRUE),
                    checkboxInput("encabezado", "¿El archivo tiene encabezado?", TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.tipo_entrada == 'manual'",
                    textAreaInput("manual_data", "Introduce los datos en formato CSV:",
                                  placeholder="Ejemplo:\nNombre,Edad,Sexo\nAna,23,F\nLuis,30,M", rows = 10)
                  )
                )
              ),
              fluidRow(
                box(title = "Datos Cargados", status = "info", solidHeader = TRUE, width = 12,
                    DTOutput("tabla_datos"))
              )
      ),
      
      # Pestaña de análisis descriptivo
      tabItem(tabName = "analisis_descriptivo",
              fluidRow(
                box(title = "Seleccionar Variables", status = "primary", solidHeader = TRUE, width = 3,
                    uiOutput("selector_variables_desc")),
                box(title = "Estadísticas Descriptivas", status = "info", solidHeader = TRUE, width = 9,
                    verbatimTextOutput("estadisticas_descriptivas"))
              ),
              fluidRow(
                box(title = "Visualización", status = "success", solidHeader = TRUE, width = 12,
                    plotOutput("grafico_descriptivo"))
              )
      ),
      
      # Pestaña de prueba de normalidad
      tabItem(tabName = "normalidad",
              fluidRow(
                box(title = "Seleccionar Variable", status = "primary", solidHeader = TRUE, width = 3,
                    uiOutput("selector_variables_norm"),
                    radioButtons("prueba_norm", "Prueba de normalidad:",
                                 choices = c("Shapiro-Wilk" = "shapiro", 
                                             "Anderson-Darling" = "ad",
                                             "Kolmogorov-Smirnov" = "ks"),
                                 selected = "shapiro")),
                box(title = "Resultados de la Prueba", status = "info", solidHeader = TRUE, width = 9,
                    verbatimTextOutput("resultados_normalidad"))
              ),
              fluidRow(
                box(title = "Gráficos de Normalidad", status = "success", solidHeader = TRUE, width = 12,
                    tabsetPanel(
                      tabPanel("Histograma", plotOutput("histograma")),
                      tabPanel("Q-Q Plot", plotOutput("qqplot"))
                    ))
              )
      ),
      
      # Pestañas de análisis cuantitativo y cualitativo
      tabItem(tabName = "cuantitativo",
              fluidRow(
                box(title = "Análisis Cuantitativo", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("selector_variables_cuant"),
                    actionButton("realizar_analisis_cuant", "Realizar Análisis", class = "btn-primary"))
              ),
              fluidRow(
                box(title = "Resultados", status = "info", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("resultados_cuantitativos"),
                    plotOutput("grafico_cuantitativo"))
              )
      ),
      
      tabItem(tabName = "cualitativo",
              fluidRow(
                box(title = "Análisis Cualitativo", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("selector_variables_cual"),
                    actionButton("realizar_analisis_cual", "Realizar Análisis", class = "btn-primary"))
              ),
              fluidRow(
                box(title = "Resultados", status = "info", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("resultados_cualitativos"),
                    plotOutput("grafico_cualitativo"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  datos <- reactiveVal(NULL)
  
  # Carga de datos
  observeEvent(input$archivo, {
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$name)
    
    tryCatch({
      df <- NULL
      if(ext == "csv") {
        df <- read.csv(input$archivo$datapath, header = input$encabezado, sep = input$delim)
      } else if(ext %in% c("xls", "xlsx")) {
        df <- readxl::read_excel(input$archivo$datapath, col_names = input$encabezado)
      }
      datos(df)
      showNotification("Datos cargados correctamente", type = "message")
    }, error = function(e) {
      showNotification(paste("Error al cargar el archivo:", e$message), type = "error")
    })
  })
  
  # Observar la entrada manual de datos
  observeEvent(input$manual_data, {
    req(input$manual_data)
    if(input$tipo_entrada == "manual" && nchar(input$manual_data) > 0) {
      tryCatch({
        df <- read.csv(text = input$manual_data, header = TRUE, sep = ",")
        datos(df)
      }, error = function(e) {
        # Silenciar errores durante la edición
      })
    }
  })
  
  # Tabla de datos
  output$tabla_datos <- renderDT({
    req(datos())
    datatable(datos(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Selector de variables numéricas
  output$selector_variables_desc <- renderUI({
    req(datos())
    vars_num <- names(datos())[sapply(datos(), is.numeric)]
    
    selectInput("var_desc", "Variable a analizar:", choices = vars_num,
                selected = if(length(vars_num) > 0) vars_num[1] else NULL)
  })
  
  # Estadísticas descriptivas
  output$estadisticas_descriptivas <- renderPrint({
    req(datos(), input$var_desc)
    var_data <- datos()[[input$var_desc]]
    
    cat("Resumen estadístico para", input$var_desc, ":\n\n")
    print(summary(var_data))
    
    cat("\nDesviación estándar:", sd(var_data, na.rm = TRUE), "\n")
    cat("Varianza:", var(var_data, na.rm = TRUE), "\n")
    cat("Coeficiente de variación:", 
        sd(var_data, na.rm = TRUE) / mean(var_data, na.rm = TRUE) * 100, "%\n")
  })
  
  # Gráfico descriptivo
  output$grafico_descriptivo <- renderPlot({
    req(datos(), input$var_desc)
    var_data <- datos()[[input$var_desc]]
    
    ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(aes(y = ..density..), fill = "steelblue", color = "black", bins = 30) +
      geom_density(color = "red", size = 1) +
      labs(title = paste("Distribución de", input$var_desc),
           x = input$var_desc, y = "Densidad") +
      theme_minimal()
  })
  
  # UI para prueba de normalidad
  output$selector_variables_norm <- renderUI({
    req(datos())
    vars_num <- names(datos())[sapply(datos(), is.numeric)]
    
    selectInput("var_norm", "Variable a analizar:", choices = vars_num,
                selected = if(length(vars_num) > 0) vars_num[1] else NULL)
  })
  
  # Prueba de normalidad
  output$resultados_normalidad <- renderPrint({
    req(datos(), input$var_norm)
    var_data <- na.omit(datos()[[input$var_norm]])
    
    if(length(var_data) < 3) {
      return("Se necesitan al menos 3 observaciones para realizar pruebas de normalidad.")
    }
    
    cat("Prueba de normalidad para", input$var_norm, ":\n\n")
    
    test_result <- switch(input$prueba_norm,
                          "shapiro" = if(length(var_data) > 5000) {
                            return("La prueba Shapiro-Wilk está limitada a 5000 observaciones.\nSe recomienda usar otra prueba para este conjunto de datos.")
                          } else {
                            shapiro.test(var_data)
                          },
                          "ad" = ad.test(var_data),
                          "ks" = ks.test(var_data, "pnorm", mean = mean(var_data), sd = sd(var_data)))
    
    print(test_result)
    cat("\nInterpretación:\n")
    cat(interpret_normality(test_result$p.value))
  })
  
  # Gráficos de normalidad
  output$histograma <- renderPlot({
    req(datos(), input$var_norm)
    var_data <- datos()[[input$var_norm]]
    
    ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(aes(y = ..density..), fill = "steelblue", color = "black", bins = 30) +
      geom_density(color = "red", size = 1) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(var_data, na.rm = TRUE), sd = sd(var_data, na.rm = TRUE)),
                    color = "green", size = 1) +
      labs(title = paste("Histograma con curva normal para", input$var_norm),
           x = input$var_norm, y = "Densidad") +
      theme_minimal()
  })
  
  output$qqplot <- renderPlot({
    req(datos(), input$var_norm)
    var_data <- datos()[[input$var_norm]]
    
    ggplot(data.frame(x = var_data), aes(sample = x)) +
      stat_qq() + stat_qq_line(color = "red") +
      labs(title = paste("Gráfico Q-Q para", input$var_norm),
           x = "Cuantiles teóricos", y = "Cuantiles muestrales") +
      theme_minimal()
  })
  
  # UI para análisis cuantitativo
  output$selector_variables_cuant <- renderUI({
    req(datos())
    vars_num <- names(datos())[sapply(datos(), is.numeric)]
    
    tagList(
      selectInput("var_cuant", "Variable a analizar:", choices = vars_num,
                  selected = if(length(vars_num) > 0) vars_num[1] else NULL),
      selectInput("tipo_analisis_cuant", "Tipo de análisis:",
                  choices = c("Correlación" = "corr", "Regresión Lineal" = "reg", "Series Temporales" = "ts"),
                  selected = "corr"),
      conditionalPanel(
        condition = "input.tipo_analisis_cuant == 'corr' || input.tipo_analisis_cuant == 'reg'",
        selectInput("var_cuant2", "Segunda variable:", choices = vars_num,
                    selected = if(length(vars_num) > 1) vars_num[2] else vars_num[1])
      )
    )
  })
  
  # Resultados cuantitativos
  observeEvent(input$realizar_analisis_cuant, {
    req(datos(), input$var_cuant, input$tipo_analisis_cuant)
    
    output$resultados_cuantitativos <- renderPrint({
      var_data1 <- datos()[[input$var_cuant]]
      
      if(input$tipo_analisis_cuant %in% c("corr", "reg")) {
        req(input$var_cuant2)
        var_data2 <- datos()[[input$var_cuant2]]
        valid_rows <- !is.na(var_data1) & !is.na(var_data2)
        
        if(sum(valid_rows) < 3) {
          return("No hay suficientes datos no-NA para realizar el análisis.")
        }
        
        if(input$tipo_analisis_cuant == "corr") {
          cat("Análisis de correlación entre", input$var_cuant, "y", input$var_cuant2, ":\n\n")
          cor_test <- cor.test(var_data1[valid_rows], var_data2[valid_rows])
          print(cor_test)
          
          cat("\nInterpretación:\n")
          r <- cor_test$estimate
          cat("Coeficiente de correlación r =", r, "\n")
          
          if(abs(r) < 0.3) cat("Correlación débil\n")
          else if(abs(r) < 0.7) cat("Correlación moderada\n")
          else cat("Correlación fuerte\n")
          
          if(cor_test$p.value < 0.05) {
            cat("La correlación es estadísticamente significativa (p < 0.05)\n")
          } else {
            cat("La correlación NO es estadísticamente significativa (p >= 0.05)\n")
          }
        } else if(input$tipo_analisis_cuant == "reg") {
          cat("Análisis de regresión lineal:\n")
          cat("Variable dependiente:", input$var_cuant, "\n")
          cat("Variable independiente:", input$var_cuant2, "\n\n")
          
          df_reg <- na.omit(data.frame(y = var_data1, x = var_data2))
          modelo <- lm(y ~ x, data = df_reg)
          print(summary(modelo))
          
          cat("\nEcuación de regresión:\n")
          cat(input$var_cuant, "=", round(coef(modelo)[1], 4), "+", 
              round(coef(modelo)[2], 4), "*", input$var_cuant2, "\n")
        }
      } else if(input$tipo_analisis_cuant == "ts") {
        cat("Análisis de series temporales para", input$var_cuant, ":\n\n")
        var_data1 <- na.omit(var_data1)
        
        if(length(var_data1) < 10) {
          return("Se necesitan al menos 10 observaciones para el análisis de series temporales.")
        }
        
        ts_data <- ts(var_data1)
        cat("Estadísticas descriptivas de la serie temporal:\n")
        print(summary(ts_data))
        
        if(length(var_data1) >= 24) {
          cat("\nAnálisis de tendencia:\n")
          modelo_tendencia <- lm(ts_data ~ time(ts_data))
          print(summary(modelo_tendencia)$coefficients)
          
          if(summary(modelo_tendencia)$coefficients[2, 4] < 0.05) {
            cat("\nLa serie tiene una tendencia significativa.\n")
          } else {
            cat("\nNo se detecta una tendencia significativa.\n")
          }
        } else {
          cat("\nSe necesitan más observaciones para un análisis completo de descomposición.\n")
        }
      }
    })
    
    output$grafico_cuantitativo <- renderPlot({
      req(datos(), input$var_cuant, input$tipo_analisis_cuant)
      var_data1 <- datos()[[input$var_cuant]]
      
      if(input$tipo_analisis_cuant %in% c("corr", "reg")) {
        req(input$var_cuant2)
        var_data2 <- datos()[[input$var_cuant2]]
        df_plot <- data.frame(x = var_data2, y = var_data1)
        
        p <- ggplot(df_plot, aes(x = x, y = y)) +
          geom_point(color = "blue", alpha = 0.6) +
          labs(title = if(input$tipo_analisis_cuant == "corr") {
            paste("Diagrama de dispersión:", input$var_cuant, "vs", input$var_cuant2)
          } else {
            paste("Regresión lineal:", input$var_cuant, "vs", input$var_cuant2)
          },
          x = input$var_cuant2, y = input$var_cuant) +
          theme_minimal()
        
        if(input$tipo_analisis_cuant == "reg") {
          p <- p + geom_smooth(method = "lm", color = "red", se = TRUE)
        }
        p
      } else if(input$tipo_analisis_cuant == "ts") {
        var_data1 <- na.omit(var_data1)
        df_ts <- data.frame(tiempo = 1:length(var_data1), valor = var_data1)
        
        ggplot(df_ts, aes(x = tiempo, y = valor)) +
          geom_line(color = "blue") +
          geom_point(color = "darkblue", size = 2) +
          geom_smooth(method = "loess", color = "red", se = TRUE) +
          labs(title = paste("Serie temporal para", input$var_cuant),
               x = "Tiempo", y = input$var_cuant) +
          theme_minimal()
      }
    })
  })
  
  # UI para análisis cualitativo
  output$selector_variables_cual <- renderUI({
    req(datos())
    vars_cat <- names(datos())[sapply(datos(), function(x) is.factor(x) || is.character(x))]
    
    if(length(vars_cat) == 0) {
      return(HTML("<div class='alert alert-warning'>No se detectaron variables categóricas en los datos.</div>"))
    }
    
    tagList(
      selectInput("var_cual", "Variable categórica:", choices = vars_cat,
                  selected = if(length(vars_cat) > 0) vars_cat[1] else NULL),
      selectInput("tipo_analisis_cual", "Tipo de análisis:",
                  choices = c("Tabla de frecuencias" = "freq", "Tabla cruzada" = "cross"),
                  selected = "freq"),
      conditionalPanel(
        condition = "input.tipo_analisis_cual == 'cross'",
        selectInput("var_cual2", "Segunda variable categórica:", choices = vars_cat,
                    selected = if(length(vars_cat) > 1) vars_cat[2] else vars_cat[1])
      )
    )
  })
  
  # Resultados cualitativos
  observeEvent(input$realizar_analisis_cual, {
    req(datos(), input$var_cual, input$tipo_analisis_cual)
    
    output$resultados_cualitativos <- renderPrint({
      var_data1 <- datos()[[input$var_cual]]
      
      if(input$tipo_analisis_cual == "freq") {
        cat("Análisis de frecuencias para", input$var_cual, ":\n\n")
        tabla_freq <- table(var_data1, useNA = "ifany")
        prop_tabla <- prop.table(tabla_freq) * 100
        
        resultado <- data.frame(
          Categoría = names(tabla_freq),
          Frecuencia = as.vector(tabla_freq),
          Porcentaje = as.vector(prop_tabla)
        )
        print(resultado)
      } else if(input$tipo_analisis_cual == "cross") {
        req(input$var_cual2)
        var_data2 <- datos()[[input$var_cual2]]
        
        cat("Tabla cruzada entre", input$var_cual, "y", input$var_cual2, ":\n\n")
        tabla_cruzada <- table(var_data1, var_data2, useNA = "ifany")
        print(tabla_cruzada)
        
        cat("\nPrueba Chi-cuadrado de independencia:\n")
        resultado_chi <- chisq.test(tabla_cruzada, simulate.p.value = TRUE)
        print(resultado_chi)
        
        cat("\nInterpretación:\n")
        if(resultado_chi$p.value < 0.05) {
          cat("Con un nivel de significancia de 0.05, se rechaza la hipótesis nula.\n")
          cat("Existe una asociación significativa entre las variables.\n")
        } else {
          cat("Con un nivel de significancia de 0.05, no se rechaza la hipótesis nula.\n")
          cat("No hay evidencia suficiente para afirmar que existe asociación entre las variables.\n")
        }
      }
    })
    
    output$grafico_cualitativo <- renderPlot({
      req(datos(), input$var_cual, input$tipo_analisis_cual)
      var_data1 <- datos()[[input$var_cual]]
      
      if(input$tipo_analisis_cual == "freq") {
        df_plot <- as.data.frame(table(var_data1, useNA = "ifany"))
        names(df_plot) <- c("Categoria", "Frecuencia")
        
        ggplot(df_plot, aes(x = Categoria, y = Frecuencia, fill = Categoria)) +
          geom_bar(stat = "identity") +
          labs(title = paste("Frecuencias para", input$var_cual),
               x = input$var_cual, y = "Frecuencia") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if(input$tipo_analisis_cual == "cross") {
        req(input$var_cual2)
        var_data2 <- datos()[[input$var_cual2]]
        df_plot <- data.frame(Var1 = var_data1, Var2 = var_data2)
        
        ggplot(df_plot) +
          geom_count(aes(x = Var1, y = Var2, color = Var2)) +
          labs(title = paste("Relación entre", input$var_cual, "y", input$var_cual2),
               x = input$var_cual, y = input$var_cual2) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
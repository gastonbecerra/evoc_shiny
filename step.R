library(tidyverse)
library(shiny)

proto <- function(data, 
                  column_id, column_palabra, column_orden, column_valor,
                  freq_min, cut_stat) {
  
  data2 <- data 
  # %>% filter(!is.na({{column_id}}),
  #                          !is.na({{column_orden}}),
  #                          !is.na({{column_orden}}),
  #                          )
  # 
  # if ( !is.numeric(data2[column_orden,]) ) {
  #   return(glimpse(data2[column_orden,]))
  # }
  
  evocaciones <- data2 %>% 
    group_by(!!sym(column_palabra)) %>%
    summarize(
      freq = n(),
      orden = mean(!!sym(column_orden), na.rm = TRUE),
      valor = mean(!!sym(column_valor), na.rm = TRUE)
    ) %>%
    filter(freq >= freq_min)
  
  
  info <- list(
    palabras_unicas = nrow(evocaciones),
    palabras_freq_max = max(evocaciones$freq),
    palabras_freq_min = min(evocaciones$freq),
    palabras_freq_sd = sd(evocaciones$freq),
    freq_min = freq_min
  )
    
  options <- list(
    column_id = column_id,
    column_palabra = column_palabra,
    column_orden = column_orden,
    column_valor = column_valor,
    update=now()
  )
  
  return(
    list(
      evocaciones=evocaciones,
      info=info,
      options=options
    )
  )
}

cuadrantes <- function(evoc) {
  data <- evoc$evocaciones
  
  # 2do: calcular los valores de corte. aca o en proto?
  # 2do: separar los valores y meterlos en una tabla de 4 cuadrantes
  
  # prototipico <- data %>%
  #   mutate( segmento = case_when(
  #   freq >= evoc$info$ & orden_media < evoc$ ~ 1,
  #   freq >= freq_cut & orden_media >= orden_cut ~ 2,
  #   freq < freq_cut & orden_media < orden_cut ~ 3,
  #   freq < freq_cut & orden_media >= orden_cut ~ 4
  # )
  # ) %>% arrange(segmento, desc(freq),desc(orden_media))

    return(data)
}

grafico <- function(evoc) {
  data <- evoc$evocaciones
  
  x <- data %>% ggplot(aes(x=freq,y=orden_media)) +
      geom_point(aes(size=freq), show.legend = TRUE) +
      theme_dark()
  
  # ggplot(data, 
  #        aes(x=freq, y=orden_media, label=input$variable3)) + 
  #   scale_x_continuous(trans='log') + 
  #   #geom_hline(yintercept = orden_cut, linetype = 2) + 
  #   #geom_vline(xintercept = freq_cut, linetype = 2) + 
  #   geom_point(aes(size=freq, colour=valoracion_media), show.legend = TRUE) +
  #   scale_colour_gradient(low = "red", high = "green", na.value = NA) + 
  #   geom_text(aes(size=20, colour=valoracion_media), fontface = "bold",
  #             show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) +
  #   labs(y="Orden de evocación", x = "Frecuencia (log)") + 
  #   theme_minimal() 
  
  return(x)
}


ui <- fluidPage(
  titlePanel("Evocaciones"),
  
  sidebarLayout(
    sidebarPanel(
      shiny::h3("Ingresar datos"),
      fileInput("file", "Seleccionar CSV", accept = c(".csv")),
      verbatimTextOutput("glimpse_csv"),
      shiny::h3("Seleccione las columnas"),
      selectInput("column_id", "Columna ID", ""),
      selectInput("column_palabra", "Columna Palabras", ""),
      selectInput("column_orden", "Columna Orden", ""),
      selectInput("column_valor", "Columna Valoración / Importancia", ""),
    ),
    
    mainPanel(
      shiny::h3("Parametros y preprocesamientos"),
      
      fluidRow(
        column(4,
               numericInput("param_umbral", "Umbral", value = 1),
               selectInput("param_stat", "Stat", choices = c("media","mediana")),
               selectInput("param_clean", "Pre-procesamiento", choices = c("No aplicar", "Stemming", "Lemma", "Custom"))
        ),
        column(8,
               style = "max-height: 200px; overflow-y: auto;",
               verbatimTextOutput("options")
        )
      ),
      
      shiny::h3("Tablas y gráficos"),
      tableOutput("cuadrantes"), # aca vamos a mostrar la tipica tabla con cuadrantes
      plotOutput("grafico")
      
      #tableOutput("selected_column_values2"),
      #verbatimTextOutput("result_summary")
    ),
  )
)

server <- function(input, output, session) {
  
  evoc_data <- reactiveVal( # objeto para guardar los datos
    list(data=NULL)
  )
  
  evoc_proto <- reactiveVal( # objeto para guardar datos y parametros
    list()
  )
  
  observeEvent( # actualizamos cuando hay un nuevo file
    input$file, { 
    req(input$file)
    data <- read.csv(input$file$datapath)
    col_names <- colnames(data)
    updateSelectInput(session, "column_id", choices = col_names, selected = col_names[1]) 
    updateSelectInput(session, "column_palabra", choices = col_names, selected = col_names[2]) # 2do: en realidad deberian ser columnas de solo texto
    updateSelectInput(session, "column_orden", choices = col_names, selected = col_names[3]) # 2do: en realidad deberian ser columnas de solo numero (porque promediamos)
    updateSelectInput(session, "column_valor", choices = col_names, selected = col_names[4]) # 2do: en realidad deberian ser columnas de solo numero (porque promediamos)
    evoc_data(list(data=data))
  })

  observeEvent( # actualizamos cuando cambian las selecciones de variables 
    c(input$column_id, input$column_palabra, input$column_orden, input$column_valor), {
      print("Debug: cambio en el input de datos")
      req(evoc_data()$data)
      evoc_proto(
        proto(data = evoc_data()$data, 
              column_id = input$column_id, 
              column_palabra = input$column_palabra, 
              column_orden = input$column_orden, 
              column_valor = input$column_valor,
              freq_min = input$param_umbral, 
              cut_stat = input$param_stat)
              )
  })
  
  observeEvent( # actualizamos cuando cambian los parametros
    c(input$param_umbral, input$param_stat, input$param_clean), {
      print("Debug: cambio en los parametros")
      req(evoc_data()$data)
      evoc_proto(
        proto(data = evoc_data()$data,
              column_id = input$column_id,
              column_palabra = input$column_palabra,
              column_orden = input$column_orden,
              column_valor = input$column_valor,
              freq_min = input$param_umbral,
              cut_stat = input$param_stat)
      )
    })
  

  # outputs -----------------

  
  output$glimpse_csv <- renderPrint({ # mostramos el CSV abajo del boton de browse
    req(evoc_data()$data)
    glimpse(evoc_data()$data)
  })
  
  observe({
    
    output$options <- renderPrint({ evoc_proto() })
    
    output$cuadrantes <- renderTable({ 
      cuadrantes( evoc_proto() )
    })

  })
  
}

shinyApp(ui, server)

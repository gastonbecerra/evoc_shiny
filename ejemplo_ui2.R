library(tidyverse)
library(shiny)


analisis_proto <- function (tabla_evocaciones,
                            variable_palabra,
                            variable_valoracion,
                            variable_orden,
                            frecuencia_minima = 2,  
                            criterio_corte = "media") {
  
  # (paso 1) tabla de frecuencias
  asoc_frecuentes <- tabla_evocaciones %>% 
    group_by({{variable_palabra}}) %>% 
    summarize( 
      freq = n(), 
      valoracion_media = mean({{variable_valoracion}}), 
      orden_media = mean({{variable_orden}}) 
    )  
  
  # (paso 2) establecemos un umbral de frecuencia minima (usando parametro)
  asoc_frecuentes <- asoc_frecuentes %>% 
    filter(freq > frecuencia_minima) 
  message("frecuencia minima = ", frecuencia_minima)
  
  # (paso 3) calculamos el corte de frecuencia (usamos mean o median, segun parametro)
  if (criterio_corte == "media") {
    freq_cut <- mean(asoc_frecuentes$freq) 
    orden_cut <- mean(asoc_frecuentes$orden_media) 
  } else {
    freq_cut <- median(asoc_frecuentes$freq) 
    orden_cut <- median(asoc_frecuentes$orden_media) 
  }
  message("valor corte frecuencia = ", freq_cut)
  message("valor corte orden = ", orden_cut)
  
  # (paso 4) segmentamos las palabras
  ## La tilde o virgulilla ~ significa THEN
  prototipico <- asoc_frecuentes %>% mutate( segmento = case_when(
    freq >= freq_cut & orden_media < orden_cut ~ 1,
    freq >= freq_cut & orden_media >= orden_cut ~ 2,
    freq < freq_cut & orden_media < orden_cut ~ 3,
    freq < freq_cut & orden_media >= orden_cut ~ 4 
  )
  ) %>% arrange(segmento, desc(freq),desc(orden_media))
  
  # (paso 5) vamos a contar las palabras en cada segmento y lo mostramos en pantalla
  palabras_por_segmento <- prototipico %>% 
    count(segmento) %>%
    pull(n) # pull extrae una variable, quedando como vector
  message("palabras en cada segmento = ", 
          paste(palabras_por_segmento, collapse = " | "))
  
  # ... y lo devolvemos
  return(list(prototipico = prototipico,
              freq_cut = freq_cut,
              orden_cut = orden_cut))
}



ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Cargá tu archivo CSV"),
      selectInput("variable1", "Variable 1", choices = NULL),
      selectInput("variable2", "Variable 2", choices = NULL),
      selectInput("variable3", "Variable 3", choices = NULL),
      selectInput("freq_min", "frecuencia minima", choices = c(1:10)),
      selectInput("criterio_corte", "criterio de corte", choices = c("media","mediana"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla de datos", dataTableOutput("table_data")),
        tabPanel("Análisis prototípico", plotOutput("prototipico"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observe({
    if (!is.null(data())) {
      updateSelectInput(session, "variable1", choices = names(data()))
      updateSelectInput(session, "variable2", choices = names(data()))
      updateSelectInput(session, "variable3", choices = names(data()))
    }
  })
  
  selected_data <- reactive({
    data_to_plot <- data()
    variable1 <- input$variable1
    variable2 <- input$variable2
    variable3 <- input$variable3
    
    data_to_plot %>%
      select({{variable1}}, {{variable2}}, {{variable3}})
  })
  
  output$table_data <- renderDataTable({
    selected_data()
  })
  
  output$prototipico <- renderPlot({
    resultado_funcion <- analisis_proto(selected_data(), 
                                        input$variable1, input$variable2, input$variable3,
                                        frecuencia_minima = input$freq_min, 
                                        criterio_corte = input$criterio_corte)
    
    freq_cut <- resultado_funcion$freq_cut
    orden_cut <- resultado_funcion$orden_cut
    
    ggplot(resultado_funcion$prototipico, 
           aes(x=freq, y=orden_media, label=input$variable3)) + 
      scale_x_continuous(trans='log') + 
      #geom_hline(yintercept = orden_cut, linetype = 2) + 
      #geom_vline(xintercept = freq_cut, linetype = 2) + 
      geom_point(aes(size=freq, colour=valoracion_media), show.legend = TRUE) +
      scale_colour_gradient(low = "red", high = "green", na.value = NA) + 
      geom_text(aes(size=20, colour=valoracion_media), fontface = "bold",
                show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) +
      labs(y="Orden de evocación", x = "Frecuencia (log)") + 
      theme_minimal() 
  })
}

shinyApp(ui, server)


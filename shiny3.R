library(tidyverse)
library(shiny)




# este es el tipo de objeto que habria que armar... pero esto no anda en la shiny por la manera en que se arma el grafico
analisis_proto <- function (tabla_evocaciones, 
                            frecuencia_minima = 2,  
                            criterio_corte = "media") {
  
  # (paso 1) tabla de frecuencias
  asoc_frecuentes <- tabla_evocaciones %>% 
    group_by(palabra) %>% 
    summarize( 
      freq = n(), 
      valoracion_media = mean(valoracion), 
      orden_media = mean(orden) 
    )  
  
  # (paso 2) establecemos un umbral de frecuencia minima (usando parametro)
  asoc_frecuentes <- asoc_frecuentes %>% 
    filter(freq > frecuencia_minima) 
  
  # (paso 3) calculamos el corte de frecuencia (usamos mean o median, segun parametro)
  if (criterio_corte == "media") {
    freq_cut <- mean(asoc_frecuentes$freq) 
    orden_cut <- mean(asoc_frecuentes$orden_media) 
  } else {
    freq_cut <- median(asoc_frecuentes$freq) 
    orden_cut <- median(asoc_frecuentes$orden_media) 
  }
  
  # (paso 4) segmentamos las palabras
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
    pull(n) 
  
  objeto <- list()
  objeto$datos = prototipico
  objeto$freq_cut = freq_cut
  objeto$orden_cut = orden_cut
  objeto$segmentos = palabras_por_segmento
  
  return(objeto)
  
}


x <- analisis_proto(tabla_evocaciones = asociaciones, 
                    frecuencia_minima = 3, criterio_corte = "media")


ui <- fluidPage(
  
  fluidRow(
    shiny::h1("Análisis prototipico de evocaciones")
  ),
  
  fluidRow(
    shiny::h2("Input datos"),
    fileInput("file", "Cargá tu archivo CSV"),
    selectInput("variable1", "Variable 1", choices = NULL),
    selectInput("variable2", "Variable 2", choices = NULL),
    selectInput("variable3", "Variable 3", choices = NULL),
    selectInput("freq_min", "frecuencia minima", choices = c(1:10)),
    selectInput("criterio_corte", "criterio de corte", choices = c("media","mediana"))
    
  ),
  
  fluidRow(
    selectInput("freq_min", "frecuencia minima", choices = c(1:10)),
    selectInput("criterio_corte", "criterio de corte", choices = c("media","mediana"))
  ),
  
  fluidRow(
    column(8, plotOutput("grafico"))
    #column(8, plotOutput("keywords"))
  ),
  
  fluidRow(
    tableOutput("parametros")
  )
  
)

server <- function(input, output) {
  
  asociaciones <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  asociaciones <- readr::read_csv(file = "https://raw.githubusercontent.com/gastonbecerra/curso-intro-r/main/data/asociaciones.csv")    

  evoc <- reactive( analisis_proto(
    tabla_evocaciones = asociaciones, 
    frecuencia_minima = input$freq_min, 
    criterio_corte = input$criterio_corte) )
  
  output$grafico <- renderPlot(
    evoc()$datos %>%
      ggplot(aes(x=freq,y=orden_media,label=palabra)) + 
      scale_x_continuous(trans='log') + 
      geom_hline(yintercept = evoc()$orden_cut, linetype = 2) + # estos valores hay que des-hardcodear
      geom_vline(xintercept = evoc()$freq_cut, linetype = 2) +  # estos valores hay que des-hardcodear
      geom_point(aes(size=freq, colour=valoracion_media), show.legend = TRUE) +
      scale_colour_gradient(low = "red", high = "green", na.value = NA) + 
      geom_text( aes(size=20, colour=valoracion_media), fontface = "bold",
                 show.legend = FALSE, nudge_y = -.1, check_overlap = TRUE) +
      labs(y="Orden de evocación", x = "Frecuencia (log)") + 
      theme_minimal() 
  )
  
  output$parametros <- renderTable(
    evoc()$datos
  )
  
}

shinyApp(ui, server)

# 2do: hay que poder pasar por parametro el df
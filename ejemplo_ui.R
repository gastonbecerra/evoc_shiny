# Instalar paquetes necesarios si aún no están instalados
# install.packages(c("shiny", "shinydashboard"))

# Cargar bibliotecas necesarias
library(shiny)
library(shinydashboard)

# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "CSV Analysis App"),
  dashboardSidebar(
    fileInput("file", "Choose CSV File", accept = c(".csv")),
    actionButton("submit", "Submit")
  ),
  dashboardBody(
    tabItems(
      # Primer Tab - Subida de archivo
      tabItem(
        tabName = "upload",
        h2("Upload CSV File"),
        fluidRow(
          box(
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            fileInput("file", "Choose CSV File", accept = c(".csv")),
            tags$br(),
            actionButton("submit", "Submit")
          )
        )
      ),
      
      # Segundo Tab - Análisis
      tabItem(
        tabName = "analysis",
        h2("Data Analysis"),
        fluidRow(
          box(
            width = 12,
            solidHeader = TRUE,
            tableOutput("table")
          )
        )
      )
    )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  # Datos reactivos para almacenar el archivo cargado
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Observar cuando se hace clic en el botón "Submit"
  observeEvent(input$submit, {
    updateTabItems(session, "tabs", selected = "analysis")
  })
  
  # Renderizar la tabla en la segunda pestaña
  output$table <- renderTable({
    data()
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)

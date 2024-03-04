library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel(h1("Generá tu tabla de análisis prototípico", align = "center")),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Cargá tu archivo CSV"),
      selectInput("variable1", "Variable 1", choices = NULL),
      selectInput("variable2", "Variable 2", choices = NULL),
      selectInput("variable3", "Variable 3", choices = NULL)
    ),
    mainPanel(
      dataTableOutput("table_data")
    )
  )
)

# Define server logic
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
  
  output$table_data <- renderDataTable({
    data_to_plot <- data()
    variable1 <- input$variable1
    variable2 <- input$variable2
    variable3 <- input$variable3
    
    selected_data <- data_to_plot %>%
      select({{variable1}}, {{variable2}}, {{variable3}})
    
    
    return(selected_data)
  })
}

shinyApp(ui = ui, server = server)

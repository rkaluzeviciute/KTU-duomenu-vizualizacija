library(shiny)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imones_kodas", label = "Imones vardas", choices = NULL, selected = NULL)
    ),
    mainPanel(tabsetPanel(
      tabPanel("grafikas", plotOutput("plot")),
      tabPanel("lentele", tableOutput("table"))
    )
    )
  )
)
server <- function(input, output, session) {
  data <- read_csv("https://github.com/kestutisd/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
  updateSelectizeInput(session, "imones_kodas", choices = data$name, server = TRUE)
  
  output$table <- renderTable(
    data %>%
      filter(name == input$imones_kodas) , digits = 0
  )
  
  output$plot <- renderPlot(
    data %>%
      filter(name == input$imones_kodas) %>%
      ggplot(aes(x = month, y = numInsured)) +
      geom_line()      
  )
}
shinyApp(ui, server)

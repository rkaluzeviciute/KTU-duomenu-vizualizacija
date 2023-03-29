library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Hello world"),
  dashboardSidebar(
    selectizeInput(inputId = "imones_kodas", label = "Imones vardas", choices = NULL, selected = NULL)
  ),
  dashboardBody(
    fluidRow(box("grafikas", plotOutput("plot"))),
    fluidRow(box("grafikas", plotlyOutput("plotly")))
    
    # fluidRow(box("lentele", tableOutput("table")))
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
  
  output$plotly <- renderPlotly({
    p2 <- data %>%
      group_by(code, name) %>%
      summarise(avg_wage = mean(avgWage), avg_insured = median(numInsured), total_tax = sum(tax)) %>%
      na.omit() %>%
      arrange(desc(avg_wage)) %>%
      head(20) %>%
      ggplot(aes(x = avg_wage, y = total_tax, size = avg_insured, color = name)) +
      geom_point() +
      # geom_smooth(aes(x = avg_wage, y = total_tax), method="glm", se=T) +
      theme(legend.position = "none")
    plot(p2)
    ggplotly(p2)
  })
}

shinyApp(ui, server)
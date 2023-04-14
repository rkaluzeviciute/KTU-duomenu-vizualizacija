library(shiny)
library(tidyverse)
library(shinydashboard)

ui <- dashboardPage(skin="green",
                    dashboardHeader(title="Sodra"),
                    dashboardSidebar(
                      selectizeInput(inputId = "imones_kodas",
                                     label="Įmonės kodas",
                                     choices = NULL)),
                    dashboardBody(fluidRow(box("Grafikas",
                                               plotOutput("plot"))))
                    )
server <- function(input, output, session) {
  data1 <- read_csv("https://github.com/kestutisd/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
  data=
    data1%>%
    filter(grepl("68100", ecoActCode))%>%
    group_by(name)%>%
    filter(mean(avgWage, na.rm=TRUE)>0)
  updateSelectizeInput(session, "imones_kodas", choices = data$code, server = TRUE)
  
  
  output$plot <- renderPlot(
    data %>%
      filter(code == input$imones_kodas) %>%
      ggplot(aes(x = month, y = avgWage)) +
      geom_line()      
  )
}
shinyApp(ui, server)

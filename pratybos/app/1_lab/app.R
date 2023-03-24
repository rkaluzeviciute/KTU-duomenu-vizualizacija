library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Lab 2"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          numericInput("code", value = 976923, label = "Imones kodas:")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # plotOutput("distPlot")
          tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- read_csv("../../../laboratorinis/data/lab_sodra.csv")
  
  output$table <- renderTable({
    data %>%
      filter(code == input$code) %>%
      head(20)
  })

    output$distPlot <- renderPlot({
        # # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

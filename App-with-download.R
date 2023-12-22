# Install and load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("DT")) install.packages("DT")
if (!require("openxlsx")) install.packages("openxlsx")

library(shiny)
library(DT)
library(openxlsx)

# Define the UI
ui <- fluidPage(
  titlePanel("Frequency Table and Excel Download"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select a variable"),
      selectInput("variable", "Choose a variable", choices = names(mtcars), selected = "mpg"),
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      DTOutput("frequencyTable")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Create a reactive expression for the frequency table
  frequencyTable <- reactive({
    table_data <- table(mtcars[[input$variable]])
    data.frame(Value = as.character(names(table_data)),
               Frequency = as.numeric(table_data),
               stringsAsFactors = FALSE)
  })
  
  # Render the frequency table using DT
  output$frequencyTable <- renderDT({
    datatable(frequencyTable(), options = list(pageLength = 10))
  })
  
  # Define the download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("frequency_table_", input$variable, ".xlsx", sep = "")
    },
    content = function(file) {
      # Write the data to an Excel file
      write.xlsx(frequencyTable(), file, rowNames = FALSE)
    }
  )
}

# Run the application
shinyApp(ui, server)
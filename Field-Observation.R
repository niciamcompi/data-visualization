library(tidyverse)
library(shiny)
library(shinydashboard)
library(rpivotTable)

library(shinyjs)
library(haven)
library(dplyr)
library(xml2)

library(haven)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(faux)
library(scales)

#Import
load("~/faux.Rdata")

sum <- data.frame(table(faux$Date, faux$Institut))
sum$Var1 <- as.Date(sum$Var1, format = "%Y-%m-%d")
sum$Var2 <- as.character(sum$Var2)

sum <- dplyr::rename(sum, "Date"="Var1")
sum <- dplyr::rename(sum, "Institute"="Var2")
sum <- dplyr::rename(sum, "Count"="Freq")


#App
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    shinyjs::useShinyjs(),
    sidebarMenu(id = "inTabset",
      menuItem("Pivottable",
                         tabName = "Pivottable"),
             
      menuItem("Lineplot", tabName = "Lineplot"))),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #0c3b63;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color:#0c3b63;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #0c3b63;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #0c3b63;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #0c3b63;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #0c3b63;
                                color: #5F6A6A;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #FDFEFE;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #FDFEFE;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FDFEFE;
                                }
                                
                                '))),
    tabItems(
    tabItem("Pivottable", fluidRow(width = 800, height = 600, rpivotTableOutput("plot1"))),
    tabItem("Lineplot", fluidRow(width = 2200, height = 800, plotlyOutput("myPlot"))))))



server <- function(input, output, session) { 
  my_rpivotTable <- function (data, rows = NULL, cols = NULL, aggregatorName = NULL, 
                              vals = NULL, rendererName = NULL, sorter = NULL, exclusions = NULL, 
                              inclusions = NULL, locale = "en", subtotals = FALSE, ..., 
                              width = 800, height = 600, elementId = NULL) 
  {
    if (length(intersect(class(data), c("data.frame", "data.table", 
                                        "table", "structable", "ftable"))) == 0) {
      stop("data should be a data.frame, data.table, or table", 
           call. = F)
    }
    if (length(intersect(c("table", "structable", "ftable"), 
                         class(data))) > 0) 
      data <- as.data.frame(data)
    params <- list(rows = rows, cols = cols, aggregatorName = aggregatorName, 
                   vals = vals, rendererName = rendererName, sorter = sorter, 
                   ...)
    params <- Map(function(p) {
      # added to the class check -------------------------------------------------
      if (length(p) == 1 && class(p[[1]]) != "JS_EVAL") {
        p = list(p)
      }
      return(p)
    }, params)
    par <- list(exclusions = exclusions, inclusions = inclusions)
    params <- c(params, par)
    params <- Filter(Negate(is.null), params)
    x <- list(data = data, params = params, locale = locale, 
              subtotals = subtotals)
    htmlwidgets::createWidget(name = "rpivotTable", x, width = width, 
                              height = height, elementId = elementId, package = "rpivotTable")
  }
  
  pivot <- my_rpivotTable(faux,
                          aggregatorName = c("Count"), 
                          aggregators = list(Count = htmlwidgets::JS('$.pivotUtilities.aggregators["Count"]'),
                                             "Percent per Columns" = htmlwidgets::JS('$.pivotUtilities.aggregators["Count as Fraction of Columns"]'),
                                             "Percent per Rows" = htmlwidgets::JS('$.pivotUtilities.aggregators["Count as Fraction of Rows"]')),
                          renderers = list(Table = htmlwidgets::JS('$.pivotUtilities.renderers["Table"]'),
                                           "Bar Chart" = htmlwidgets::JS('$.pivotUtilities.c3_renderers["Bar Chart"]'),
                                           "Stacked Bar Chart" = htmlwidgets::JS('$.pivotUtilities.c3_renderers["Stacked Bar Chart"]')),
                          rendererOptions = list(
                            c3 = list(legend = list(show = FALSE), 
                                      data = list(labels = FALSE),
                                      options = list(responsive = FALSE,
                                                     maintainAspectRatio = FALSE),
                                      size = list(width = "600",
                                                  height = "500")),
                            d3 = list(size = list(width = "500", height = "100%")) 
                          ))

  
  
  
  gg <- ggplot(sum, aes(x = Date, y = Count, color = Institute)) +
    geom_line() +
    labs(title = "Count per Day by Institute", x = "Date", y = "Count") +
    scale_x_date(labels = date_format("%Y-%m-%d")) +
    scale_colour_manual(values=c("blue", "red")) +
    theme(text = element_text(size = 9),element_line(size =1), panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey"))
  line <- ggplotly(gg, width = 1200, height = 600)
  
  
observeEvent(input$inTabset, {
      updateTabItems(session, "dasboard", "other")
     if(input$inTabset == "Pivottable") {
    output$plot1 <- rpivotTable::renderRpivotTable(pivot) }
      if(input$inTabset == "Lineplot")  {
        output$myPlot <- renderPlotly(line) }
  })
}
    
    
shinyApp(ui, server)


                    

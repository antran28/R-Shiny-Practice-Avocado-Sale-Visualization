#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dbplyr)
library(tidyverse)

avocado <- read.csv("avocado2.csv")

avocado <- avocado %>%
  select(-X) %>%
  pivot_longer(cols = starts_with("X4"), names_to = "avo_size", values_to = "avo_volumn")

print(avocado)

plot_epicurve <- function(data, type = "All", avocadosize = "X400") {
  
  if (!("All" %in% type)) {
    data <- data %>%
      filter(Type %in% type)
    
    plot_title_type <- stringr::str_glue("{paste0(type, collapse = ', ')} types")
    
  } else {
    
    plot_title_type <- "all types"
    
  }
  
  # if no remaining data, return NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  data <- data %>%
    filter(avo_size == avocadosize)
  
  
  # if no remaining data, return NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  if (avocadosize == "X400") {
    avocadosize_title <- "All sizes"
  } else {
    avocadosize_title <- stringr::str_glue("{str_remove(avocadosize, 'X')}")
  }
  
  
  ggplot(data, aes(x = Date, y = avo_volumn)) +
    geom_col(width = 0.6, fill = "darkolivegreen4") + 
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
    theme_minimal() +
    labs(
      x = "Date",
      y = "Number of avos sales",
      title = stringr::str_glue("Hass avocado - {plot_title_type}"),
      subtitle = avocadosize_title
    )
  
  
  
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Avocado Sales Visualisation"),
  
  sidebarLayout(
    
    sidebarPanel(
      # selector for avocado type
      selectInput(
        inputId = "select_avocadotype",
        label = "Select Avocado Type",
        choices = c(
          "All",
          "Coventional"="conventional",
          "Organic"="organic"
        ),
        selected = "All",
        multiple = TRUE
      ),
      # selector for avocado size group
      selectInput(
        inputId = "select_avocadosize",
        label = "Select Avocado Size",
        choices = c(
          "All" = "X400",
          "Small/ Medium" = "X4046",
          "Large" = "X4225",
          "Extra Large" = "X4770"
        ), 
        selected = "All",
        multiple = FALSE
      ),
      # horizontal line
      hr(),
      downloadButton(
        outputId = "download_epicurve",
        label = "Download plot"
      )
      
    ),
    
    mainPanel(
      # epicurve goes here
      plotOutput("avocado_epicurve"),
      br(),
      hr(),
      p("Hey avo lovers! Welcome to the avocado sales visualisation app! To use this app, manipulate the widgets on the side to change the curve according to your preferences! To download a high quality image of the plot you've created, you can also download it with the download button. To see the raw data, use the raw data tab for an interactive form of the table. The data dictionary is as follows:"),
      tags$ul(
        tags$li(tags$b("region"), " - the city or region that the data were collected at"),
        tags$li(tags$b("date"), " - the date the data were collected at"),
        tags$li(tags$b("X4046"), " - small/ medium avocado (3-5oz)"),
        tags$li(tags$b("X4225"), " - large avocado (8-10oz)"),
        tags$li(tags$b("X4770"), " - extra large avocado (10-15oz)"),
        tags$li(tags$b("X400"), " - total volume of avocado sale"),
        tags$li(tags$b("type"), " - organic or conventional avocado")
      )
      
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$avocado_epicurve <- renderPlot(
    plot_epicurve(avocado, type = input$select_avocadotype, avocadosize = input$select_avocadosize)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

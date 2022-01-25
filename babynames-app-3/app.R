library(shiny)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(stringr)
library(babynames)

# Load data ---------------------------------------------------------
baby_data <- as_tibble(babynames)

# Data cleaning
# baby_data <- baby_data %>%
#   rename(year = Year, sex = Sex, name = Name, n = Count, prop = Proportion)
colnames(baby_data) <- c("Year", "Sex", "Name", "Count", "Proportion")

# Determine years in data -------------------------------------------
years <- unique(baby_data$Year)


# UI ----------------------------------------------------------------
ui <- fluidPage(
  # App title -------------------------------------------------------
  titlePanel("US Baby Names trend from 1880 to 2017"), 
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs --------------------------------------------------------
    sidebarPanel(
      
      sliderInput("year",
                  label = "Year",
                  min = min(years),
                  max = max(years),
                  step = 1,
                  sep = "",
                  value = range(years)),
      
      # selectInput("name",
      #             label = "Select a name",
      #             names
                  
      # selectInput("name", "Choose a name:",
      #               sort(unique(baby_data$Name)),
      #                     selected = "Anna", multiple = FALSE),
      
      textInput("name", "Type a name", "Anna"),
      
      
      # checkboxInput("opt1", "Male", FALSE),
      # checkboxInput("opt2", "Female", FALSE),
    ),
    
    # Output --------------------------------------------------------
    mainPanel(
      plotOutput("plot", height = "500px")
    )
  )   
)


server <- function(input, output, session) {
  selected_years <- reactive(baby_data %>%
                           filter(Year >= input$year[1], Year <= input$year[2], Name == input$name))

  output$plot <- renderPlot({
    
    ggplot(selected_years(), aes(Year, Count, colour = Sex)) +
      geom_line() +
      labs(y = "Number of selected name occurences")
  }, res = 96)
}

shinyApp(ui, server)
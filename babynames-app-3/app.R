library(shiny)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(stringr)
library(babynames)

# Load data ---------------------------------------------------------
baby_data <- as_tibble(babynames)

# Data cleaning
baby_data <- baby_data %>%
  rename(Year = year, Sex = sex, Name = name, Count = n, Proportion = prop)
#colnames(baby_data) <- c("Year", "Sex", "Name", "Count", "Proportion")

# Determine years in data -------------------------------------------
years <- unique(baby_data$Year)


# UI ----------------------------------------------------------------
ui <- fluidPage(
  # App title -------------------------------------------------------
  titlePanel("USA Baby Names trend from 1880 to 2017"), 
  
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
      
      selectizeInput('name', 'Choose a name', choices = NULL, multiple = TRUE),
      
      br(),
      actionButton("go", "Go!")
      
    ),
    
    # Output --------------------------------------------------------
    mainPanel(
      plotOutput("plot", height = "500px")
    )
  )   
)


server <- function(input, output, session) {
  updateSelectizeInput(session, 'name', choices = sort(unique(baby_data$Name)), selected = 'Anna', server = TRUE)
  
  # Reactive and Event Reactive
  selected_years <- reactive(baby_data %>%
                           filter(Year >= input$year[1], Year <= input$year[2], Name == input$name))

 
  
  draw_plot <- eventReactive(input$go, {
    ggplot(selected_years(), aes(Year, weight = Count, fill = paste(Name, Sex))) +
      geom_histogram(binwidth = 1) +
      labs(y = "Number of selected name occurences") + xlim(input$year[1], input$year[2]) 
  })
  
  # Reactive and Observe
  observe({
    req(input$go)
    if(nrow(selected_years()) >= 30) {
      message("Hooray! You found a popular name")
    }
    else {
      message("Oops! This name is not quite popular")
    }
  })
  
  # Observe Event
  # observeEvent(input$go, {
  #   showModal(modalDialog(
  #     title = "Hooray! You found a popular name",
  #     "This is an important message!",
  #     easyClose = TRUE
  #   ))
  # })
  # 
  
  output$plot <- renderPlot({
   draw_plot()
  })
}

shinyApp(ui, server)
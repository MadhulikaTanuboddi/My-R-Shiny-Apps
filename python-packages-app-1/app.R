library(shiny)
#library(here)
library(tidyverse)
library(DT)

# Data import and clean up
python_packages <- read_csv("600_Python_Packages.csv")
python_packages <- python_packages %>% rename(license_type = license, distribution = metadata_source)


# TODO
# Get the unique license types

# Update license types to be generic

# Version numbers clean up

ui <- fluidPage(
  # App title ----
  titlePanel("Python Package List"),
  br(),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      sliderInput("slider_version", label = "Package Version", min = 0, max = 9,
                  value = c(0, 2)),
      
      checkboxGroupInput("check_dist", "Distribution:",
                         choices = c("Python Package Index" = "PyPI",
                                     "Anaconda" = "Anaconda"),
                         selected = c("PyPI", "Anaconda")
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: DT table ----
      DTOutput("table"),
      
      # Output: Histogram ----
      plotOutput(outputId = "histPlot")
    )
    
  )
  
)


# Define server logic ---
server <- function(input, output, session) {
  
  python_version <- reactive({
    python_packages %>% filter(version >= input$slider_version[1], version <= input$slider_version[2]) 
  })
  
  python_dist <- reactive({
    req(input$check_dist)
    python_packages %>% filter(distribution %in% input$check_dist) %>% filter(version>=input$slider_version[1] & version<=input$slider_version[2])
    
  })
  
  output$table <- renderDT(
    python_dist() %>% select(package_name, version, summary, distribution)
  )
  
  output$histPlot <- renderPlot({
    plot (as.factor(python_dist()$distribution))
  })
  
  
}

shinyApp(ui, server)
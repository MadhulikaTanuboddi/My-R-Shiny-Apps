#
# This is a Shiny web application. You can run the application by clicking

library(shiny)
library(maps)
library(mapproj)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(shinyBS)
library(bslib)
library(thematic)

# Load data ----
cafires <- read_csv("cafires.csv")

# Call thematic_shiny before app launch to set the theming defaults for all the plots in the app
thematic_shiny(font = "auto")

# UI for application
ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "minty"),
    
    # Application title
    titlePanel("California Wildfires: Historical Data"),

    # Sidebar with a slider input for number of fires per year
    sidebarLayout(
        sidebarPanel(
            sliderInput("slider_year",
                        "Select Year Range:",
                        min = 2013,
                        max = 2019,
                        sep = "",
                        value = c(2013,2019)
            ),
            
            actionButton(inputId ="reset", label = 'Reset Slider'),
            bsTooltip("reset", "Make sure to click Generate Plot to see the updated plots",
                      "right", options = list(container = "body")),
            
            selectInput("County", "Choose a county:",
                        sort(unique(cafires$Counties)),
                        selected = "Napa",
                        multiple = FALSE
            ),
          
           actionButton("submit", label = "Generate Plot"),
           
           # Include clarifying text ----
           helpText("Note: while the County level and Interactive map tabs will show only the specified",
                    "number of observations, the Summary tab will still be based",
                    "on the full dataset."),
        ),



        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(type = "pills",
                       tabPanel("County Level Data",
                                h3("Acrage Burned Over Years"),
                                plotOutput("acrePlot"),
                                h3("Number of Fires Per Year"),
                                plotOutput("numberPlot")
                                ),
                       tabPanel("Interactive Map",
                                h3("Interactive Map of Fire Activity"),
                                leafletOutput("camap")
                                ),
                       tabPanel("Summary",
                                h3("Key Summary Statistics"),
                                DT::dataTableOutput("summaryTable"),
                                h3("List of All Fires"),
                                DT::dataTableOutput('contents')
                       )
           )
        )
    )
)

# Define server logic 
server <- function(input, output, session) {
  
    sumTable <- 
     cafires %>%
      select(ArchiveYear, AcresBurned, CrewsInvolved, Engines, Fatalities, Injuries, StructuresDamaged, StructuresDestroyed) %>%
      group_by(ArchiveYear) %>% summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
      # TODO: Consolidate the mutate in to sumTable. Use column names instead of column number. length, count, summarise
      mutate((cafires %>% group_by(ArchiveYear) %>% summarise(NumOfFires = n()))[2]) %>% relocate(NumOfFires, .after = ArchiveYear)
    
    output$summaryTable <- DT::renderDataTable({
      DT::datatable(sumTable, options = list(orderClasses = TRUE))
    })

    # Display table data from select and relevant columns
    display_data <- 
      cafires %>% select(Name, AcresBurned, ArchiveYear, CalFireIncident,
                          Counties, CrewsInvolved, Dozers, Engines,
                          Fatalities, Helicopters, Injuries, Longitude,
                          Latitude, MajorIncident, StructuresDamaged,
                          StructuresDestroyed)
  
   
    # Output contents table
    output$contents <- DT::renderDataTable({
      display_data
    })

    ## COUNTY TAB
    # Filter data from slider input range - using eventReactive
    fire_years <- eventReactive(input$submit,{
      cafires %>% 
        filter(ArchiveYear >= input$slider_year[1], ArchiveYear <= input$slider_year[2] & Counties == input$County) 
    })
    

    # Sum up acres burned per year
    data_AcresBurned <- reactive({
      aggregate(fire_years()$AcresBurned, by=list(fire_years()$ArchiveYear), FUN=sum, na.rm=TRUE)
    })
    

    # Output acres burned area chart
    output$acrePlot <- renderPlot({
      ggplot(data_AcresBurned(),
             aes(x=data_AcresBurned()$Group.1,
                 y=data_AcresBurned()$x))+
        xlab("Year")+
        ylab("Acres Burned")+
        geom_area(fill='#142F86',alpha=2)
    })
    
    # Output number of fires plot
    output$numberPlot <- renderPlot({
      plot (as.factor(fire_years()$ArchiveYear), ylab='Number of Fires')
    })

    # Output number of fires plot - Using observe event
    observeEvent(input$reset, {
      updateSliderInput(session, "slider_year", min = 2013, max = 2019, value = c(2013,2019))
    })
    

    ## INTERACTIVE MAP TAB
    points <- reactive({
      cafires %>%
        filter(ArchiveYear >= input$slider_year[1] & ArchiveYear <= input$slider_year[2]) %>%
        filter(Counties == input$County) %>%
        select(Longitude, Latitude)
    })

    output$camap <- renderLeaflet({
      fire_dt <- fire_years()
      leaflet() %>%
        setView(lat = 36.778259, lng = -119.417931, zoom = 5) %>%
        addTiles() %>%
        addCircleMarkers(data = points(),
                         radius = sqrt(fire_years()$AcresBurned/pi)/10,
                         fillOpacity = 3/4, stroke = T, color = 'tomato',
                         popup =paste("<p> <b>", fire_years()$Name, "</b> </br>",
                                      "Year:", fire_years()$ArchiveYear, "</br>",
                                      "Acres Burned:", fire_years()$AcresBurned, "</br>",
                                      "Structures Damaged:", fire_years()$StructuresDamaged, "</br>",
                                      "Fatalities:", fire_years()$Fatalities, "</br>",
                                      "</p>")
                          )

    })

}

# Run the application
shinyApp(ui = ui, server = server)

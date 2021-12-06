#
# This is a Shiny web application. You can run the application by clicking

library(shiny)
library(maps)
library(mapproj)
library(tidyverse)
library(ggplot2)
library(leaflet)

# Load data ----
cafires <- read_csv("cafires.csv")

# UI for application
ui <- fluidPage(

    # Application title
    titlePanel("California Wildfires: Historical Data"),

    # Sidebar with a slider input for number of fires per year
    sidebarLayout(
        sidebarPanel(
          br(),
            sliderInput("slider_year",
                        "Select Year Range:",
                        min = 2013,
                        max = 2019,
                        sep = "",
                        value = c(2013,2019)
            ),
            br(),
            br(),
            selectInput("County", "Choose A County:",
                        sort(unique(cafires$Counties)),
                        selected = "Napa",
                        multiple = FALSE
            )
        ),
        
        

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("State Level Data",
                                h3("Key Summary Statistics"),
                                DT::dataTableOutput("summaryTable"),
                                h3("Acrage Burned Over Years"),
                                plotOutput("statePlot"),
                                h3("List of All Fires"),
                                DT::dataTableOutput('contents')
                                ),
                       tabPanel("County Level Data", 
                                h3("Acrage Burned Over Years"),
                                plotOutput("acrePlot"),
                                h3("Number of Fires Per Year"),
                                plotOutput("numberPlot")
                                ),
                       tabPanel("Interactive Map",
                                h3("Interactive Map of Fire Activity"),
                                leafletOutput("camap")
                                )
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## SUMMARY TAB
    # Generate summary data by year
    a_year <- unique(cafires$ArchiveYear)
    a_noFires <- as.vector(table(cafires$ArchiveYear))
    a_acres <- aggregate(cafires$AcresBurned, by=list(cafires$ArchiveYear),FUN=sum, na.rm=TRUE)$x
    #a_calfireincident <- 
    #a_counties
    a_crews <- aggregate(cafires$CrewsInvolved, by=list(cafires$ArchiveYear),FUN=sum, na.rm=TRUE)$x
    a_engines <- aggregate(cafires$Engines, by=list(cafires$ArchiveYear),FUN=sum, na.rm=TRUE)$x
    a_fatalities <- aggregate(cafires$Fatalities, by=list(cafires$ArchiveYear),FUN=sum, na.rm=TRUE)$x
    a_injuries <- aggregate(cafires$Injuries,by=list(cafires$ArchiveYear),FUN=sum, na.rm=TRUE)$x
    a_struct_damaged <- aggregate(cafires$StructuresDamaged,by=list(cafires$ArchiveYear),FUN=sum, na.rm=TRUE)$x
    a_struct_destroyed <- aggregate(cafires$StructuresDestroyed,by=list(cafires$ArchiveYear),FUN=sum, na.rm=TRUE)$x
    
    # Create a table with summary data
    a_sumTable <- tibble(
      "Year" = a_year,
      "Number Of Fires" = a_noFires,
      "Acres Burned" = a_acres,
      "Crews Involved" = a_crews,
      "Engines Used"= a_engines,
      "Fatalities" = a_fatalities,
      "Injuries" = a_injuries,
      "Structures Damaged" = a_struct_damaged,
      "Structures Destroyed" = a_struct_destroyed
    )
    
    #output summary table
    output$summaryTable <- DT::renderDataTable({
      DT::datatable(a_sumTable)   
    })
    
    #output area plot of acres burned
    a_data <- tibble(a_year,a_acres)
    output$statePlot <- renderPlot({
      ggplot(a_data, aes(x=a_year, y=a_acres)) +
       geom_point(alpha = 0.5) +
       xlab("Year") +
       ylab("Acres Burned") +
       geom_smooth(method=lm, colour = "red", fill = "mediumpurple1")
    })
    
    # Display table data from select and relavent columns
    display_data <- reactive({
      cafires %>%  select(AcresBurned, ArchiveYear, CalFireIncident, 
                          Counties, CrewsInvolved, Dozers, Engines, 
                          Fatalities, Helicopters, Injuries, Longitude, 
                          Latitude, MajorIncident, Name, StructuresDamaged, 
                          StructuresDestroyed)
    })
    
    # Output contents table
    output$contents <- DT::renderDataTable({
      DT::datatable(display_data())       
    })
    
    
    ## COUNTY TAB
    # Filter data from slider input range
    fire_years <- reactive({
      cafires %>% 
        filter(ArchiveYear >= input$slider_year[1], ArchiveYear <= input$slider_year[2], Counties == input$County)
    })
    
    # Sum up acres burned per year
    data_AcresBurned <- reactive({
      aggregate(fire_years()$AcresBurned,by=list(fire_years()$ArchiveYear),FUN=sum, na.rm=TRUE)
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
    
    ## INTRACTIVE MAP TAB
    points <- reactive({
      cafires %>% 
        filter(ArchiveYear >= input$slider_year[1] & ArchiveYear <= input$slider_year[2]) %>% 
        filter(Counties == input$County) %>% 
        select(Longitude, Latitude)
    })
    
    output$camap <- renderLeaflet({
      #TODO
      fire_dt <- fire_years()
      leaflet() %>%
        setView(lat = 36.778259, lng = -119.417931, zoom = 5) %>%
        addTiles() %>% 
        addCircleMarkers(data = points(), 
                         radius = fire_years()$AcresBurned/5000,
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

library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(ggplot2)

branch <- "41_extending_baseline_map"

points_in_perimeter <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/points_in_perimeter.geojson"), stringsAsFactors = FALSE)

points_in_perimeter@data$occrrnS <- as.factor(points_in_perimeter@data$occrrnS)

perimeter_shape <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE)

bbox <- as.data.frame(perimeter_shape@bbox)

overview_RBU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/observations_RBU.csv"))
overview_RBSU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/observations_RBSU.csv"))
occupancy_RBU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBU.csv"))
occupancy_RBSU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBSU.csv"))


ui <- navbarPage(
  title = "Riparias D1",
  header= fluidRow(
    box(width = 12, 
        img(src='Riparias_Logo.png', align = "right", height = 90)
    )
  ),
  tabPanel("Maps",
    sidebarLayout(
      sidebarPanel(
        sliderInput("slider", 
                    "Years", 
                    2000, 
                    lubridate::year(Sys.Date()), 
                    1,
                    value = c(2010, 2020),
                    dragRange = TRUE),
        checkboxGroupInput("species",
                           "Species",
                           choices = unique(points_in_perimeter$vernacular_name_en)
        )
        ),
      mainPanel(
        fluidRow(
          box(
            title = "map", width = 12,
            uiOutput("text1"),
            leafletOutput("map", height = 600)
          )
        )
      )
  )
  
)
, tabPanel('Distribution',
           sidebarLayout(
             sidebarPanel(
               selectInput("RBUi", "Select a river basin:",
                           choices = unique(overview_RBU$RBU))
             ),
           mainPanel(
             fluidRow(
               tabsetPanel(type = "tabs",
                           tabPanel("Observations", plotOutput("graphRBU")),
                           tabPanel("Occupancy", plotOutput("OccRBU"))
                           )
           )
           )),
           sidebarLayout(
             sidebarPanel(
               numericInput("RBSUi", " Select an A0_CODE:", 10)
             ),
             mainPanel(
               fluidRow(
                 tabsetPanel(type = "tabs",
                             tabPanel("Observations", plotOutput("graphRBSU")),
                             tabPanel("Occupancy", plotOutput("OccRBSU"))
                 )
               )
             )))
           
           ,
tabPanel('Surveillance'),
tabPanel('Trends')
)

  


server <- function(input, output) { 
  
  output$text1 <- renderUI({
    text <- "Select at least one species to display observations"
    if(length(input$species) == 1){
      text <- HTML(paste0(em(input$species), " observations between ", 
                          strong(input$slider[1]), " & ", 
                          strong(input$slider[2])))
    }
    if(length(input$species) == 2){
      text <- HTML(paste0(em(paste(input$species, collapse = " & ")), 
                          " observations between ", 
                          strong(input$slider[1]), " & ", 
                          strong(input$slider[2])))
    }
    if(length(input$species) > 2){
      last_species <- input$species[length(input$species)]
      species <- subset(input$species, !input$species %in% last_species)
      text <- HTML(paste0(em(paste(species, collapse = ", ")), " & ", 
                          em(last_species) , " observations between ", 
                          strong(input$slider[1]), " & ", 
                          strong(input$slider[2])))
    }
    print(text)
    
  })
  
  datObs<-reactive({
      test <- overview_RBU[(overview_RBU$RBU == input$RBUi),]
      test
  })
  
  output$graphRBU <-renderPlot ({
    ggplot(datObs(), aes(x=scientific_name, y=n, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_brewer(palette="Paired")+
      coord_flip()+ 
      labs(y = "Number of observations")+ 
      labs(x = "Species")
    
  })

  datOcc <-reactive({
    test1 <- occupancy_RBU[(occupancy_RBU$RBU == input$RBUi),]
    test1
  })
  
  output$OccRBU <-renderPlot ({
    ggplot(datOcc(), aes(x=scientific_name, y=Occupancy, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_brewer(palette="Paired")+
      coord_flip()+ 
      labs(y = "Occupancy")+ 
      labs(x = "Species")
    
  })
  
  datObs2<-reactive({
    test2 <- overview_RBSU[(overview_RBSU$A0_CODE == input$RBSUi),]
    test2
  })
  
  output$graphRBSU <-renderPlot ({
    ggplot(datObs2(), aes(x=scientific_name, y=n, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_brewer(palette="Paired")+
      coord_flip()+ 
      labs(y = "Number of observations")+ 
      labs(x = "Species")
    
  })
  
  datOcc2<-reactive({
    test3 <- occupancy_RBSU[(occupancy_RBSU$A0_CODE == input$RBSUi),]
    test3
  })
  
  output$OccRBSU <-renderPlot ({
    ggplot(datOcc2(), aes(x=scientific_name, y=Occupancy, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_brewer(palette="Paired")+
      coord_flip()+ 
      labs(y = "Occupancy")+ 
      labs(x = "Species")
    
  })
  
  output$map <- renderLeaflet({
    
    jaren <- seq(from = min(input$slider), 
                 to = max(input$slider),
                 by = 1)
    
    points_in_perimeter_sub <- subset(points_in_perimeter, 
                                      points_in_perimeter$year %in% jaren)
    
    points_in_perimeter_sub <- subset(points_in_perimeter_sub,
                                      points_in_perimeter_sub$vernacular_name_en %in%
                                        input$species)
    
    pal <- colorFactor(palette = c("#1b9e77", "#d95f02", "#636363"),
                       levels = c("ABSENT", "PRESENT", NA))
    
    leaflet(points_in_perimeter_sub) %>% 
      addTiles() %>% 
      addPolylines(data = perimeter_shape) %>% 
      addCircleMarkers(data = points_in_perimeter_sub,
                       popup = points_in_perimeter_sub$popup,
                       radius = 1,
                       color = ~pal(points_in_perimeter_sub@data$occrrnS),
                       fillColor = ~pal(points_in_perimeter_sub@data$occrrnS)) %>% 
      addLegend(data = points_in_perimeter_sub,
                title = "occurrence Status",
                values = ~unique(points_in_perimeter@data$occrrnS),
                pal = pal) %>% 
      setMaxBounds(lng1 = bbox$min[1], 
                   lat1 = bbox$min[2], 
                   lng2 = bbox$max[1], 
                   lat2 = bbox$max[2])
  })
}

shinyApp(ui, server)

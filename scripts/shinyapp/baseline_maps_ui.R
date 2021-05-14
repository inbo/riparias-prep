library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)

points_in_perimeter <- readOGR("https://github.com/inbo/riparias-prep/raw/master/data/spatial/baseline/points_in_perimeter.geojson", stringsAsFactors = FALSE)
perimeter_shape <- readOGR("https://github.com/inbo/riparias-prep/raw/master/data/spatial/perimeter/Riparias_Official_StudyArea.geojson", stringsAsFactors = FALSE)

bbox <- as.data.frame(perimeter_shape@bbox)

ui <- dashboardPage(
  dashboardHeader(title = "Riparias maps"),
  dashboardSidebar(
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
  dashboardBody(
    tags$head(
      includeCSS(path = "www/style.css")
    ),
    fluidRow(
      box(width = 12, 
        img(src='Riparias_Logo.png', align = "right", height = 90)
      )
    ),
    fluidRow(
      box(
        title = "map", width = 12,
        uiOutput("text1"),
        leafletOutput("map", height = 600)
      )
    ),
    fluidRow(
      box(width = 12,
        "This tool was developed by the Research Institute for Nature and Forest within the framework of the Life RIPARIAS project"
      )
    )
  )
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
  
  output$map <- renderLeaflet({
    
    jaren <- seq(from = min(input$slider), 
                 to = max(input$slider),
                 by = 1)
    
    points_in_perimeter_sub <- subset(points_in_perimeter, 
                                      points_in_perimeter$year %in% jaren)
    
    points_in_perimeter_sub <- subset(points_in_perimeter_sub,
                                      points_in_perimeter_sub$vernacular_name_en %in%
                                        input$species)
    
    leaflet(points_in_perimeter_sub) %>% 
      addTiles() %>% 
      addPolylines(data = perimeter_shape) %>% 
      addCircleMarkers(popup = points_in_perimeter_sub$popup,
                       radius = 1,
                       color = "red") %>% 
      setMaxBounds(lng1 = bbox$min[1], 
                   lat1 = bbox$min[2], 
                   lng2 = bbox$max[1], 
                   lat2 = bbox$max[2])
  })
}

shinyApp(ui, server)

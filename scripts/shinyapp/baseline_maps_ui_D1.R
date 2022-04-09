library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)

#Reading in data####

branch <- "41_extending_baseline_map"

current_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/current_state.geojson"), stringsAsFactors = FALSE)

baseline_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/baseline.geojson"), stringsAsFactors = FALSE)

RBU_laag <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE)

RBSU_laag <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch,"/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"), stringsAsFactors = FALSE)

EEA_per_species_baseline <- st_as_sf(readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch,"/data/interim/EEA_per_species_baseline.geojson")))
EEA_per_species_current <- st_as_sf(readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch,"/data/interim/EEA_per_species_current.geojson")))

level_of_invasion_RBSU <- st_as_sf(readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch,"/data/interim/level_of_invasion_RBSU.geojson")))

level_of_invasion_RBSU_current <- level_of_invasion_RBSU%>%filter(state=='current')
level_of_invasion_RBSU_baseline <- level_of_invasion_RBSU%>%filter(state=='baseline')

bbox <- as.data.frame(RBU_laag@bbox)

occupancy_RBU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBU.csv"))
occupancy_RBSU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBSU.csv"))
full_name_RBSU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/input/Full_name_per_RBSU.csv"), sep=";")

occupancy_RBSU <- merge(occupancy_RBSU, full_name_RBSU, by.x='A0_CODE', by.y='Id', all.x=TRUE)

centroid_per_RBSU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/input/centroid_per_RBSU.csv"))
centroid_per_RBSU <- merge(centroid_per_RBSU, full_name_RBSU, by='Id', all.x=TRUE)

level_of_invasion_RBSU_current <- merge (level_of_invasion_RBSU_current, full_name_RBSU,by= 'Id', all.x=TRUE)
level_of_invasion_RBSU_baseline <- merge(level_of_invasion_RBSU_baseline, full_name_RBSU, by= 'Id', all.x=TRUE)

level_of_invasion_color_current <- as.data.frame(level_of_invasion_RBSU_current)
level_of_invasion_color_baseline <- as.data.frame(level_of_invasion_RBSU_baseline)

#Userinterface####

ui <- navbarPage(
  title = "Riparias D1",
  header= fluidRow(
    box(width = 12, 
      img(src='Riparias_Logo.png', align = "right", height = 90)
  )),
  ##Distribution####
  tabPanel("Distribution",
    tabsetPanel(
      tabPanel('Maps',
    titlePanel('Maps'),
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
                           choices = unique(current_state$scientific_name)
        )
        ),
      mainPanel(
        fluidRow(
          box(width = 12,
            uiOutput("text1"),
            leafletOutput("map", height = 600)
          )
        )
      )
  )
),#tabpanel
##Occupancy####
tabPanel('Occupancy',
         titlePanel('Occupancy'),
         
         sidebarLayout(
           sidebarPanel(
             selectInput("RBUi", "Select a river basin:",
                         choices = unique(occupancy_RBU$RBU))
           ),
           mainPanel(
             fluidRow(
               tabsetPanel(
                           tabPanel("Absolute occupancy", plotOutput("OccRBU")),
                           tabPanel("Relative occupancy", plotOutput("OccRBUREL"))
               )
             )
           )),
         

         sidebarLayout(
           sidebarPanel(
             selectInput("RBSUi", " Select a river basin subunit:",
                         choices = unique(occupancy_RBSU$fullnameRBSU))
           ),
           mainPanel(
             fluidRow(
               tabsetPanel(type = "tabs",
                           tabPanel("Absolute occupancy", plotOutput("OccRBSU")),
                           tabPanel("Relative occupancy", plotOutput("OccRBSUREL"))
               )
             )
           ))        
         
)#tabPanel

)#tabsetPanel
),#tabPanel
##Surveillance####
tabPanel('Surveillance',
      tabsetPanel(
        tabPanel('Observations',
        titlePanel('Observations'),
        sidebarLayout(

          sidebarPanel(
            selectInput("RBUi2", "Select a river basin:",
                        choices = unique(occupancy_RBU$RBU))
          ),
          mainPanel(
            fluidRow(
              plotOutput("graphRBU")
              )
            )
          ),
        sidebarLayout(
          
          sidebarPanel(
            selectInput("RBSUi2", "Select a river basin subunit:",
                        choices = unique(occupancy_RBSU$fullnameRBSU))
          ),
          mainPanel(
            fluidRow(
              plotOutput("graphRBSU")
            )
          )
        )
          
        ),#tabPanel,
        tabPanel('Effort',
                 titlePanel('Surveillance effort')
        )#tabPanel Effort
                 )#tabsetPanel 
         ),#tabPanel Surveillance
##Species trends####
tabPanel('Species trends',
         titlePanel('Species trends'),
         sidebarLayout(
           sidebarPanel(
             selectInput("Species_trends", "Select a species:",
                         choices = unique(occupancy_RBSU$scientific_name))
             ),#sidebarPanel
           mainPanel(
             fluidRow(
               box(
                 title='Observations',
                 plotOutput("plot_trends_obs")
               ),
               box(
                 title='Observations-corrected',
                 plotOutput("plot_trends_obs_cor")
               )
             ),#fluidRow,
             fluidRow(
               box(
                 title='Occupancy',
                 plotOutput("plot_trends_occ")
               ),
               box(
                 title='Occupancy-corrected',
                 plotOutput("plot_trends_occ_cor")
               )
             )#fluidRow,
           )#mainPanel
         )#sidebarLayout
         ),#tabPanel
##Management####
tabPanel('Management',
         titlePanel('Level of invasion'),
         sidebarLayout(
           sidebarPanel(
             selectInput("Species_loi", "Select a species:",
                         choices = unique(occupancy_RBSU$scientific_name)),
             selectInput("RBSU_loi", "Select a river basin subunit:",
                         choices = unique(centroid_per_RBSU$fullnameRBSU))),#sidebarPanel
           mainPanel(
             fluidRow(
             box(
               title='baseline state',
                 leafletOutput("map_level_of_invasion_baseline")
             ),
             box(
               title='current state',
                 leafletOutput("map_level_of_invasion_current")
                 )
             ),#fluidRow,
             fluidRow(
               box(
                 title='baseline state',
                 leafletOutput("map_baseline_state")
               ),
               box(
                 title='current state',
                 leafletOutput("map_current_state")
               )
             )#fluidRow,
      
           )#mainPanel
         )#sidebarLayout
         ),#tabPanel
img(src='Riparias_Logo.png', align = "right", height = 90)
)

  

#Server####
server <- function(input, output) { 
  ##Maps####
  ###Text1####
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
  
  ###Map####
  output$map <- renderLeaflet({
    
    jaren <- seq(from = min(input$slider), 
                 to = max(input$slider),
                 by = 1)
    
    current_state_sub <- subset(current_state, 
                                current_state$year %in% jaren)
    
    current_state_sub <- subset(current_state_sub,
                                current_state_sub$scientific_name %in%
                                  input$species)
    
    pal <- colorFactor(palette = c("#1b9e77", "#d95f02", "#636363"),
                       levels = c("ABSENT", "PRESENT", NA))
    
    leaflet(current_state_sub) %>% 
      addTiles() %>% 
      addPolylines(data = RBU_laag) %>% 
      addCircleMarkers(data = current_state_sub,
                       popup = current_state_sub$popup,
                       radius = 1,
                       color = ~pal(current_state_sub@data$occrrnS),
                       fillColor = ~pal(current_state_sub@data$occrrnS)) %>% 
      addLegend(data = current_state_sub,
                title = "occurrence Status",
                values = ~unique(current_state@data$occrrnS),
                pal = pal) %>% 
      setMaxBounds(lng1 = bbox$min[1], 
                   lat1 = bbox$min[2], 
                   lng2 = bbox$max[1], 
                   lat2 = bbox$max[2])
  })
  
  ##occupancy####
  ###Occupance_RBU_absoluut####
  

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
      labs(y = "Absolute occupancy (1km² grid cells)")+ 
      labs(x = "Species")
    
  })
  
  ###Occupance_RBU_relatief####
  output$OccRBUREL <-renderPlot ({
    ggplot(datOcc(), aes(x=scientific_name, y=Occupancy_rel, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_brewer(palette="Paired")+
      coord_flip()+ 
      labs(y = "Relative occupancy")+ 
      labs(x = "Species")
    
  })
  
  ###Occupance_RBSU_absoluut####
  datOcc2<-reactive({
    test3 <- occupancy_RBSU[(occupancy_RBSU$fullnameRBSU == input$RBSUi),]
    test3
  })
  
  output$OccRBSU <-renderPlot ({
    ggplot(datOcc2(), aes(x=scientific_name, y=Occupancy, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_brewer(palette="Paired")+
      coord_flip()+ 
      labs(y = "Absolute occupancy (1km² grid cells)")+ 
      labs(x = "Species")
    
  })
  ###Occupance_RBSU_relatief####
  output$OccRBSUREL <-renderPlot ({
    ggplot(datOcc2(), aes(x=scientific_name, y=Occupancy_rel, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_brewer(palette="Paired")+
      coord_flip()+ 
      labs(y = "Relative occupancy")+ 
      labs(x = "Species")
    
  })
  
  ##Surveillance####
  ###Observations####
  ####Observations_RBU####
  datObs<-reactive({
    test <- occupancy_RBU[(occupancy_RBU$RBU == input$RBUi2),]
    test
  })
  
  output$graphRBU <-renderPlot ({
    ggplot(datObs(), aes(x=scientific_name, y=n_observations, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_brewer(palette="Paired")+
      coord_flip()+ 
      labs(y = "Number of observations")+ 
      labs(x = "Species")
    
  })
  
  ####Observations_RBSU####
  datObs2<-reactive({
    test2 <- occupancy_RBSU[(occupancy_RBSU$fullnameRBSU == input$RBSUi2),]
    test2
  })
  
  output$graphRBSU <-renderPlot ({
    ggplot(datObs2(), aes(x=scientific_name, y=n_observations, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_brewer(palette="Paired")+
      coord_flip()+ 
      labs(y = "Number of observations")+ 
      labs(x = "Species")
    
  })
  ##Species_trends####
  ###plot_trends_obs####
  
  #output$plot_trends_obs <- renderPlot ({})
  ###plot_trends_obs_cor####
  
  #output$plot_trends_obs_cor <- renderPlot ({})
  ###plot_trends_occ####
  
  #output$plot_trends_occ <- renderPlot ({})
  ###plot_trends_occ_cor####
  
  #output$plot_trends_occ_cor <- renderPlot ({})
  ##Level of invasion####
  ###Level of invasion baseline####
    output$map_level_of_invasion_baseline <- renderLeaflet({
      
      labels <- sprintf(
        "<strong>%s</strong>",
        level_of_invasion_RBSU_baseline$fullnameRBSU
      ) %>% lapply(htmltools::HTML)
      
      pal <- colorFactor(palette = c("yellow", "orange", "red", "grey"),
                         levels = c( "scattered occurences only", "weakly invaded", "heavily invaded", NA))
      
      leaflet(level_of_invasion_RBSU_baseline)%>%
        addTiles()%>%
        addPolygons(
          fillColor = ~pal(level_of_invasion_color_baseline[,str_replace(input$Species_loi, ' ', '.')]),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.5,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))%>%
        addLegend(data = level_of_invasion_color_baseline,
                  title = "Level of invasion",
                  values = ~c("scattered occurences only", "weakly invaded", "heavily invaded", NA),
                  pal = pal)
      
    
  })
  
  ###Level_of_invasion_current####
  output$map_level_of_invasion_current <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong>",
      level_of_invasion_RBSU_current$fullnameRBSU
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorFactor(palette = c("yellow", "orange", "red", "grey"),
                       levels = c( "scattered occurences only", "weakly invaded", "heavily invaded", NA))
    
    leaflet(level_of_invasion_RBSU_current)%>%
      addTiles()%>%
      addPolygons(
        fillColor = ~pal(level_of_invasion_color_current[,str_replace(input$Species_loi, ' ', '.')]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.5,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
      addLegend(data = level_of_invasion_color_current,
                title = "Level of invasion",
                values = ~c("scattered occurences only", "weakly invaded", "heavily invaded", NA),
                pal = pal)
    
  })
    
  ###Map_baseline_state####
  

  
    output$map_baseline_state  <- renderLeaflet({
      
    baseline_state_sub <- subset(baseline_state,
                                  baseline_state$scientific_name %in%
                                    input$Species_loi)
    
    EEA_per_species_baseline_sub <- subset(EEA_per_species_baseline,
                                           EEA_per_species_baseline$scientific_name %in%
                                             input$Species_loi)
    leaflet() %>% 
      addTiles() %>% 
      addPolylines(data = RBSU_laag, color="grey") %>%
      addPolygons(data = EEA_per_species_baseline_sub, color="grey") %>%
      addCircleMarkers(data = baseline_state_sub,
                       popup = baseline_state_sub$popup,
                       radius = 1,
                       color="blue")
      
    })
  
  ###map_current_state####
  output$map_current_state  <- renderLeaflet({
    
    current_state_sub <- subset(current_state,
                                 current_state$scientific_name %in%
                                   input$Species_loi)
    
    EEA_per_species_current_sub <- subset(EEA_per_species_current,
                                           EEA_per_species_current$scientific_name %in%
                                             input$Species_loi)
    
    leaflet() %>% 
      addTiles() %>% 
      addPolylines(data = RBSU_laag, color="grey") %>%
      addPolygons(data = EEA_per_species_current_sub, color="grey") %>%
      addCircleMarkers(data = current_state_sub,
                       popup = current_state_sub$popup,
                       radius = 1,
                       color="blue")
    
  })
  
    center <- reactive({
      subset(centroid_per_RBSU, fullnameRBSU == input$RBSU_loi) 
    })
    
    observe({
      leafletProxy('map_baseline_state') %>% 
        setView(lng =  center()$longitude, lat = center()$latitude, zoom = 11)
    })
    observe({
      leafletProxy('map_level_of_invasion_current') %>% 
        setView(lng =  center()$longitude, lat = center()$latitude, zoom = 11)
    })
    observe({
      leafletProxy('map_level_of_invasion_baseline') %>% 
        setView(lng =  center()$longitude, lat = center()$latitude, zoom = 11)
    })
    observe({
      leafletProxy('map_current_state') %>% 
        setView(lng =  center()$longitude, lat = center()$latitude, zoom = 11)
    })
    
    
    
}

shinyApp(ui, server)

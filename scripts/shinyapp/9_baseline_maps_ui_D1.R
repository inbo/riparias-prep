# 0. Packages ####
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)
library(trias) 
library(readxl)

# Note: possible cause of failure concerning trias package. Only packages installed from GitHub with devtools::install_github, in version 1.4 (or later) of devtools, are supported. Packages installed with an earlier version of devtools must be reinstalled with the later version before you can deploy your application. If you get an error such as “PackageSourceError” when you attempt to deploy, check that you have installed all the packages from Github with devtools 1.4 or later.

# 1. Import data ####
repo <- "https://github.com/inbo/riparias-prep/raw/"
repo_raw <- "https://raw.githubusercontent.com/inbo/riparias-prep/refs/heads/"
branch <- "master"

## Maps ####
all_pointdata_2000 <- st_read(paste0(repo, branch,
                                     "/data/spatial/baseline/points_in_perimeter_sel.geojson"))

current_state <- st_read(paste0(repo, branch,
                                "/data/spatial/baseline/current_state.geojson"))

baseline_state <- st_read (paste0(repo, branch,
                                  "/data/spatial/baseline/baseline.geojson"))

RBU_laag <- st_read(paste0(repo, branch,
                           "/data/spatial/Riparias_subunits/RBU_RIPARIAS_12_02_2021.geojson"))

RBSU_laag <- st_read(paste0(repo, branch,
                            "/data/spatial/Riparias_subunits/RBSU_RIPARIAS_12_02_2021.geojson"))

## Occupancy and observations ####
occupancy_RBU <- read.csv(paste0(repo, branch,
                                 "/data/interim/occupancy_rel_RBU.csv"))

occupancy_RBSU <- read.csv(paste0(repo, branch,
                                  "/data/interim/occupancy_rel_RBSU.csv")) %>%
  rename(A0_CODE = RBSU)

Surveillance_effort_RBSU <- read.csv(paste0(repo, branch,
                                            "/data/interim/Surveillance_effort.csv"))

full_name_RBSU <- read.csv(paste0(repo, branch,
                                  "/data/input/Full_name_per_RBSU.csv"), sep = ";")

## Surveillance effort ####
EEA_per_species_baseline <- st_read(paste0(repo, branch,
                                           "/data/interim/EEA_per_species_baseline.geojson"))

EEA_per_species_current <- st_read(paste0(repo, branch,
                                          "/data/interim/EEA_per_species_current.geojson"))

EEA_surveillance_effort <- st_read(paste0(repo, branch,
                                          "/data/interim/EEA_high_search_effort.geojson"))

## Management - summarizing table ####
table_summarizing_management <- readr::read_csv2(paste0(repo_raw, branch, 
                                                  "/data/interim/summarizing_management_table.csv")) %>% 
  dplyr::mutate(baseline = as.integer(baseline),
                current = as.integer(current))

## Management - maps: Level of invasion ####
level_of_invasion_RBSU <- st_read(paste0(repo, branch,
                                         "/data/interim/level_of_invasion_RBSU.geojson"))

level_of_invasion_RBSU_current <- level_of_invasion_RBSU %>%
  filter(state == 'current')

level_of_invasion_RBSU_baseline <- level_of_invasion_RBSU %>%
  filter(state == 'baseline')

bbox <- st_bbox(RBU_laag)

## Site-level monitoring ####
dafor_monitoring <- read.csv(paste0(repo, branch,
                                    "/data/interim/dafor_monitoring.csv"))

## Add RBSU name to dataframes ####
occupancy_RBSU <- merge(occupancy_RBSU, full_name_RBSU, by.x = 'A0_CODE', by.y = 'Id', all.x = TRUE)
Surveillance_effort_RBSU <- merge(Surveillance_effort_RBSU, full_name_RBSU, by = 'Id', all.x = TRUE)
centroid_per_RBSU <- read.csv(paste0(repo, branch,
                                     "/data/input/centroid_per_RBSU_versie2.csv"))
centroid_per_RBSU <- merge(centroid_per_RBSU, full_name_RBSU, by = 'Id', all.x = TRUE)

level_of_invasion_RBSU_current <- merge(level_of_invasion_RBSU_current, full_name_RBSU,
                                        by = 'Id', all.x = TRUE)
level_of_invasion_RBSU_baseline <- merge(level_of_invasion_RBSU_baseline, full_name_RBSU,
                                         by = 'Id', all.x = TRUE)

level_of_invasion_color_current <- level_of_invasion_RBSU_current %>%
  st_drop_geometry()

level_of_invasion_color_baseline <- level_of_invasion_RBSU_baseline %>%
  st_drop_geometry()

df_ts_compact <- read.csv(paste0(repo, branch,
                                 "/data/interim/trends_compact.csv"))

## Calculate evaluation years ####
evaluation_years <- seq(from = as.integer(format(Sys.Date(), "%Y")) - 4,
                        to = as.integer(format(Sys.Date(), "%Y")) - 1)

maxjaar <- as.integer(format(Sys.Date(), "%Y"))


tags$head(
  tags$style(HTML("
    .custom-box {
      background-color: #f0f0f0;
      border-radius: 10px;
      padding: 15px;
      margin-top: 20px;
      margin-bottom: 20px;
      border: 1px solid #cccccc;
      box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);
    }
  "))
)

# 2. User Interface ####

ui <- navbarPage(
  title = div(img(src = "logoLIFEsimple.jpg", height = 30),
              img(src = "logoRIP_transparant.png", height = 30),
              "D1 dashboard"),
  
  ## Home ####
  tabPanel("Home",
           fluidPage(
             
             tags$head(
               tags$style(HTML("
      .custom-box {
        background-color: #f0f0f0;  /* Light grey background */
        border-radius: 10px;       /* Rounded corners */
        padding: 15px;            /* Inner spacing */
        margin-top: 20px;         /* Space above the box */
        margin-bottom: 20px;      /* Space below the box */
        border: 1px solid #cccccc; /* Optional border */
        box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1); /* Shadow effect */
      }
      
          .custom-box2 {
      background-color: #ffffff;       /* White background */
      border-radius: 10px;            /* Rounded corners */
      padding: 15px;                 /* Inner spacing */
      margin-top: 20px;              /* Space above the box */
      margin-bottom: 20px;           /* Space below the box */
      border: 4px solid  #00a491;     /* Thick border with color #007c66 */
      box-shadow: 2px 2px 10px rgba(0, 164, 145, 0.5); /* Teal-green shadow */
          }
          
                .custom-box3 {
      background-image: url('background2.jpg'); /* Correct syntax for background image */
      background-size: cover;                 /* Ensures the image covers the entire box */
      background-repeat: no-repeat;          /* Prevents tiling */
      background-position: center;           /* Centers the image */
      color: #ffffff;                        /* Makes the text white */
      border-radius: 10px;                    /* Rounded corners */
      padding: 15px;                         /* Inner spacing */
      margin-top: 20px;                      /* Space above the box */
      margin-bottom: 20px;                   /* Space below the box */
      border: 4px solid #00a491;             /* Thick border with color #00a491 */
      box-shadow: 2px 2px 10px rgba(0, 164, 145, 0.5); /* Teal-green shadow */
    }
    .custom-box4 {
        background-color: #f0f0f0;
        border-radius: 10px;
        padding: 15px;
        text-align: center;
        border: 1px solid #cccccc;
        box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);
    }
      
    .custom-image {
      width: 100%;
      height: auto;
      border-radius: 10px;
    }

  
    /* Sidebar background color with opacity */
          .well {
            background-color: #ffffff;
            padding: 15px;
          }
          
    /* Sidebar text color */
          .well .control-label, .well h3, .well h4 {
            color: #00a491;
          }                    
                        
 
  
    "))),
               
             box(
               width = 12,
               class = "custom-box2",
               HTML(
                 "<h1>Welcome to the RIPARIAS Dashboard</h1>
               <p>This dashboard gathers data related to work performed within the 
               <a href='https://www.riparias.be/' target='_blank'>LIFE RIPARIAS project</a> (LIFE19 NAT/BE/000953). 
               It allows project partners to consult basic information, and to track the project’s progress.</p>
               
               <h3>On Data & Development</h3>
               <p>Most information shown on these webpages is publicly available through 
               <a href='https://www.gbif.org/' target='_blank'>GBIF</a>, but some is internally stored on our systems. 
               Data are analysed and visualised using 
               <a href='https://shiny.posit.co/' target='_blank'>Shiny for R</a>. 
               The Git repository is accessible 
               <a href='https://github.com/inbo/riparias-prep' target='_blank'>here</a>.</p>
               
               <h3>On Species</h3>
               <p>The dashboard relates to all species of the 
               <a href='https://www.gbif.org/dataset/fd004d9a-2ea4-4244-bb60-0df508d20a15' target='_blank'>
               RIPARIAS target species list</a>.</p>
               
               <h3>Contact</h3>
               <p>The dashboard is maintained by the Research Institute for Nature and Forest (INBO), as part of action D1. 
               The contact address for inquiries or suggestions is 
               <a href='mailto:faunabeheer@inbo.be'>faunabeheer@inbo.be</a>.</p>"
               )
             ),
             
             fluidRow(
               column(
                 width = 3, # Each column takes 3/12 of the row
                 div(class = "custom-box4",
                     img(src = "image1.jpg", class = "custom-image"),
                     p(tags$i("Houttuynia cordata "), "by \u00A9 Meneerke bloem")
                 )
               ),
               column(
                 width = 3,
                 div(class = "custom-box4",
                     img(src = "image2.jpg", class = "custom-image"),
                     p(tags$i("Erythranthe guttata"))
                 )
               ),
               column(
                 width = 3,
                 div(class = "custom-box4",
                     img(src = "image3.JPG", class = "custom-image"),
                     p(tags$i("Procambarus clarkii")," by \u00A9 Arnoud Monty")
                 )
               ),
               column(
                 width = 3,
                 div(class = "custom-box4",
                     img(src = "image4.jpg", class = "custom-image"),
                     p(tags$i("Zizania latifolia ")," by \u00A9 Dido Gosse")
                 )
               )
             )
           )
           
           
  ),
  
  ## Distribution ####
  tabPanel("Distribution",
           tabsetPanel(
             ### Maps ####
             tabPanel('Maps',
                titlePanel('Maps'),
                   fluidPage(    
                     # Add the descriptive text box with rounded edges and grey background
                      box(
                        width = 12,
                        class = "custom-box",
                        HTML("<p>The <b>map</b> below shows observations of species within the selected timeframe. The <b>RIPARIAS                          project area</b> is displayed.</p>")
                      )
                      ,
                      # First sidebar layout 
                      box(
                        width = 12,
                        class = "custom-box2",
                        title = "Map of observations within Riparias project area", # Title for the box
                        
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("slider", "Years", 
                                        2000, lubridate::year(Sys.Date()), 1,
                                        value = c(2021, lubridate::year(Sys.Date())),
                                        # default: from project start to now
                                        dragRange = TRUE),
                            checkboxGroupInput("species", "Species",
                                               choices = sort(unique(all_pointdata_2000$species))),
                            width = 3 # Out of 12
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
                        
                      )
                   )
             ),

                ###Observations####    
tabPanel(
  "Observations",
  titlePanel('Observations'),
  fluidPage(
    # Add the descriptive text box with rounded edges and grey background
    box(
      width = 12,
      class = "custom-box",
      HTML("<p>The number of <b>observations</b> per species. Presence as well as absence. 
            Divided between periods.</p>
            <p>
            <li>For <b>plants</b>: <i>baseline</i> period from <b>2000-2020</b>. 
            <i>Current</i> period from <b>2021-present</b>.</li>
            <li>For <b>crayfish</b>: <i>baseline</i> period from <b>2000-2015</b>. 
            <i>Current</i> period from <b>2016-present</b>.</li>
            </p>")
    ),
    # First sidebar layout for river basin in a custom box
    box(
      width = 12,
      class = "custom-box2",
      title = "At River Basin Level", # Title for the box
      sidebarLayout(
        sidebarPanel(
          selectInput("RBUi2", "Select a river basin:",
                      choices = unique(occupancy_RBU$RBU)),
          width = 3 # Out of 12
        ),
        mainPanel(
          fluidRow(
          plotOutput("graphRBU")
          )
        )
      )
    ),
    
    # Second sidebar layout for river basin subunit in a custom box
    box(
      width = 12,
      class = "custom-box2",
      title = "At River Basin Subunit Level", # Title for the box
      sidebarLayout(
        sidebarPanel(
          selectInput("RBSUi2", "Select a river basin subunit:",
                      choices = unique(occupancy_RBSU$fullnameRBSU)),
          width = 3 # Out of 12
        ),
        mainPanel(
          fluidRow(
            plotOutput("graphRBSU")
          )
        )
      )
    )
  )
),
             ### Occupancy ####
             tabPanel('Occupancy',
                      titlePanel('Occupancy'),
                      fluidPage(
                      box(
                        width = 12,
                        class = "custom-box",
                        HTML("<p><b>Occupancy</b>, or: the number of grid cells with observations of the species (<a href=https://www.eea.europa.eu/data-and-maps/figures/eea-reference-grids>EEA</a> 1-km² grid). Expressed as the <b>absolute</b> number of occupied cells, or <b>relative</b> to the number of cells in the river basin. Divided between periods.</p><p>
<li>For <b>plants</b>: <i>baseline</i> period from <b>2000-2020</b>. 
<i>Current</i> period from <b>2021-present</b>.</li>
<li>For <b>crayfish</b>: <i>baseline</i> period from <b>2000-2015</b>.
<i>Current</i> period from <b>2016-present</b>.</li>
     </p>")
                      ),
                      
                      box(
                        width = 12,
                        class = "custom-box2",
                        title = "At River Basin Level", # Title for the box
                        
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("RBUi", "Select a river basin:",
                                        choices = unique(occupancy_RBU$RBU)),
                            width = 3 # Out of 12
                          ),
                          mainPanel(
                            fluidRow(
                              tabsetPanel(
                                tabPanel("Absolute occupancy", plotOutput("OccRBU")),
                                tabPanel("Relative occupancy", plotOutput("OccRBUREL"))
                              )
                            )
                          ))
                        
                        ),
                      
                      box(
                        width = 12,
                        class = "custom-box2",
                        title = "At River Basin Subunit Level", # Title for the box
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("RBSUi", " Select a river basin subunit:",
                                        choices = unique(occupancy_RBSU$fullnameRBSU)),
                            width = 3 # Out of 12
                          ),
                          mainPanel(
                            fluidRow(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Absolute occupancy", plotOutput("OccRBSU")),
                                          tabPanel("Relative occupancy", plotOutput("OccRBSUREL"))
                              )
                            )
                          ))
                            )
                          )
                        ),
tabPanel('Level of invasion',
         titlePanel('Level of invasion'),
         fluidPage(
         box(
           width = 12,
           HTML('
<div class="custom-box">
  <!-- Introductory text -->
  <p>Level of invasion is based on relative occupancy (the number of grid cells with observations of the species (<a href=https://www.eea.europa.eu/data-and-maps/figures/eea-reference-grids>EEA</a> 1-km² grid), relative</b> to the number of cells in the river basin or river basin subunit. Level of invasion is scale dependent (at river basin or river basin subunit level). Baseline and current period differ between plants and crayfish.</p>
  
  <!-- Periods for plants and crayfish -->
  <ul>
    <li>For <b>plants</b>: <i>baseline</i> period from <b>2000-2020</b>. 
    <i>Current</i> period from <b>2021-present</b>.</li>
    <li>For <b>crayfish</b>: <i>baseline</i> period from <b>2000-2015</b>. 
    <i>Current</i> period from <b>2016-present</b>.</li>
  </ul>
  
  <!-- Flex container for the additional columns -->
  <div style="display: flex; justify-content: space-between; gap: 10px;">
    
    <!-- Column 3: RBSU -->
    <div style="flex: 1; padding: 10px;">
      <h4>River Basin Subunit Level</h4>
      <p>Not recorded: relative occupancy equals 0</p>
      <p>Scattered occurrences only: 0 < relative occupancy ≤ 0.10</p>
      <p>Weakly invaded: 0.10 < relative occupancy ≤ 0.20</p>
      <p>Heavily invaded: relative occupancy > 0.20</p>
    </div>
    
    <!-- Column 4: RBU -->
    <div style="flex: 1; padding: 10px;">
      <h4>River Basin Unit Level</h4>
      <p>Not recorded: relative occupancy equals 0</p>
      <p>Scattered occurrences only: 0 < relative occupancy ≤ 0.01</p>
      <p>Weakly invaded: 0.01 < relative occupancy ≤ 0.05</p>
      <p>Heavily invaded: relative occupancy > 0.05</p>
    </div>

  </div>
</div>

                  ')

),

box(
  width = 12,
  class = "custom-box2",
           
         sidebarLayout(
           sidebarPanel(
             selectInput("Species_loi", "Select a species:",
                         choices = unique(occupancy_RBSU$species)),
             selectInput("RBSU_loi", "Select a river basin subunit:",
                         choices = unique(centroid_per_RBSU$fullnameRBSU))),#sidebarPanel
           mainPanel(
             fluidRow(
               box(
                 title='baseline state - level of invasion',
                 leafletOutput("map_level_of_invasion_baseline")
               ),
               box(
                 title='current state - level of invasion',
                 leafletOutput("map_level_of_invasion_current")
               )
             ),#fluidRow,
             fluidRow(
               box(
                 title='baseline state - observations in detail',
                 leafletOutput("map_baseline_state")
               ),
               box(
                 title='current state - observations in detail',
                 leafletOutput("map_current_state")
               )
             )#fluidRow,
             
           )#mainPanel
         ),#sidebarLayout,
)
)#fluidPage
),
           )#tabsetPanel
  ),#tabPanel

  ##Surveillance####
  tabPanel('Surveillance',
           titlePanel('Surveillance effort'),
           fluidPage(
             box(
               width = 12,
               class = "custom-box",
               HTML("<p>Under development. Soon a new measure of surveillance effort will be applied.
            </p>")
             ),
           box(
             width = 12,
             class = "custom-box2",
             fluidRow(
                        box(
                          'Percentage of EEA cells (1km²) per river basin subunit with heigh surveillance effort for plant species',
                          plotOutput("Plot_surveillance_effort_RBSU", height=600)
                        ),
                        box(
                          'Distribution of EEA cells (1km²) with high surveillance effort for plant species',
                          leafletOutput("map_EEA_surveillance_effort", height=600)
                        )
                      )#fluidrow
             )
           )#tabPanel Effort
           )#tabsetPanel 
  ,#tabPanel Surveillance
  ##Species trends####
  tabPanel('Species trends',
           titlePanel('Species trends'),
           fluidPage(
             box(
               width = 12,
               class = "custom-box",
               HTML('<p>
                      An overview of the  <a href="https://trias-project.github.io/indicators/" target="_blank">
                      TRIAS indicators
                    </a>, per species in Riparias project area.
                    </p>')
             ),
            
             box(
               width = 12,
               class = "custom-box2", 
               
               sidebarLayout(
                 sidebarPanel(
                   selectInput("Species_trends", "Select a species:",
                               choices = unique(occupancy_RBSU$species))
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
             )
           )
    ),#tabPanel
  ##Management####
  tabPanel('Management',
           tabsetPanel(
             #tabPanel,
             tabPanel('Table',
                      titlePanel('Management Table (current versus target state)'),
                      fluidPage(
                        box(
                          width = 12,
                          class = "custom-box",
                          HTML('Per river basin, the number of river basin subunits where the species is present in the current state (2021-present) is displayed. The baseline and target number are also mentioned.')),
                        
                        box(
                          width = 12,
                          class = "custom-box2",
                      tableOutput('table_summarizing_management')
                      )
             )
           )
           )
           #tabsetPanel
           ##Site-level monitoring####
  ),#tabPanel
  tabPanel('Site-level monitoring',
           fluidPage(
             box(
               width = 12,
               class = "custom-box",
               HTML("In those areas where management is performed, monitoring surveys are organised before and after management to assess its impact. The crayfish data presents dummy data.")),
             
             box(
               width = 12,
               class = "custom-box2",
           sidebarLayout(
             sidebarPanel(
               selectInput("Species_dafor", "Select a species:",
                           choices = unique(dafor_monitoring$species)
               )),
             mainPanel(
               fluidRow(
                 box(
                   title='Plants',
                   plotOutput("DAFOR")
                 )#box
               )#fluidRow
             )#mainPanel
           )
           )
           ,#sidebarlayout
           box(
             width = 12,
             class = "custom-box2",
           sidebarLayout(
             sidebarPanel(
               selectInput("Species_cpue", "Select a species:",
                           choices = c( "Orconectes virilis",
                                        "Procambarus clarkii",
                                        "P. fallax")
               )),
             mainPanel(
               fluidRow(
                 box(
                   title='Crayfish',
                   plotOutput("CPUE")
                 )#box
               )#fluidRow
             )#mainPanel
           )#Sidebarlayout
           )
  )
  )
,
tags$head(
  tags$style(HTML("
      /* Change navbar background color */
      .navbar-default {
        background-color: #00a491;  /* Dark green background */
        border-color: #00a491;
      }
      /* Change text color */
      .navbar-default .navbar-brand {
        color: white;
      }
      .navbar-default .navbar-nav > li > a {
        color: white;
      }
      /* Change color on hover */
      .navbar-default .navbar-nav > li > a:hover {
        color: #FFD700;  /* Golden hover color */
      }
    "))
)
)

# 3. Server ####

server <- function(input, output) { 
  ##Maps####
  ###Text1####
  output$text1 <- renderUI({
    text <- "Select at least one species to display observations."
    if(length(input$species) == 1){
      text <- HTML(paste0("Currently showing ",
                          strong(em(input$species)), " observations from ", 
                          strong(input$slider[1]), " to ", strong(input$slider[2]), "."))
    }
    if(length(input$species) == 2){
      text <- HTML(paste0("Currently showing ",
                          strong(em(paste(input$species, collapse = " & "))),
                          " observations from ",
                          strong(input$slider[1]), " to ", strong(input$slider[2]), "."))
    }
    if(length(input$species) > 2){
      last_species <- input$species[length(input$species)]
      species <- subset(input$species, !input$species %in% last_species)
      text <- HTML(paste0("Currently showing ",
                          strong(em(paste(species, collapse = ", "))), " & ", 
                          strong(em(last_species)),
                          " observations from ", 
                          strong(input$slider[1]), " to ", strong(input$slider[2]), "."))
    }
    print(text)
    
  })
  
  ###map####
  output$map <- renderLeaflet({
    
    jaren <- seq(from = min(input$slider), 
                 to = max(input$slider),
                 by = 1)
    
    all_pointdata_2000_sub <- all_pointdata_2000 %>%
      filter(year%in%jaren)%>%
      filter(species%in%input$species)
    
    pal <- colorFactor(palette = c("#00a491", "#d95f02", "#636363"),
                       levels = c("ABSENT", "PRESENT", NA))
    
    leaflet(all_pointdata_2000_sub) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolylines(data = RBU_laag, color= "#00a491", opacity=1) %>% 
      addCircleMarkers(data = all_pointdata_2000_sub,
                       popup = all_pointdata_2000_sub$species,
                       radius=1,
                       color = ~pal(all_pointdata_2000_sub$occurrenceStatus),
                       fillColor = ~pal(all_pointdata_2000_sub$occurrenceStatus)) %>% 
      addLegend(data = all_pointdata_2000_sub,
                title = "occurrence Status",
                values = c("ABSENT", "PRESENT", NA),
                pal = pal) %>% 
      setMaxBounds(lng1 = bbox[1], 
                   lat1 = bbox[2], 
                   lng2 = bbox[3], 
                   lat2 = bbox[4])
  })
  
  ##occupancy####
  ###Occupance_RBU_absoluut####
  
  
  datOcc <-reactive({
    test1 <- occupancy_RBU[(occupancy_RBU$RBU == input$RBUi),]
    test1
  })
  
  output$OccRBU <-renderPlot ({
    ggplot(datOcc(), aes(x=species, y=Occupancy, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_manual(values=c("#99d8c9","#00a491"))+
      coord_flip()+ 
      labs(y = "Absolute occupancy (1km² grid cells)")+ 
      labs(x = "Species")
    
  })
  
  ###Occupance_RBU_relatief####
  #geom_text(aes(label = signif(CC,2)), hjust = -0.2)
  output$OccRBUREL <-renderPlot ({
    ggplot(datOcc(), aes(x=species, y=Occupancy_rel, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_manual(values=c("#99d8c9","#00a491"))+
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
    ggplot(datOcc2(), aes(x=species, y=Occupancy, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_manual(values=c("#99d8c9","#00a491"))+
      coord_flip()+ 
      labs(y = "Absolute occupancy (1km² grid cells)")+ 
      labs(x = "Species")
    
  })
  ###Occupance_RBSU_relatief####
  #geom_text(aes(label = signif(CC,2)), hjust = -0.2)
  output$OccRBSUREL <-renderPlot ({
    ggplot(datOcc2(), aes(x=species, y=Occupancy_rel, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_manual(values=c("#99d8c9","#00a491"))+
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
    ggplot(datObs(), aes(x=species, y=n_observations, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_manual(values=c("#99d8c9","#00a491"))+
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
    ggplot(datObs2(), aes(x=species, y=n_observations, fill= state)) +
      geom_bar(stat="identity", position=position_dodge())+
      theme_minimal() +
      scale_fill_manual(values=c("#99d8c9","#00a491"))+
      coord_flip()+ 
      labs(y = "Number of observations")+ 
      labs(x = "Species")
    
  })
  
  ##Site-level monitoring####
  ###DAFOR####
  dat_dafor <-reactive({
    test5 <- dafor_monitoring[(dafor_monitoring$species == input$Species_dafor),]
    test5
  })
  
  output$DAFOR <- renderPlot ({
    ggplot(dat_dafor(), 
           aes(fill=DAFOR, 
               y=count, 
               x=factor(Time_period,
                        level = c('before management',
                                  'after management (2026)',
                                  'afterlife (2031)')))) + 
      geom_bar(position="stack", stat="identity")+
      labs(y='Number of sites')+
      labs(x='Time period')+
      theme_bw()
  })
  
  ###CPUE####
  output$CPUE <- renderPlot ({
    df2 <- data.frame(location=rep(c("site 1", "site 2", "site 3"), each=2),
                      dose=rep(c("baseline", "target"),3),
                      len=c(6.8, 15, 12, 4.2, 10, 6))
    
    ggplot(df2, aes(x=dose, y=len, group=location)) +
      geom_line(aes(linetype=location))+
      geom_point()+
      labs(y='CPUE')+
      labs(x='Time')+
      theme_bw()
  })
  
  
  
  ###Surveillance effort per RBSU####
  ####Plot_surveillance_effort_RBSU####
  
  output$Plot_surveillance_effort_RBSU <-renderPlot ({
    ggplot(Surveillance_effort_RBSU, aes(x=fullnameRBSU, y=SurveillanceEffortRel)) +
      geom_bar(stat="identity", fill = "#00a491")+
      coord_flip()+ 
      labs(y = "Percentage of EEA 1 km² cells with high surveillance effort")+ 
      labs(x = "River basin subunit")
  })
  
  ####map_EEA_surveillance_effort####
  #labels_se <- sprintf(
  #  "<strong>%s</strong>",
  #  RBSU_laag$fullnameRBSU
  #) %>% lapply(htmltools::HTML)
  
  output$map_EEA_surveillance_effort <- renderLeaflet ({
    leaflet(EEA_surveillance_effort) %>% 
      addTiles() %>% 
      addPolygons(color="#00a491", weight=1)
    
    
  })
  
  ##Species_trends####
  # For every type of trend plot there will be 3 "results".
  # 1: An assessment of emergence can be made => GAM graph
  # 2: No assessment of emergence can be made => ALT graph 
  # 3: No data in dataset => Error message
  
  ### subset data ####
  df_key <- reactive({
    df_key <- df_ts_compact[(df_ts_compact$canonicalName == input$Species_trends),]
  })
  
  ###plot_trends_obs####
  
  output$plot_trends_obs <- renderPlot ({
    
    df_key_1 <- df_key()
    
    trend_type <- "observations"
    
    #### Determine emergence status ####
    if(nrow(df_key_1) > 0){
      
      test_eval_years <- FALSE %in% unique(evaluation_years %in% df_key_1$year)
      
      if(test_eval_years == FALSE){
        results_gam <- apply_gam(
          df = df_key_1,
          y_var = "obs",
          taxonKey = "taxonKey",
          eval_years = evaluation_years,
          type_indicator = "observations",
          taxon_key = unique(df_key_1$taxonKey),
          name = unique(df_key_1$canonicalName)
        )
      }else{
        results_gam <- list(plot = NULL)
      }
    }else{
      results_gam <- "empty"
    }
    
    #### Create plots ####
    ##### ALT_Plot ####
    if(is.null(results_gam$plot)){
      alt_plot <- df_key_1 %>% 
        ggplot(aes(x = year, y = obs)) + 
        ylab("observations") +
        geom_point(stat = "identity") +
        scale_x_continuous(breaks = seq(from = min(df_key_1$year, na.rm = TRUE),
                                        to = max(df_key_1$year, na.rm = TRUE),
                                        by = 5)) 
      
      if(max(df_key_1$obs, na.rm = TRUE) == 1){
        alt_plot <- alt_plot +
          scale_y_continuous(breaks =  seq(from = 0,
                                           to = 2,
                                           by = 1)) +
          annotate("text", x = max(df_key_1$year), y = 2, label = paste0("The ", trend_type, " trend of \n", input$Species_trends, " \n cannot be assessed."),vjust = "inward", hjust = "inward", colour = "red")
      }else{
        alt_plot <- alt_plot +
          annotate("text", x = max(df_key_1$year), y = max(df_key_1$obs), label = paste0("The ", trend_type, " trend of \n", input$Species_trends, " \n cannot be assessed."),vjust = "inward", hjust = "inward", colour = "red")
      }
      
      print(alt_plot)
    }
    
    ##### No Data plot ####
    if("empty" %in% results_gam){
      alt_plot_2 <- df_key_1 %>% 
        ggplot(aes(x = year, y = obs)) + 
        ylab("observations") +
        geom_point(stat = "identity") +
        annotate("text", x = maxjaar, y = 1, label = paste0(input$Species_trends, " \n is not yet present \n in Belgium"),vjust = "inward", hjust = "inward", colour = "red")
      print(alt_plot_2)
    }
    ##### GAM plot ####
    if(!"empty" %in% results_gam & !is.null(results_gam$plot)){
      gam_plot <- results_gam$plot +
        labs(title = "")
      
      print(gam_plot)
    }
  })
  ###plot_trends_obs_cor####
  
  output$plot_trends_obs_cor <- renderPlot ({
    df_key_1 <- df_key()
    
    trend_type <- "corrected observations"
    
    #### Determine emergence status ####
    if(nrow(df_key_1) > 0){
      
      test_eval_years <- FALSE %in% unique(evaluation_years %in% df_key_1$year)
      
      if(test_eval_years == FALSE){
        results_gam <- apply_gam(
          df = df_key_1,
          y_var = "obs",
          baseline_var = "cobs",
          taxonKey = "taxonKey",
          eval_years = evaluation_years,
          type_indicator = "observations",
          taxon_key = unique(df_key_1$taxonKey),
          name = unique(df_key_1$canonicalName),
          df_title = ""
        )
      }else{
        results_gam <- list(plot = NULL)
      }
    }else{
      results_gam <- "empty"
    }
    
    #### Create plots ####
    ##### ALT_Plot ####
    if(is.null(results_gam$plot)){
      alt_plot <- df_key_1 %>% 
        ggplot(aes(x = year, y = obs)) + 
        ylab("observations") +
        geom_point(stat = "identity") +
        scale_x_continuous(breaks = seq(from = min(df_key_1$year, na.rm = TRUE),
                                        to = max(df_key_1$year, na.rm = TRUE),
                                        by = 5)) 
      
      if(max(df_key_1$obs, na.rm = TRUE) == 1){
        alt_plot <- alt_plot +
          scale_y_continuous(breaks =  seq(from = 0,
                                           to = 2,
                                           by = 1)) +
          annotate("text", x = max(df_key_1$year), y = 2, label = paste0("The ", trend_type, " trend of \n", input$Species_trends, " \n cannot be assessed."),vjust = "inward", hjust = "inward", colour = "red")
      }else{
        alt_plot <- alt_plot +
          annotate("text", x = max(df_key_1$year), y = max(df_key_1$obs), label = paste0("The ", trend_type, " trend of \n", input$Species_trends, " \n cannot be assessed."),vjust = "inward", hjust = "inward", colour = "red")
      }
      
      print(alt_plot)
    }
    
    ##### No Data plot ####
    if("empty" %in% results_gam){
      alt_plot_2 <- df_key_1 %>% 
        ggplot(aes(x = year, y = obs)) + 
        ylab("observations") +
        geom_point(stat = "identity") +
        annotate("text", x = maxjaar, y = 1, label = paste0(input$Species_trends, " \n is not yet present \n in Belgium"),vjust = "inward", hjust = "inward", colour = "red")
      print(alt_plot_2)
    }
    ##### GAM plot ####
    if(!"empty" %in% results_gam & !is.null(results_gam$plot)){
      gam_plot <- results_gam$plot +
        labs(title = "")
      
      print(gam_plot)
    }
  })
  ###plot_trends_occ####
  
  output$plot_trends_occ <- renderPlot ({
    df_key_1 <- df_key()
    
    trend_type <- "occupancy"
    
    #### Determine emergence status ####
    if(nrow(df_key_1) > 0){
      
      test_eval_years <- FALSE %in% unique(evaluation_years %in% df_key_1$year)
      
      if(test_eval_years == FALSE){
        results_gam <- apply_gam(
          df = df_key_1,
          y_var = "ncells",
          taxonKey = "taxonKey",
          eval_years = evaluation_years,
          type_indicator = "occupancy",
          taxon_key = unique(df_key_1$taxonKey),
          name = unique(df_key_1$canonicalName),
          df_title = "",
          y_label = "occupancy (km2)"
        )
      }else{
        results_gam <- list(plot = NULL)
      }
    }else{
      results_gam <- "empty"
    }
    
    #### Create plots ####
    ##### ALT_Plot ####
    if(is.null(results_gam$plot)){
      alt_plot <- df_key_1 %>% 
        ggplot(aes(x = year, y = ncells)) + 
        ylab("occupancy (km2)") +
        geom_point(stat = "identity") +
        scale_x_continuous(breaks = seq(from = min(df_key_1$year, na.rm = TRUE),
                                        to = max(df_key_1$year, na.rm = TRUE),
                                        by = 5)) 
      
      if(max(df_key_1$ncells, na.rm = TRUE) == 1){
        alt_plot <- alt_plot +
          scale_y_continuous(breaks =  seq(from = 0,
                                           to = 2,
                                           by = 1)) +
          annotate("text", x = max(df_key_1$year), y = 2, label = paste0("The ", trend_type, " trend of \n", input$Species_trends, " \n cannot be assessed."),vjust = "inward", hjust = "inward", colour = "red")
      }else{
        alt_plot <- alt_plot +
          annotate("text", x = max(df_key_1$year), y = max(df_key_1$ncells), label = paste0("The ", trend_type, " trend of \n", input$Species_trends, " \n cannot be assessed."),vjust = "inward", hjust = "inward", colour = "red")
      }
      
      print(alt_plot)
    }
    
    ##### No Data plot ####
    if("empty" %in% results_gam){
      alt_plot_2 <- df_key_1 %>% 
        ggplot(aes(x = year, y = ncells)) + 
        ylab("occupancy (km2)") +
        geom_point(stat = "identity") +
        annotate("text", x = maxjaar, y = 1, label = paste0(input$Species_trends, " \n is not yet present \n in Belgium"),vjust = "inward", hjust = "inward", colour = "red")
      print(alt_plot_2)
    }
    ##### GAM plot ####
    if(!"empty" %in% results_gam & !is.null(results_gam$plot)){
      gam_plot <- results_gam$plot +
        labs(title = "")
      
      print(gam_plot)
    }
  })
  ###plot_trends_occ_cor####
  
  output$plot_trends_occ_cor <- renderPlot ({
    df_key_1 <- df_key()
    
    trend_type <- "corrected occupancy"
    
    #### Determine emergence status ####
    if(nrow(df_key_1) > 0){
      
      test_eval_years <- FALSE %in% unique(evaluation_years %in% df_key_1$year)
      
      if(test_eval_years == FALSE){
        results_gam <- apply_gam(
          df = df_key_1,
          y_var = "ncells",
          baseline_var = "c_ncells",
          taxonKey = "taxonKey",
          eval_years = evaluation_years,
          type_indicator = "occupancy",
          taxon_key = unique(df_key_1$taxonKey),
          name = unique(df_key_1$canonicalName),
          df_title = "",
          y_label = "occupancy (km2)"
        )
      }else{
        results_gam <- list(plot = NULL)
      }
    }else{
      results_gam <- "empty"
    }
    
    #### Create plots ####
    ##### ALT_Plot ####
    if(is.null(results_gam$plot)){
      alt_plot <- df_key_1 %>% 
        ggplot(aes(x = year, y = ncells)) + 
        ylab("occupancy (km2)") +
        geom_point(stat = "identity") +
        scale_x_continuous(breaks = seq(from = min(df_key_1$year, na.rm = TRUE),
                                        to = max(df_key_1$year, na.rm = TRUE),
                                        by = 5)) 
      
      if(max(df_key_1$ncells, na.rm = TRUE) == 1){
        alt_plot <- alt_plot +
          scale_y_continuous(breaks =  seq(from = 0,
                                           to = 2,
                                           by = 1)) +
          annotate("text", x = max(df_key_1$year), y = 2, label = paste0("The ", trend_type, " trend of \n", input$Species_trends, " \n cannot be assessed."),vjust = "inward", hjust = "inward", colour = "red")
      }else{
        alt_plot <- alt_plot +
          annotate("text", x = max(df_key_1$year), y = max(df_key_1$ncells), label = paste0("The ", trend_type, " trend of \n", input$Species_trends, " \n cannot be assessed."),vjust = "inward", hjust = "inward", colour = "red")
      }
      
      print(alt_plot)
    }
    
    ##### No Data plot ####
    if("empty" %in% results_gam){
      alt_plot_2 <- df_key_1 %>% 
        ggplot(aes(x = year, y = ncells)) + 
        ylab("occupancy (km2)") +
        geom_point(stat = "identity") +
        annotate("text", x = maxjaar, y = 1, label = paste0(input$Species_trends, " \n is not yet present \n in Belgium"),vjust = "inward", hjust = "inward", colour = "red")
      print(alt_plot_2)
    }
    ##### GAM plot ####
    if(!"empty" %in% results_gam & !is.null(results_gam$plot)){
      gam_plot <- results_gam$plot +
        labs(title = "")
      
      print(gam_plot)
    }
  })
  ##Management####
  ###Summarizing_table####
  output$table_summarizing_management <- renderTable(table_summarizing_management)
  ###Level of invasion####
  ###Level of invasion baseline####
  output$map_level_of_invasion_baseline <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong>",
      level_of_invasion_RBSU_baseline$fullnameRBSU
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorFactor(palette = c("yellow", "orange", "red", "grey"),
                       levels = c( "scattered occurrences only", "weakly invaded", "heavily invaded", NA))
    
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
                values = ~c("scattered occurrences only", "weakly invaded", "heavily invaded", NA),
                pal = pal)
    
    
  })
  
  ###Level_of_invasion_current####
  output$map_level_of_invasion_current <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong>",
      level_of_invasion_RBSU_current$fullnameRBSU
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorFactor(palette = c("yellow", "orange", "red", "grey"),
                       levels = c( "scattered occurrences only", "weakly invaded", "heavily invaded", NA))
    
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
                values = ~c("scattered occurrences only", "weakly invaded", "heavily invaded", NA),
                pal = pal)
    
  })
  
  ###Map_baseline_state####
  
  
  
  output$map_baseline_state  <- renderLeaflet({
    
    baseline_state_sub <- subset(baseline_state,
                                 baseline_state$species %in%
                                   input$Species_loi)
    
    EEA_per_species_baseline_sub <- subset(EEA_per_species_baseline,
                                           EEA_per_species_baseline$species %in%
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
                                current_state$species %in%
                                  input$Species_loi)
    
    EEA_per_species_current_sub <- subset(EEA_per_species_current,
                                          EEA_per_species_current$species %in%
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

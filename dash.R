library(shiny)

# Ensure all libraries are included in the Dockerfile
library(tidyr)
library(stringr)
library(readr)
library(magrittr)
library(dplyr)
library(arrow)
library(sf)
library(leaflet)
library(scales)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
# for plots
library(echarts4r)
# Custom error messages and loading screens
library(sever)
library(waiter)
# Custom shiny theme
library(dashboardthemes)

# ---- Load data ----
source("functions.R")

# Boundaries
lad_shp <- read_sf("data/lad.shp")
msoa_shp <- read_sf("data/msoa.shp")

ri <- read_feather("data/resilience-index.feather")
vi <- read_feather("data/vulnerability-index-msoa-england.feather")

labels <- read_rds("data/la-labels.rds")  # Local Authority labels for map

la_centroids <- read_feather("data/la-centroids.feather")

# ---- Data prep ----
ri_shp <- lad_shp %>% 
  left_join(ri, by = c("lad19cd" = "LAD19CD"))

vi_shp <- msoa_shp %>% 
  left_join(vi, by = c("MSOA11CD" = "Code"))

# ---- UI ----
# https://community.rstudio.com/t/big-box-beside-4-small-boxes-using-shinydashboard/39489
body_colwise <- dashboardBody(
  
  # - Error and waiting functions to improve UX -
  use_sever(),
  use_waiter(),
  waiter_show_on_load(html = tagList(
    spin_5(),
    div(p("Loading Index"), style = "padding-top:25px;")
  )),
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("styles.css")),
  tags$head(HTML("<title>British Red Cross Vulnerability Index and Resilience Index</title>")),
  
  # Load custom theme
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  
  fluidRow(
    column(
      width = 12,
      box(
        width = NULL, height = "960px",
        title = "Local Authority Vulnerability and Resilience",
        leafletOutput("map", height = "890px"),
        
        absolutePanel(
          id = "legend", class = "panel panel-default",
          top = "auto", left = 25, bottom = 0, right = "auto", width = 225, fixed = FALSE,
          draggable = FALSE, height = "auto",
          img(src = "bivar-legend.png", width = 300)
        )
      )
    )
  ) # fluidRow
) # dashboardBody

ui <- function(request) {
  dashboardPage(
    header = dashboardHeader(
      title = "Vulnerability Index", titleWidth = "300px",
      # to add in bookmark button
      tags$li(class = "dropdown", bookmarkButton(), style = "padding-top: 8px; padding-bottom: 8px; padding-right: 15px")
    ),
    sidebar = dashboardSidebar(
      width = "300px",
      
      h3("About", style = "padding-left:10px; padding-right:10px;"),
      
      p(
        style = "text-align: justify; font-size:12px; color:black; padding-left:10px; padding-right:10px;",
        "Hello"
      ),
      
      h3("Instructions", style = "padding-left:10px; padding-right:10px;"),
      
      p(
        style = "text-align: justify; font-size:12px; color:black; padding-left:10px; padding-right:10px;",
        "Use the drop-down boxes below to first select a local authority or primary care network,
        and then select a domain of vulnerability to show on the map."
      ),
      
      h4("1. Select type of resilience", style = "padding-left:10px; padding-right:10px;"),
      
      sidebarMenu(
        id = "sidebar",
        menuItem("Disasters and Emergencies",
                 tabName = "shocks_tab", icon = icon("building"), startExpanded = TRUE,
                 
                 selectInput("shocks",
                             label = "Filter areas by disaster",
                             choices = c("None", "Floods", "Dwelling fires"),
                             selected = "None",
                             multiple = FALSE
                             ),
                 
                 conditionalPanel(
                   condition = "input.shocks == 'Floods'",
                   
                   checkboxInput("highest_flood_risks",
                                 label = "Show only most-affected areas?",
                                 value = TRUE
                   ),
                 
                   checkboxInput("flood_incidents",
                                 label = "Include historical flooding?",
                                 value = FALSE
                                 )
                 )
        ),
        
        menuItem("Health Inequalities",
                 icon = icon("stethoscope"), tabName = "health_tab", # badgeLabel = "new", badgeColor = "green",
                 "Health ineqs"
        ),
        
        menuItem("Migration and Displacement",
                 icon = icon("stethoscope"), tabName = "migration_tab", # badgeLabel = "new", badgeColor = "green",
                 "Migration"
        )
      ),
      
      h4("2. Select area", style = "padding-left:10px; padding-right:10px;"),
      
      selectInput("lad",
                  label = "Choose a Local Authority",
                  choices = c("All", sort(lad_shp$lad19nm)),
                  selected = "All"
                  ),
      
      # h4("2. Select vulnerability domain", style = "padding-left:10px; padding-right:10px;"),
      # 
      # selectInput("vi",
      #             label = "Type of vulnerability",
      #             choices = c("Socioeconomic vulnerability", "Clinical vulnerability", "Overall vulnerability")
      # ),
      br(),
      br(),
      
      div(
        img(src = "brc-logo.jpg", width = 220),
        p(
          style = "font-size:7px; color:black;",
          a(href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target = "_blank", "Contains public sector information licensed under the Open Government Licence v3.0.")
        ),
        style = "position:fixed; bottom:0; padding:10px; text-align: center;")
    ),
    body_colwise
  )
}

# ---- Server ----
server <- function(input, output, session) {
  vi_pal <- colorFactor("viridis", c(1:10), reverse = TRUE)
  
  curr_polygon <- reactiveVal()  # track which LA the user clicked on
  
  # ---- Draw basemap ----
  # set up the static parts of the map (that don't change as user selects different options)
  output$map <- renderLeaflet({
    leaflet(ri_shp,
            options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = F)) %>%
      
      setView(lat = 54.00366, lng = -2.547855, zoom = 7) %>% # centre map on Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # - All LA's -
      addPolygons(
        # Use the layerID to observe click-events and update plots
        layerId = ~lad19nm,
        group = "Vulnerability vs. Resilience",
        fillColor = ~fill,
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>% 
      
      # Add button to reset zoom
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset zoom level",
        onClick = JS("function(btn, map){ map.setZoom(6); }")
      ))
  })
  
  # ---- Map filters ----
  filteredLAs <- reactive({
    output_shp <- ri_shp  # Set up object to return
    
    # - Filter based on type of resilience selected -
    if (input$sidebarItemExpanded == "DisastersandEmergencies") {
      if (input$shocks == "None") {
        output_shp <- ri_shp
        
      } else if (input$shocks == "Floods"){
        
        if (input$highest_flood_risks & !input$flood_incidents) {
          # Show areas with highest flood risks but not historical incidents
          output_shp <- ri_shp %>% filter(`Flood incidents quintile` == 5)
          
        } else if (!input$highest_flood_risks & !input$flood_incidents) {
          # Show areas with any flood risk but not historical incidents
          output_shp <- ri_shp %>% filter(!is.na(`Flood incidents quintile`))
          
        } else if (input$highest_flood_risks & input$flood_incidents) {
          # Show areas with highest floods risk and/or historical incidents
          output_shp <- ri_shp %>% filter(`Flood incidents quintile` == 5 | !is.na(`Total historical flooding incidents`))
          
        } else if (!input$highest_flood_risks & input$flood_incidents) {
          # Show areas with any floods ris and/or historical incidents
          output_shp <- ri_shp %>% filter(!is.na(`Flood incidents quintile`) | !is.na(`Total historical flooding incidents`))
          
        }
        
      } else if (input$shocks == "Dwelling fires") {
        output_shp <- ri_shp %>% filter(`Fire incidents quintile` == 5)
      }
      
    } else if (input$sidebarItemExpanded == "HealthInequalities") {
      output_shp <- ri_shp
      
    } else if (input$sidebarItemExpanded == "MigrationandDisplacement") {
      output_shp <- ri_shp
    }
    
    output_shp
  })
  
  # Track which polygon the user clicked on
  observeEvent(input$map_shape_click, {
    
    if (is.null(input$map_shape_click$id)) {
      curr_polygon(NULL)
      
    } else if (str_detect(input$map_shape_click$id, "^E02")) {
      # User selected an MSOA - do nothing
      return()
    
    } else {
      curr_polygon(input$map_shape_click$id)
      
      # Get name from selected LA code
      curr_lad <- lad_shp$lad19nm[ lad_shp$lad19cd == input$map_shape_click$id ]
      
      # Show clicked LA name in the select box
      updateSelectInput(session, "lad", selected = curr_lad)
    }
  })
  
  # ---- Observer for updating map ----
  observe({
    # Debug
    # print(input$sidebarItemExpanded)
    # print(input$shocks)
    # print(nrow(filteredLAs()))
    # print(curr_polygon())
    # print(input$map_shape_click$id)
    
    # curr_polygon(clicked_polygon())  # Did user click a polygon on the map?
    
    # - Filter based on user-selected LAs from list -
    if (input$lad == "All") {
      # Deselect LAs
      curr_polygon(NULL)
      
    } else {
      # update map zoom
      curr_polygon(lad_shp$lad19cd[ lad_shp$lad19nm == input$lad ])
    }
    
    # print(curr_polygon())
    
    # Get selected set of LAs
    curr_LAs <- filteredLAs()
    
    # Re-create labels, in case user has filtered LAs
    labels <-
      paste0(
        sprintf("<strong>%s</strong><br/>", curr_LAs$lad19nm),
        "Vulnerability quintile: ", curr_LAs$`Vulnerability quintile`, "<br/>",
        "Resilience quintile: ", curr_LAs$`Capacity quintile`
      ) %>%
      lapply(htmltools::HTML)
    
    map <- leafletProxy("map") %>%
      clearShapes() %>%

      # Show filtered Local Authorities
      addPolygons(
        data = curr_LAs,
        # Use the layerID to observe click-events and update plots
        layerId = ~lad19cd,
        group = "Vulnerability vs. Resilience",
        fillColor = ~fill,
        weight = 0.7,
        opacity = 0.8,
        color = "black",
        dashArray = "0.1",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),

        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
    
    # If user clicks a Local Authority, zoom to it and show vulnerable MSOAs
    if (!is.null(curr_polygon())) {
      # Get LA centroid for zooming
      curr_centroid <- la_centroids %>% 
        filter(lad19cd == curr_polygon())
      
      # Get vulnerable MSOAs for current LA
      vi_curr <- vi_shp %>% 
        filter(LAD19CD == curr_polygon())
      
      map <- map %>% 
        addPolygons(
          data = vi_curr,
          group = "VI",
          layerId = ~MSOA11CD,
          fillColor = ~ vi_pal(`Vulnerability decile`), fillOpacity = 0.8, color = "white", weight = 0.7,
          popup = ~ paste(
            "<b>", Name, "</b><br/><br/>",
            "Overall vulnerability (10 = worst): ", `Vulnerability decile`, "<br/>",
            "Clinical vulnerability: ", `Clinical Vulnerability decile`, "<br/>",
            "Health/wellbeing vulnerability: ", `Health/Wellbeing Vulnerability decile`, "<br/>",
            "Socioeconomic vulnerability: ", `Socioeconomic Vulnerability decile`, "<br/>"
          )
        ) %>%
        
        clearControls() %>%
        addLegend_decreasing(
          data = vi_shp,
          position = "bottomright",
          pal = vi_pal,
          values = ~`Vulnerability decile`,
          title = paste0("Vulnerability", tags$br(), " (10 = most vulnerable)"),
          opacity = 0.8,
          decreasing = TRUE
        ) %>% 
        
        setView(lng = curr_centroid$lng, lat = curr_centroid$lat, zoom = 10)
    }
    
    map
  })
  
  
  # - Error messages -
  sever()
  
  # - Waiter -
  waiter_hide()
}

enableBookmarking(store = "url") # saving as url results in a very long url but necessary in this deployment

# Run app ----
shinyApp(ui, server)

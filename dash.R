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
# for plots
library(echarts4r)
# Custom error messages and loading screens
library(sever)
library(waiter)
# Custom shiny theme
library(dashboardthemes)

# ---- Load data ----
# source("functions.R")

ri <- read_feather("data/resilience-index.feather")
lad_shp <- read_sf("data/lad.shp")

ri_shp <- lad_shp %>% 
  left_join(ri, by = c("lad19cd" = "LAD19CD"))

# ---- Prep data ----
labels <-
  paste0(
    sprintf("<strong>%s</strong><br/>", ri_shp$lad19nm),
    "Vulnerability quintile: ", ri_shp$`Vulnerability quintile`, "<br/>",
    "Resilience quintile: ", ri_shp$`Capacity quintile`
  ) %>%
  lapply(htmltools::HTML)

# # ---- UI ----
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
      width = 6,
      box(
        width = NULL, height = "960px",
        title = "Local Authority Vulnerability and Resilience",
        leafletOutput("map", height = "890px"),
        
        absolutePanel(
          id = "legend", class = "panel panel-default",
          top = "auto", left = 25, bottom = 0, right = "auto", width = 225, fixed = TRUE,
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
      
      h4("1. Select area", style = "padding-left:10px; padding-right:10px;"),
      
      sidebarMenu(
        id = "sidebar",
        menuItem("Disasters and Emergencies",
                 tabName = "shocks_tab", icon = icon("building"), startExpanded = TRUE,
                 selectInput("shocks",
                             label = "Filter areas by disaster",
                             choices = c("None", "Flood risk", "Historical flooding", "Dwelling fires"),
                             selected = "None",
                             multiple = TRUE
                 )
        ),
        
        menuItem("Health Inequalities",
                 icon = icon("stethoscope"), tabName = "health_tab", # badgeLabel = "new", badgeColor = "green",
                 "Health ineqs"
        ),
        
        menuItem("Migration and Displacement",
                 icon = icon("stethoscope"), tabName = "pcn", # badgeLabel = "new", badgeColor = "green",
                 "Migration"
        )
      ),
      
      h4("2. Select vulnerability domain", style = "padding-left:10px; padding-right:10px;"),
      
      selectInput("vi",
                  label = "Type of vulnerability",
                  choices = c("Socioeconomic vulnerability", "Clinical vulnerability", "Overall vulnerability")
      ),
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
server <- function(input, output) {
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
      ) %>% 
      
      # Add button to reset zoom
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset zoom level",
        onClick = JS("function(btn, map){ map.setZoom(6); }")
      ))
  })
  
  # - Error messages -
  sever()
  
  # - Waiter -
  waiter_hide()
}

enableBookmarking(store = "url") # saving as url results in a very long url but necessary in this deployment

# Run app ----
shinyApp(ui, server)

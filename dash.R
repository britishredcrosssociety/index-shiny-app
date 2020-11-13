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
library(shinydashboardPlus)
library(shinyWidgets)
library(htmltools)
library(DT)
# for plots
library(echarts4r)
# Custom error messages and loading screens
library(sever)
library(waiter)
# Custom shiny theme
library(dashboardthemes)
library(shinyjs)
library(cicerone)

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

# ---- Guided tour of UI ----
guide <- Cicerone$
  new()$ 
  step(
    el = ".sidebar-menu",
    title = "Choose type of resilience",
    description = "Click the items here to view resilience to disasters and emergencies, health inequalities, or migration and displacement.",
    is_id = FALSE
  )$
  step(
    ".treeview-menu",
    "Choose filters",
    "You can filter some types of resilience. For example, here you can filter Local Authorities with high risks of flooding or fires.",
    is_id = FALSE
  )$
  step(
    ".lad-select",
    "Select Local Authorities",
    "Use this box to choose or search for Local Authorities. You can also click Local Authorities on the map to see more details, include their vulnerable neighbourhoods.",
    is_id = FALSE
  )$
  step(
    ".fa-chart-pie",
    "View vulnerability indicators",
    "After choosing a Local Authority and clicking on a vulnerable neighbourhood, this button will show you the Vulnerability Index indicators.",
    is_id = FALSE
  )$
  step(
    "[data-value='Data']",
    "View the data",
    "Click this tab to see the underlying data rather than a map.",
    is_id = FALSE
  )

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
  
  use_cicerone(),
  useShinyjs(),
  
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
      tabBox(
        width = NULL, height = "960px", side = "right",
        title = "Local Authority Vulnerability and Resilience",
        
        tabPanel("Map", 
                 id = "map-tab",
                 icon = icon(name = "globe"),
                 
                 leafletOutput("map", height = "890px"),
                 
                 absolutePanel(
                   id = "legend", class = "panel panel-default",
                   top = "auto", left = 25, bottom = 0, right = "auto", width = 225, fixed = FALSE,
                   draggable = FALSE, height = "auto",
                   img(src = "bivar-legend.png", width = 300)
                 )
        ),
        
        tabPanel("Data", 
                 icon = icon(name = "table"),
                 
                 fluidRow(
                   id = "data-info",
                   p(
                     "Explore English local authority vulnerability and capacity scores in the table below.
                     Scores are given as ranks, quintiles and deciles - in all cases, higher numbers mean", 
                     tags$b("higher"), " vulnerability and a ", tags$b("lower"),
                     " capacity. To learn more about the British Red Cross Vulnerability Index and how these scores are calculated,",
                     tags$a(href = "https://github.com/britishredcrosssociety/covid-19-vulnerability/", "click here.")
                   ),
                   hr()
                 ),
                 
                 fluidRow(
                   id = "data-infographic-row",
                   img(
                     src = "data-info-download.png",
                     height = 90,
                     style = "margin-left:30px;margin-bottom:-8px;"
                   ),
                   img(
                     src = "data-info-hide-cols.png",
                     height = 90,
                     style = "margin-left:20px;margin-bottom:-15px;"
                   ),
                   img(
                     src = "data-info-find.png",
                     height = 90,
                     style = "position:absolute;top:180px;right:0;margin-right:50px;"
                   )
                 ),
                 fluidRow(
                   id = "data-table-row",
                   DTOutput("data")
                 )
        )
      )
    )
  ) # fluidRow
) # dashboardBody

ui <- function(request) {
  dashboardPagePlus(
    header = dashboardHeaderPlus(
      title = "British Red Cross Vulnerability Index", titleWidth = "350px",
      # to add in bookmark button
      tags$li(class = "dropdown", bookmarkButton(), style = "padding-top: 8px; padding-bottom: 8px; padding-right: 15px"),
      
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "chart-pie"
    ),
    
    # - Main, left-hand sidebar -
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
      
      h4("1. Select type of resilience", id = "h_resilience", style = "padding-left:10px; padding-right:10px;"),
      
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
                 icon = icon("user"), tabName = "migration_tab", # badgeLabel = "new", badgeColor = "green",
                 "Migration"
        )
      ),
      
      h4("2. Select area", style = "padding-left:10px; padding-right:10px;"),
      
      div(class = "lad-select",  # use this <div> for help guide
        selectInput("lad",
                    label = "Choose a Local Authority",
                    choices = c("All", sort(lad_shp$lad19nm)),
                    selected = "All"
                    )
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
        p("Developed by"),
        img(src = "brc-logo.jpg", width = 225),
        p(
          style = "font-size:7px; color:black;",
          a(href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target = "_blank", "Contains public sector information licensed under the Open Government Licence v3.0.")
        ),
        style = "text-align: center;"
        # style = "position:fixed; bottom:0; padding:10px; text-align: center;"
      )
    ),
    
    # - Right-hand sidebar showing VI underlying indicators -
    rightsidebar = rightSidebar(
      # background = "dark",
      rightSidebarTabContent(
        id = 1,
        title = "Clinical Vulnerability",
        icon = "virus",
        
        active = TRUE,
        fluidRow(
          uiOutput("vi_clinical")
        )
      ),
      
      rightSidebarTabContent(
        id = 2,
        title = "Health/Wellbeing Vulnerability",
        icon = "stethoscope",
        
        uiOutput("vi_wellbeing")
      ),
      
      rightSidebarTabContent(
        id = 3,
        title = "Economic Vulnerability",
        icon = "pound-sign",
        
        uiOutput("vi_economic")
      ),
      
      rightSidebarTabContent(
        id = 4,
        title = "Social Vulnerability",
        icon = "users",
        
        uiOutput("vi_social")
      )
    ),
    
    body_colwise
  )
}

# ---- Server ----
server <- function(input, output, session) {
  
  # initialise then start the guide
  guide$init()$start()
  
  # ---- Functions and variables for the server ----
  vi_pal <- colorFactor("viridis", c(1:10), reverse = TRUE)
  
  selected_polygon <- reactiveVal()  # track which LA the user clicked on
  selected_msoa <- reactiveVal()  # track if user clicked an MSOA
  
  cols_to_format <- reactiveVal()  # list of data table columns to format in `renderDT()`
  
  # Set up a waiter for the map
  map_waiter <- Waiter$new(id = "map", color = transparent(.5))
  
  # ---- Draw basemap ----
  # set up the static parts of the map (that don't change as user selects different options)
  output$map <- renderLeaflet({
    leaflet(ri_shp,
            options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = F)) %>%
      
      # setView(lat = 54.00366, lng = -2.547855, zoom = 7) %>% # centre map on Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # - All LA's -
      # addPolygons(
      #   # Use the layerID to observe click-events and update plots
      #   layerId = ~lad19nm,
      #   group = "Vulnerability vs. Resilience",
      #   fillColor = ~fill,
      #   weight = 0.7,
      #   opacity = 0.8,
      #   color = "black",
      #   dashArray = "0.1",
      #   fillOpacity = 0.7,
      #   highlight = highlightOptions(
      #     weight = 5,
      #     color = "#666",
      #     dashArray = "",
      #     fillOpacity = 0.7,
      #     bringToFront = TRUE
      #   ),
      #   
      #   label = labels,
      #   labelOptions = labelOptions(
      #     style = list("font-weight" = "normal", padding = "3px 8px"),
      #     textsize = "15px",
      #     direction = "auto"
      #   )
      # ) %>% 
      
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
      selected_polygon(NULL)
      
      # Hide the right-hand sidebar with VI indicators (if it was open)
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
      
    } else if (str_detect(input$map_shape_click$id, "^E02")) {
      # User selected an MSOA - do nothing
      # return()
      selected_msoa(input$map_shape_click$id)
      
      # Show the right-hand sidebar with VI indicators
      shinyjs::addClass(selector = "body", class = "control-sidebar-open")
    
    } else {
      selected_polygon(input$map_shape_click$id)
      
      # Get name from selected LA code
      curr_lad <- lad_shp$lad19nm[ lad_shp$lad19cd == input$map_shape_click$id ]
      
      # Show clicked LA name in the select box
      updateSelectInput(session, "lad", selected = curr_lad)
      
      # Hide the right-hand sidebar with VI indicators (if it was open)
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
    }
  })
  
  # ---- Reset view if user changes disasters/shocks ----
  observeEvent(input$shocks, {
    updateSelectInput(session, "lad", selected = "All")
  })
  
  observeEvent(input$highest_flood_risks, {
    updateSelectInput(session, "lad", selected = "All")
  })
  
  observeEvent(input$flood_incidents, {
    updateSelectInput(session, "lad", selected = "All")
  })
  
  # ---- Observer for updating map ----
  observe({
    # Debug
    # print(input$sidebarItemExpanded)
    # print(input$shocks)
    # print(nrow(filteredLAs()))
    # print(selected_polygon())
    # print(input$map_shape_click$id)
    
    # selected_polygon(clicked_polygon())  # Did user click a polygon on the map?
    
    map_waiter$show()
    
    # - Filter based on user-selected LAs from list -
    if (input$lad == "All") {
      # Deselect LAs
      selected_polygon(NULL)
      
    } else {
      # update map zoom
      selected_polygon(lad_shp$lad19cd[ lad_shp$lad19nm == input$lad ])
    }
    
    # print(selected_polygon())
    
    # Get selected set of LAs
    curr_LAs <- filteredLAs()
    
    # updateSelectInput(session, "lad", choices = c("All", sort(curr_LAs$lad19nm)), selected = input$lad)
    
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
      clearControls() %>%

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
    if (!is.null(selected_polygon())) {
      # Get LA centroid for zooming
      curr_centroid <- la_centroids %>% 
        filter(lad19cd == selected_polygon())
      
      # Get vulnerable MSOAs for current LA
      vi_curr <- vi_shp %>% 
        filter(LAD19CD == selected_polygon())
      
      # Get bounding box of current LA
      curr_bbox <- st_bbox(vi_curr)
      
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
        
        addLegend_decreasing(
          data = vi_shp,
          position = "bottomright",
          pal = vi_pal,
          values = ~`Vulnerability decile`,
          title = paste0("Vulnerability", tags$br(), " (10 = most vulnerable)"),
          opacity = 0.8,
          decreasing = TRUE
        ) %>% 
        
        flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]), 
                    lat1 = as.numeric(curr_bbox["ymin"]), 
                    lng2 = as.numeric(curr_bbox["xmax"]), 
                    lat2 = as.numeric(curr_bbox["ymax"]))
    
    } else {
      # Get bounding box of current LAs and zoom to it
      curr_bbox <- st_bbox(curr_LAs)
      
      map <- map %>% 
        flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]), 
                    lat1 = as.numeric(curr_bbox["ymin"]), 
                    lng2 = as.numeric(curr_bbox["xmax"]), 
                    lat2 = as.numeric(curr_bbox["ymax"]))
    }
    
    map_waiter$hide()
    
    map
  })
  
  # ---- Data ----
  filteredData <- reactive({
    # ri_tmp <- ri %>% 
    #   mutate(`% people in highly vulnerable areas` = paste0(round(`Extent of population living in highly vulnerable areas` * 100, 1), "%"))
    
    # - Filter based on type of resilience selected -
    if (input$sidebarItemExpanded == "DisastersandEmergencies") {
      if (input$shocks == "None") {
        cols_to_format(c("Extent of population living in highly vulnerable areas"))
        
        ri %>% 
          select(`LA code` = LAD19CD, `LA name` = LAD19NM, 
                 `Capacity rank`, `Capacity quintile`, 
                 `Vulnerability rank`, `Vulnerability quintile`, 
                 `Extent of population living in highly vulnerable areas`)
        
      } else if (input$shocks == "Floods"){
        
        if (input$highest_flood_risks & !input$flood_incidents) {
          cols_to_format(c("Extent of population living in highly vulnerable areas", "% people in flood risk areas"))
          
          # Show areas with highest flood risks but not historical incidents
          ri %>% 
            filter(`Flood incidents quintile` == 5) %>%
            select(`LA code` = LAD19CD, `LA name` = LAD19NM, 
                   `Capacity rank`, `Capacity quintile`, 
                   `Vulnerability rank`, `Vulnerability quintile`, 
                   `Extent of population living in highly vulnerable areas`, 
                   `Flood risk quintile`, `% people in flood risk areas`)
          
        } else if (!input$highest_flood_risks & !input$flood_incidents) {
          cols_to_format(c("Extent of population living in highly vulnerable areas", "% people in flood risk areas"))
          
          # Show areas with any flood risk but not historical incidents
          ri %>% 
            filter(!is.na(`Flood incidents quintile`)) %>% 
            select(`LA code` = LAD19CD, `LA name` = LAD19NM, 
                   `Capacity rank`, `Capacity quintile`, 
                   `Vulnerability rank`, `Vulnerability quintile`, 
                   `Extent of population living in highly vulnerable areas`, 
                   `Flood risk quintile`, `% people in flood risk areas`)
          
        } else if (input$highest_flood_risks & input$flood_incidents) {
          cols_to_format(c("Extent of population living in highly vulnerable areas", "% people in flood risk areas"))
          
          # Show areas with highest floods risk and/or historical incidents
          ri %>% 
            filter(`Flood incidents quintile` == 5 | !is.na(`Total historical flooding incidents`)) %>% 
            mutate(`Flooding incidents per 10,000 people` = round(`Flooding incidents per 10,000 people`, 1), 
                   `Total historical flooding incidents` = round(`Total historical flooding incidents`, 1)) %>% 
            select(`LA code` = LAD19CD, `LA name` = LAD19NM, 
                   `Capacity rank`, `Capacity quintile`, 
                   `Vulnerability rank`, `Vulnerability quintile`, 
                   `Extent of population living in highly vulnerable areas`, 
                   `Flood risk quintile`, `% people in flood risk areas`,
                   `Flooding incidents per 10,000 people`, `Total historical flooding incidents`)
          
        } else if (!input$highest_flood_risks & input$flood_incidents) {
          cols_to_format(c("Extent of population living in highly vulnerable areas", "% people in flood risk areas"))
          
          # Show areas with any floods ris and/or historical incidents
          ri %>% 
            filter(!is.na(`Flood incidents quintile`) | !is.na(`Total historical flooding incidents`)) %>% 
            mutate(`Flooding incidents per 10,000 people` = round(`Flooding incidents per 10,000 people`, 1), 
                   `Total historical flooding incidents` = round(`Total historical flooding incidents`, 1)) %>% 
            select(`LA code` = LAD19CD, `LA name` = LAD19NM, 
                   `Capacity rank`, `Capacity quintile`, 
                   `Vulnerability rank`, `Vulnerability quintile`, 
                   `Extent of population living in highly vulnerable areas`, 
                   `Flood risk quintile`, `% people in flood risk areas`,
                   `Flooding incidents per 10,000 people`, `Total historical flooding incidents`)
          
        } # end if for flooding
        
      } else if (input$shocks == "Dwelling fires") {
        cols_to_format(c("Extent of population living in highly vulnerable areas"))
        
        ri %>%
          filter(`Fire incidents quintile` == 5) %>% 
          mutate(`Dwelling fire incidents per 10,000 people` = round(`Dwelling fire incidents per 10,000 people`, 1), 
                 `Total dwelling fires (three-year average)` = round(`Total dwelling fires (three-year average)`, 1)) %>% 
          select(`LA code` = LAD19CD, `LA name` = LAD19NM, 
                 `Capacity rank`, `Capacity quintile`, 
                 `Vulnerability rank`, `Vulnerability quintile`, 
                 `Extent of population living in highly vulnerable areas`, 
                 `Fire incidents quintile`, 
                 `Dwelling fire incidents per 10,000 people`, `Total dwelling fires (three-year average)`)
        
      } # end if for shocks
      
    } else if (input$sidebarItemExpanded == "HealthInequalities") {
      cols_to_format(c("Extent of population living in highly vulnerable areas"))
      
      ri %>% 
        select(`LA code` = LAD19CD, `LA name` = LAD19NM, 
               `Capacity rank`, `Capacity quintile`, 
               `Vulnerability rank`, `Vulnerability quintile`, 
               `Extent of population living in highly vulnerable areas`)
      
    } else if (input$sidebarItemExpanded == "MigrationandDisplacement") {
      cols_to_format(c("Extent of population living in highly vulnerable areas"))
      
      ri %>% 
        select(`LA code` = LAD19CD, `LA name` = LAD19NM, 
               `Capacity rank`, `Capacity quintile`, 
               `Vulnerability rank`, `Vulnerability quintile`, 
               `Extent of population living in highly vulnerable areas`)
    }
  })
  
  output$data <- renderDT(
    datatable(filteredData(),
              rownames = FALSE,
              escape = FALSE,
              extensions = c("Buttons", "ColReorder"),
              options = list(
                dom = "Bfrtip",
                buttons = c("csv", "excel", "colvis"),
                colReorder = TRUE,
                scrollX = TRUE
              )
    ) %>% 
      formatPercentage(columns = cols_to_format(), digits = 1)
  )
  
  # ---- Vulnerability Index underlying indicators ----
  # Clinical vulnerability
  output$vi_clinical = renderUI({
    if (is.null(selected_msoa()))
      return("Select a Local Authority then click a neighbourhood to see the underlying indicators.")
    
    # str_stats = c()  # the string to build in this function

    # Get vulnerable MSOAs for current LA
    vi_curr <- vi %>% 
      # filter(Code == "E02000001")
      filter(Code == selected_msoa())
    
    str_stats = paste0("<strong>", vi_curr$Name, "</strong>")

    if (!is.na(vi_curr$`Modelled prevalence of people aged 15 who are regular smokers Rate`))
        str_stats = c(str_stats, paste0("Smoking prevalence: ", round(vi_curr$`Modelled prevalence of people aged 15 who are regular smokers Rate`, 2), "<br/>(England average: ", round(mean(vi$`Modelled prevalence of people aged 15 who are regular smokers Rate`, na.rm = TRUE), 2), ")"))

    if (!is.na(vi_curr$`Obesity prevalence Rate`))
        str_stats = c(str_stats, paste0("Obesity prevalence: ", round(vi_curr$`Obesity prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Obesity prevalence Rate`, na.rm = TRUE), 2), ")"))

    if (!is.na(vi_curr$`Cancer prevalence Rate`))
        str_stats = c(str_stats, paste0("Cancer prevalence: ", round(vi_curr$`Cancer prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Cancer prevalence Rate`, na.rm = TRUE), 2), ")"))

    if (!is.na(vi_curr$`Asthma prevalence Rate`))
        str_stats = c(str_stats, paste0("Asthma prevalence: ", round(vi_curr$`Asthma prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Asthma prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Atrial Fibrillation prevalence Rate`))
      str_stats = c(str_stats, paste0("Atrial Fibrillation prevalence: ", round(vi_curr$`Atrial Fibrillation prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Atrial Fibrillation prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Cardiovascular Disease prevalence Rate`))
      str_stats = c(str_stats, paste0("Cardiovascular Disease prevalence: ", round(vi_curr$`Cardiovascular Disease prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Cardiovascular Disease prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`COPD prevalence Rate`))
      str_stats = c(str_stats, paste0("COPD prevalence: ", round(vi_curr$`COPD prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`COPD prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Diabetes prevalence Rate`))
      str_stats = c(str_stats, paste0("Diabetes prevalence: ", round(vi_curr$`Diabetes prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Diabetes prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Coronary Heart Disease prevalence Rate`))
      str_stats = c(str_stats, paste0("Coronary Heart Disease prevalence: ", round(vi_curr$`Coronary Heart Disease prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Coronary Heart Disease prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Heart Failure prevalence Rate`))
      str_stats = c(str_stats, paste0("Heart Failure prevalence: ", round(vi_curr$`Heart Failure prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Heart Failure prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`High Blood Pressure prevalence Rate`))
      str_stats = c(str_stats, paste0("High Blood Pressure prevalence: ", round(vi_curr$`High Blood Pressure prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`High Blood Pressure prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Chronic Kidney Disease prevalence Rate`))
      str_stats = c(str_stats, paste0("Chronic Kidney Disease prevalence: ", round(vi_curr$`Chronic Kidney Disease prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Chronic Kidney Disease prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Peripheral Arterial Disease prevalence Rate`))
      str_stats = c(str_stats, paste0("Peripheral Arterial Disease prevalence: ", round(vi_curr$`Peripheral Arterial Disease prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Peripheral Arterial Disease prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Proportion people over 70`))
      str_stats = c(str_stats, paste0("Proportion of people over 70: ", round(vi_curr$`Proportion people over 70`, 2), "<br/>(England average: ", round(mean(vi$`Proportion people over 70`, na.rm = TRUE), 2), ")"))
    
    HTML(paste(str_stats, collapse = "<br/><br/>"))
  })
  
  # Health/wellbeing vulnerability
  output$vi_wellbeing = renderUI({
    if (is.null(selected_msoa()))
      return("Select a Local Authority then click a neighbourhood to see the underlying indicators.")
    
    # Get vulnerable MSOAs for current LA
    vi_curr <- vi %>% 
      filter(Code == selected_msoa())
    
    str_stats = paste0("<strong>", vi_curr$Name, "</strong>")
    
    if (!is.na(vi_curr$`Dementia prevalence Rate`))
      str_stats = c(str_stats, paste0("Dementia prevalence: ", round(vi_curr$`Dementia prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Dementia prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Learning Disabilities prevalence Rate`))
      str_stats = c(str_stats, paste0("Learning Disabilities prevalence: ", round(vi_curr$`Learning Disabilities prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Learning Disabilities prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Serious Mental Illness prevalence Rate`))
      str_stats = c(str_stats, paste0("Serious Mental Illness prevalence: ", round(vi_curr$`Serious Mental Illness prevalence Rate`, 2), "<br/>(England average: ", round(mean(vi$`Serious Mental Illness prevalence Rate`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$Loneliness))
      str_stats = c(str_stats, paste0("Loneliness score: ", round(vi_curr$Loneliness, 2), "<br/>(England average: ", round(mean(vi$Loneliness, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Frailty rank`))
      str_stats = c(str_stats, paste0("Frailty (rank out of ", max(vi$`Frailty rank`), "): ", round(vi_curr$`Frailty rank`, 2)))
    
    if (!is.na(vi_curr$`Percentage of adresses with private outdoor space (reverse ranked)`))
      str_stats = c(str_stats, paste0("Percentage of adresses with private outdoor space (rank out of ", max(vi$`Percentage of adresses with private outdoor space (reverse ranked)`), "): ", round(vi_curr$`Percentage of adresses with private outdoor space (reverse ranked)`, 2)))
    
    if (!is.na(vi_curr$`Average distance to nearest Park, Public Garden, or Playing Field (m)`))
      str_stats = c(str_stats, paste0("Average distance to green spaces: ", round(vi_curr$`Average distance to nearest Park, Public Garden, or Playing Field (m)`, 2), "m<br/>(England average: ", round(mean(vi$`Average distance to nearest Park, Public Garden, or Playing Field (m)`, na.rm = TRUE), 2), "m)"))

    HTML(paste(str_stats, collapse = "<br/><br/>"))
  })

  # Economic vulnerability
  output$vi_economic = renderUI({
    if (is.null(selected_msoa()))
      return("Select a Local Authority then click a neighbourhood to see the underlying indicators.")
    
    # Get vulnerable MSOAs for current LA
    vi_curr <- vi %>% 
      filter(Code == selected_msoa())
    
    str_stats = paste0("<strong>", vi_curr$Name, "</strong>")
    
    if (!is.na(vi_curr$`Older people social care benefit (Attendance Allowance) Rate`))
      str_stats = c(str_stats, paste0("Older people social care benefit: ", round(vi_curr$`Older people social care benefit (Attendance Allowance) Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Older people social care benefit (Attendance Allowance) Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Employment and Support Allowance claimants, disease code nervous system Rate`))
      str_stats = c(str_stats, paste0("ESA claimants - disease code nervous system: ", round(vi_curr$`Employment and Support Allowance claimants, disease code nervous system Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Employment and Support Allowance claimants, disease code nervous system Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Employment and Support Allowance claimants, disease code respiratory or circulatory Rate`))
      str_stats = c(str_stats, paste0("ESA claimants - disease code respiratory or circulatory: ", round(vi_curr$`Employment and Support Allowance claimants, disease code respiratory or circulatory Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Employment and Support Allowance claimants, disease code respiratory or circulatory Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`People receiving Disability Benefits Rate`))
      str_stats = c(str_stats, paste0("People receiving Disability Benefits: ", round(vi_curr$`People receiving Disability Benefits Rate`, 2), "%<br/>(England average: ", round(mean(vi$`People receiving Disability Benefits Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Personal Independence Payment (PIP), respiratory disease claimants Rate`))
      str_stats = c(str_stats, paste0("PIP claimants: ", round(vi_curr$`Personal Independence Payment (PIP), respiratory disease claimants Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Personal Independence Payment (PIP), respiratory disease claimants Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Households on Universal Credit - Limited Capability for Work Entitlement Rate`))
      str_stats = c(str_stats, paste0("Households on Universal Credit - Limited Capability for Work Entitlement: ", round(vi_curr$`Households on Universal Credit - Limited Capability for Work Entitlement Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Households on Universal Credit - Limited Capability for Work Entitlement Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Universal Credit claimants - Conditionality Regime: No work requirements Rate`))
      str_stats = c(str_stats, paste0("Universal Credit claimants - No work requirements: ", round(vi_curr$`Universal Credit claimants - Conditionality Regime: No work requirements Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Universal Credit claimants - Conditionality Regime: No work requirements Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Jobs in accommodation and food services (hospitality) Rate`))
      str_stats = c(str_stats, paste0("Jobs in accommodation and hospitality: ", round(vi_curr$`Jobs in accommodation and food services (hospitality) Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Jobs in accommodation and food services (hospitality) Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Jobs in arts, entertainment, recreation and other services Rate`))
      str_stats = c(str_stats, paste0("Jobs in arts, entertainment, recreation: ", round(vi_curr$`Jobs in arts, entertainment, recreation and other services Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Jobs in arts, entertainment, recreation and other services Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Jobs in retail Rate`))
      str_stats = c(str_stats, paste0("Jobs in retail: ", round(vi_curr$`Jobs in retail Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Jobs in retail Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Jobs in transport and storage (inc postal) Rate`))
      str_stats = c(str_stats, paste0("Jobs in transport and storage: ", round(vi_curr$`Jobs in transport and storage (inc postal) Rate`, 2), "%<br/>(England average: ", round(mean(vi$`Jobs in transport and storage (inc postal) Rate`, na.rm = TRUE), 2), "%)"))
    
    if (!is.na(vi_curr$`Financial Vulnerability score`))
      str_stats = c(str_stats, paste0("Financial Vulnerability score: ", round(vi_curr$`Financial Vulnerability score`, 2), "<br/>(England average: ", round(mean(vi$`Financial Vulnerability score`, na.rm = TRUE), 2), ")"))
    
    HTML(paste(str_stats, collapse = "<br/><br/>"))
  })
  
  # Social vulnerability
  output$vi_social = renderUI({
    if (is.null(selected_msoa()))
      return("Select a Local Authority then click a neighbourhood to see the underlying indicators.")
    
    # Get vulnerable MSOAs for current LA
    vi_curr <- vi %>% 
      filter(Code == selected_msoa())
    
    str_stats = paste0("<strong>", vi_curr$Name, "</strong>")
    
    if (!is.na(vi_curr$`Longest distance to supermarket (km)`))
      str_stats = c(str_stats, paste0("Distance to supermarket: ", round(vi_curr$`Longest distance to supermarket (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to supermarket (km)`, na.rm = TRUE), 2), "km)"))

    if (!is.na(vi_curr$`Longest distance to GP surgery (km)`))
      str_stats = c(str_stats, paste0("Distance to GP surgery: ", round(vi_curr$`Longest distance to GP surgery (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to GP surgery (km)`, na.rm = TRUE), 2), "km)"))
    
    if (!is.na(vi_curr$`Longest distance to Post Office (km)`))
      str_stats = c(str_stats, paste0("Distance to Post Office: ", round(vi_curr$`Longest distance to Post Office (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to Post Office (km)`, na.rm = TRUE), 2), "km)"))
    
    if (!is.na(vi_curr$`Longest distance to hospital (km)`))
      str_stats = c(str_stats, paste0("Distance to hospital: ", round(vi_curr$`Longest distance to hospital (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to hospital (km)`, na.rm = TRUE), 2), "km)"))
    
    if (!is.na(vi_curr$`Longest distance to food bank (km)`))
      str_stats = c(str_stats, paste0("Distance to food bank: ", round(vi_curr$`Longest distance to food bank (km)`, 2), "km<br/>(England average: ", round(mean(vi$`Longest distance to food bank (km)`, na.rm = TRUE), 2), "km)"))
    
    if (!is.na(vi_curr$`Household overcrowding`))
      str_stats = c(str_stats, paste0("Household overcrowding: ", round(vi_curr$`Household overcrowding`, 2), "<br/>(England average: ", round(mean(vi$`Household overcrowding`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$Homelessness))
      str_stats = c(str_stats, paste0("Homelessness rate: ", round(vi_curr$Homelessness, 2), "<br/>(England average: ", round(mean(vi$Homelessness, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Housing in poor condition`))
      str_stats = c(str_stats, paste0("Housing in poor condition: ", round(vi_curr$`Housing in poor condition`, 2), "<br/>(England average: ", round(mean(vi$`Housing in poor condition`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Houses without central heating`))
      str_stats = c(str_stats, paste0("Houses without central heating: ", round(vi_curr$`Houses without central heating`, 2), "<br/>(England average: ", round(mean(vi$`Houses without central heating`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Air quality`))
      str_stats = c(str_stats, paste0("Air quality: ", round(vi_curr$`Air quality`, 2), "<br/>(England average: ", round(mean(vi$`Air quality`, na.rm = TRUE), 2), ")"))
    
    if (!is.na(vi_curr$`Digital Vulnerability rank`))
      str_stats = c(str_stats, paste0("Digital Vulnerability (rank out of ", max(vi$`Digital Vulnerability rank`), "): ", round(vi_curr$`Digital Vulnerability rank`, 2)))
    
    HTML(paste(str_stats, collapse = "<br/><br/>"))
  })
  
  # - Error messages -
  sever()
  
  # - Waiter -
  waiter_hide()
}

enableBookmarking(store = "url") # saving as url results in a very long url but necessary in this deployment

# Run app ----
shinyApp(ui, server)

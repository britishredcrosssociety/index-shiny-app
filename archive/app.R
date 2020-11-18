# ---- Load libraries ----
library(shiny)
library(sf)
library(leaflet)
library(stringr)
library(dplyr)
library(DT)
library(sever)
library(waiter)
library(echarts4r)

# ---- Load data ----
la_shp_vul_res <-
  read_sf("data/la_shp_vul_res.geojson")

ethnicity <-
  readRDS("data/la-ethnicity.rds")

population <-
  readRDS("data/la-population.rds")

shielding <-
  readRDS("data/la-shielding.rds")

covid <-
  readRDS("data/la-covid-stats.rds")

data <-
  readRDS("data/data.rds")

# ---- Prep data ----
labels <-
  paste0(
    sprintf("<strong>%s</strong><br/>", la_shp_vul_res$la_name),
    "Vulnerability score: ", la_shp_vul_res$vulnerability, "<br/>",
    "Resilience score: ", la_shp_vul_res$resilience
  ) %>%
  lapply(htmltools::HTML)

# ---- Shiny ui -----
ui <-

  navbarPage(

    # title = div(
    #   h3("VCSEP",
    #     style = "position:absolute;left:75px;margin-top:-2px;"
    #   ),
    #   img(
    #     src = "vcs-vec.png",
    #     style = "margin-top:-20px;padding-right:90px;padding-bottom:5px;padding-top:5px;",
    #     height = 60
    #   )
    # ),
    #
    # windowTitle = "VCSEP Dashboard",
    title = div(
      img(
        src = "brc-vec.png",
        style = "margin-top:-20px;padding-right:0;padding-bottom:10px;padding-top:10px;",
        height = 60
      ),
      img(
        src = "covid-19-vulnerability-logo.png",
        style = "position:absolute;top:0;right:0;padding-bottom:15px;padding-top:10px;padding-right:15px",
        height = 60
      )
    ),

    windowTitle = "COVID-19 Vulnerability",

    tabPanel(
      title = "Map",

      icon = icon(name = "map-marked-alt"),

      # - Error and waiting functions to improve UX -
      use_sever(),
      use_waiter(),
      waiter_show_on_load(html = tagList(
        spin_5(),
        div(p("Calculating COVID-19 Vulnerabilties"), style = "padding-top:25px;")
      )),

      div(
        class = "outer",

        tags$head(
          # Include our custom CSS
          includeCSS("styles.css")
        ),

        leafletOutput("map", width = "100%", height = "100%"),

        absolutePanel(
          id = "legend", class = "panel panel-default",
          top = "auto", left = 25, bottom = 0, right = "auto", width = 225, fixed = TRUE,
          draggable = FALSE, height = "auto",
          img(src = "bivar-legend.png", width = 300)
        ),

        absolutePanel(
          id = "click-area", class = "panel panel-default",
          top = 250, left = "auto", bottom = "auto", right = 425, width = 300, fixed = TRUE,
          draggable = FALSE, height = "auto",
          img(src = "click-area.png", width = 300)
        ),

        absolutePanel(
          id = "controls", class = "panel panel-default",
          top = 75, left = "auto", bottom = "auto", right = 20, width = 425, fixed = TRUE,
          draggable = FALSE, height = "auto",
          p(
            h2(textOutput("name_la")),
            tags$br()
          ),

          # COVID19 plot
          echarts4rOutput("covid_plot", width = 400, height = 300),

          # Ethnicity plot
          echarts4rOutput("ethnicity_plot", width = 400, height = 300),

          fluidRow(
            column(
              6,
              # Shielding plot
              echarts4rOutput("shielding_plot", width = 200, height = 250)
            ),
            column(
              6,
              # Population plot
              echarts4rOutput("population_plot", width = 200, height = 250)
            )
          )
        )
      )
    ),

    tabPanel(
      title = "Data",

      icon = icon(name = "table"),

      fluidRow(
        id = "data-info",
        p(
          "Explore English local authority vulnerability and resilience scores in the table below.
          Vulnerability scores are out of five (quintiles) and resilience scores are out of 10 (deciles),
          with the exception of the VCS score which is out of three (terciles). Higher scores (i.e., larger numbers)
          indicate a ", tags$b("higher"), " vulnerability and a ", tags$b("lower"),
          " resilience respectively. To learn more about the
          COVID-19 Vulnerability Index and how these scores are calculated,",
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

# ---- Shiny server ----
server <-
  function(input, output, session) {

    # - Map -
    output$map <- renderLeaflet({
      leaflet(la_shp_vul_res) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-2, 53, zoom = 7) %>%

        # - All LA's -
        addPolygons(
          # Use the layerID to observe click-events and update plots
          layerId = ~la_code,
          group = "Bivariate: Vulnerability vs. Resilience",
          fillColor = la_shp_vul_res$fill,
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
    })

    # - Update plots upon click -
    # Track clicks
    clicked_polygon <- reactive({
      # Set default to Tower Hamlets
      if (is.null(input$map_shape_click$id)) {
        return("E09000030")
      }
      input$map_shape_click$id
    })

    # LA Name
    output$name_la <- renderText({
      la_shp_vul_res %>%
        filter(la_code == clicked_polygon()) %>%
        pull(la_name)
    })

    # COVID-19 case plot
    output$covid_plot <- renderEcharts4r({
      covid %>%
        rename(
          `Current LA` = cases_per_100000_week,
          `England Mean` = week_mean
        ) %>%
        filter(la_code == clicked_polygon()) %>%
        e_chart(x = week) %>%
        e_line(serie = `Current LA`, smooth = TRUE) %>%
        e_line(serie = `England Mean`, smooth = TRUE) %>%
        e_title("COVID-19 Infections", "Cases per 100,000") %>%
        e_theme("infographic") %>%
        e_legend(right = 0) %>%
        e_tooltip(trigger = "axis") %>%
        e_color(c("#ee2a24", "#651713"))
    })

    # Ethnicity plot
    output$ethnicity_plot <- renderEcharts4r({
      ethnicity %>%
        filter(la_code == clicked_polygon()) %>%
        e_charts(ethnicity) %>%
        e_bar(count) %>%
        e_title("Ethnicity", "Population counts") %>%
        e_theme("infographic") %>%
        e_legend(show = FALSE) %>%
        e_tooltip(trigger = "axis") %>%
        e_flip_coords() %>%
        e_grid(containLabel = TRUE) %>%
        e_color("#ee2a24")
    })

    # Shielding plot
    output$shielding_plot <- renderEcharts4r({
      liquid_value <-
        shielding %>%
        filter(la_code == clicked_polygon()) %>%
        pull(`Clinically extremely vulnerable`)

      data.frame(
        name = c(liquid_value, 1),
        color = c("#e95351", "#e95351")
      ) %>%
        e_charts() %>%
        e_liquid(
          serie = name,
          color = color,
          outline = list(show = FALSE),
          label = list(
            # insideColor = "black",
            formatter = "{c}",
            fontSize = 18
          ),
          backgroundStyle = list(
            borderColor = "black",
            borderWidth = 3,
            color = "pink"
          ),
          itemStyle = list(opacity = .4),
          shape = "roundRect",
          waveAnimation = FALSE,
          animationDuration = 0,
          animationDurationUpdate = 0,
          amplitude = 0
        ) %>%
        e_title("Shielding", "No. of clinically vulnerable") %>%
        e_theme("infographic")
    })

    # Population plot
    output$population_plot <- renderEcharts4r({
      liquid_value <-
        population %>%
        filter(la_code == clicked_polygon()) %>%
        pull(population)

      data.frame(
        name = c(liquid_value, 1),
        color = c("#ee2a24", "#ee2a24")
      ) %>%
        e_charts() %>%
        e_liquid(
          serie = name,
          color = color,
          outline = list(show = FALSE),
          label = list(
            # insideColor = "black",
            formatter = "{c}",
            fontSize = 18
          ),
          backgroundStyle = list(
            borderColor = "black",
            borderWidth = 3,
            color = "pink"
          ),
          itemStyle = list(opacity = .6),
          shape = "roundRect",
          waveAnimation = FALSE,
          animationDuration = 0,
          animationDurationUpdate = 0,
          amplitude = 0
        ) %>%
        e_title("Population", "Local authority count") %>%
        e_theme("infographic")
    })

    # - Data -
    output$data <- renderDT(
      datatable(data,
        rownames = FALSE,
        escape = FALSE,
        extensions = c("Buttons", "ColReorder"),
        options = list(
          dom = "Bfrtip",
          buttons = c("csv", "excel", "colvis"),
          colReorder = TRUE
        )
      )
    )

    # - Error messages -
    sever()

    # - Waiter -
    waiter_hide()
  }

# ---- Run app ----
shinyApp(ui = ui, server = server)

# TODO: - add shiny loading widgets - Jon Coene
#       - Use cicerone to create a guide - https://cicerone.john-coene.com/

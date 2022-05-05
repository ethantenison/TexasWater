#' map UI Function
#'
#' @description A shiny Module.
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
#' @importFrom shiny NS tagList 
#' @import leaflet
#' @import sf
#' @import RColorBrewer
#' @import tidygeocoder
#' @import shinyWidgets
#' @import MetBrewer
mod_map_ui <- function(id, height) {
  ns <- NS(id)
  tagList(leafletOutput(ns("map"), height = height),
          absolutePanel(top = -75, right = 20,left = 5, bottom = "auto",
                        fluidRow(
                          column(width = 2),
                          column(
                            width = 10,
                            style = "margin: 7px 0 0 0;",
                            div(
                              style = "float:right; font-size: 16px;",
                              prettyRadioButtons(
                                inputId = ns("search_control"),
                                choices = c("County", "Organization"),
                                selected = "County",
                                label = "Search By:",
                                width = "100%",
                                animation = "jelly",
                                inline = TRUE
                              ),
                              div(
                                style = "margin: -5px",
                              br()),
                              selectizeInput(
                                ns("search"),
                                label = NULL,
                                choices = "",
                                multiple = FALSE,
                                selected = character(0),
                                width = "100%",
                                options = list(allowEmptyOption = FALSE,
                                               placeholder = "Search...")
                              ),
                              # div(
                              #   style = "margin-top:-15px;",
                              #   searchInput(
                              #     ns("search_bar"),
                              #     label = "",
                              #     placeholder = "Enter Address...",
                              #     width = "200px",
                              #     btnSearch = icon("search"),
                              #     btnReset = icon("remove")
                              #   )
                              # )
                            ),
                          )
                        )))
}
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id,data,geo, county) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Admin Area selection -----------------------------------------------------
    
    counties_shape <- readRDS("./data/counties.rds")
    county_lonlat <- read_csv("data/county_lonlat.csv")
    
    # reactive to select geographic data
    map_data <- reactive({
      if (geo() == "River Basins") {
        readRDS("./data/rb.rds")
      } else if (geo() == "GCDs") {
        readRDS("./data/gcd.rds")
      } else if (geo() == "Rivers") {
        readRDS("./data/riv.rds")
      } else if (geo() == "Aquifers") {
        readRDS("./data/aqu.rds")
      } else if (geo() == "River Basins") {
        readRDS("./data/rb.rds")
      } else if (geo() == "RWPGs") {
        readRDS("./data/rwpa.rds")
      } else if (geo() == "Counties") {
        readRDS("./data/counties.rds")
      } else if (geo() == "RFPGs") {
        readRDS("./data/fg.rds")
      }
    })
    # Search Controls -----------------------------------------------------
    # populate the selectizeInput choices
    observe({
      current_selected <- isolate(input$search)
      if (input$search_control == "Organization") {
        updateSelectizeInput(
          session,
          "search",
          choices = data()$Organization,
          selected = current_selected,
          server = TRUE
        )
      } else if (input$search_control == "County") {
        updateSelectizeInput(
          session,
          "search",
          choices = sort(data()$County),
          selected = current_selected,
          server = TRUE
        )
        
      }
    })
    
    
    met_pallete <- MetBrewer::met.brewer(name="Isfahan2",n=20)
    met_pallete <- as.character(met_pallete)
    
    # Color Palettes -----------------------------------------------------
    pal <- reactive({
      colorFactor(
        palette = met_pallete,
        reverse = FALSE,
        domain = map_data()$name,
        na.color = rgb(0, 0, 0, 0)
      )
    })
    pal_org <- reactive({
      colorFactor(
        palette = met_pallete,
        reverse = FALSE,
        domain = data()$Type,
        na.color = rgb(0, 0, 0, 0)
      )
    })
    # River Mapping -----------------------------------------------------
    gon_line <- function(x) {
      if (geo() == "Rivers") {
        addPolylines(
          color = "lightblue",
          weight = 3,
          smoothFactor = 0.5,
          opacity = 0.7,
          fillOpacity = 0.7,
          fillColor = ~ pal()(map_data()$name),
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          popup = map_data()$name,
          label = map_data()$name
        )
      } else {
        addPolygons(
          color = ~ pal()(map_data()$name),
          weight = 2,
          smoothFactor = 0.5,
          opacity = 0.7,
          fillOpacity = 0.7,
          fillColor = ~ pal()(map_data()$name),
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            stroke = 4,
            bringToFront = NULL
          ),
          popup = map_data()$name,
          label = map_data()$name
        )
      }
      
    }
    # Zoom -----------------------------------------------------  
    output$map <- renderLeaflet({
      if (input$search == "Search..." |input$search == "") {
        ZOOM = 6
        LAT = 30.997210
        LONG = -99.808835
      } else if (input$search_control == "Organization") {
        org_selected = data() |> filter(Organization == input$search)
        LAT = org_selected$lat
        LONG = org_selected$lon
        ZOOM = 11
      } else if (input$search_control == "County") {
        county_selected = county_lonlat |> filter(County == input$search)
        ZOOM = 11
        LAT = county_selected$lat_c
        LONG = county_selected$long
      }
      
      # Map Design -----------------------------------------------------
      leaflet(map_data(), options = leafletOptions(zoomControl = FALSE)) |>
        setView(lng = LONG,
                lat = LAT,
                zoom = ZOOM)  |>
        addProviderTiles(providers$CartoDB.Positron) |>
        clearShapes() |>
        clearControls() %>%
        {
          if (county() == TRUE)
            addPolygons(
              map = .,
              data = counties_shape,
              color = "grey",
              weight = 3,
              smoothFactor = 0.5,
              opacity = 0.3,
              fillOpacity = 0
            )
          else
            addPolygons(
              map = .,
              data = counties_shape,
              color = "#2389da",
              weight = 0,
              smoothFactor = 0.5,
              opacity = 0,
              fillOpacity = 0
            )
        } %>%
        {
          if (geo() == "Rivers")
            addPolylines(
              map = .,
              color = "#2389da",
              weight = 3,
              smoothFactor = 0.5,
              opacity = 0.7,
              fillOpacity = 0.7,
              fillColor = ~ pal()(map_data()$name),
              highlightOptions = highlightOptions(
                color = "white",
                weight = 2,
                bringToFront = TRUE
              ),
              popup = map_data()$name,
              label = map_data()$name
            )
          else
            addPolygons(
              map  = .,
              color = ~ pal()(map_data()$name),
              weight = 2,
              smoothFactor = 0.5,
              opacity = 0.7,
              fillOpacity = 0.7,
              fillColor = ~ pal()(map_data()$name),
              highlightOptions = highlightOptions(
                color = "white",
                weight = 2,
                stroke = 4,
                bringToFront = NULL
              ),
              popup = map_data()$name,
              label = map_data()$name
            )
          
        } %>%
        clearMarkers() |> #you have to clear previously drawn markers
        addCircleMarkers(
          data = data(),
          lng =  ~ lon,
          lat =  ~ lat,
          stroke = FALSE,
          weight = 3,
          opacity = .7,
          color = "black",
          fillColor = ~ "black",
          fillOpacity = .7,
          radius = 4,
          label = ~ paste0(data()$Organization),
          popup = ~ paste0(data()$Organization)
        )
      
      
    })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# mod_map_server("map_ui_1")
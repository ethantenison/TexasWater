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
          absolutePanel(top = 5, right = 20,left = 5, bottom = "auto",
                        fluidRow(
                          column(width = 2),
                          column(
                            width = 10,
                            style = "margin: 7px 0 0 0;",
                            div(
                              style = "float:right; color: black; font-size: 16px;",
                              prettyRadioButtons(
                                inputId = ns("search_control"),
                                choices = c("County", "Organization"),
                                selected = "County",
                                label = "",
                                width = "100%",
                                animation = "jelly",
                                inline = TRUE
                              ),
                              selectizeInput(
                                ns("search"),
                                label = NULL,
                                choices = "",
                                multiple = FALSE,
                                selected = character(0),
                                width = "100%",
                                options = list(allowEmptyOption = FALSE,
                                               placeholder = "Zoom Search...")
                              ),
                              div(
                                style = "margin-top:-15px;",
                                searchInput(
                                  ns("search_bar"),
                                  label = "",
                                  placeholder = "Enter Address...",
                                  width = "200px",
                                  btnSearch = icon("search"),
                                  btnReset = icon("remove")
                                )
                              )
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
          choices = data()$County,
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
    # Leaflet Mapping -----------------------------------------------------
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
    
    output$map <- renderLeaflet({
      # Get latitude and longitude
      if (input$search_bar == "" |
          input$search_bar == "Enter Address...") {
        ZOOM = 6
        LAT = 30.997210
        LONG = -99.808835
      } else{
        add <- tibble(address = input$search_bar)
        print(add)
        target_pos <-
          add |> tidygeocoder::geocode(address =  address, method = "osm")
        LAT = target_pos$lat
        LONG = target_pos$long
        ZOOM = 11
      }
      
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
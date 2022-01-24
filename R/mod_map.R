#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import leaflet
#' @import sf
#' @import RColorBrewer
mod_map_ui <- function(id, height){
  ns <- NS(id)
  tagList(
    
    leafletOutput(ns("map"), height = height)
 
  )
}
    
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    map_data <- readRDS("./data-raw/rb.rds")
    
    pal <- reactive({
        colorFactor(
          palette = "Set1",
          reverse = FALSE,
          domain = map_data$basin_name,
          na.color=rgb(0,0,0,0)
        )
        })
    
    output$map <- renderLeaflet({
      
      leaflet(map_data, options = leafletOptions(zoomControl = FALSE)) |>
        setView(lng = -97.75242943917551,
                lat = 30.327729034791303,
                zoom = 5)  |>
        addProviderTiles(providers$CartoDB.Positron) |>
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") |> 
        clearShapes() |>
        clearControls() |>
        addPolygons(
          color = "#444444",
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0,
          fillOpacity = 0.7,
          fillColor = ~ pal()(map_data$basin_name),
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ))
      
      
    })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# mod_map_server("map_ui_1")

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
    
    leafletOutput(ns("map"), height = height),
    absolutePanel(top = 10, right = 10,
                  fluidRow(
                    column(
                      width = 5,
                      style = "margin: 7px 0 0 0; font-size: 14px;",
                      div(
                        style = "float:right",
                        pickerInput(
                          ns("admin"), 
                          label = NULL, 
                          choices = c("Aquifers","GCDs", "Rivers",
                                      "River Basins", "RWPAs"), 
                          multiple  = FALSE, selected = "River Basins",
                          width = "200px"
                        )
                      )
                  ))
 
  )
  )
}
    
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # Admin Area selection -----------------------------------------------------
    
    # reactive to select geographic data
    map_data <- reactive({
      if (input$admin == "River Basins") {
        readRDS("./data-raw/rb.rds")
      } else if (input$admin == "GCDs") {
        readRDS("./data-raw/gcd.rds")
      } else if (input$admin == "Rivers") {
        readRDS("./data-raw/riv.rds")
      } else if (input$admin == "Aquifers") {
        readRDS("./data-raw/aqu.rds")
      } else if (input$admin == "River Basins") {
        readRDS("./data-raw/rb.rds")
      } else if (input$admin == "RWPAs") {
        readRDS("./data-raw/rwpa.rds")
      }
    })
    
    
    
     pal <- reactive({
         colorFactor(
           palette = "Set1",
          reverse = FALSE,
           domain = map_data()$name,
           na.color=rgb(0,0,0,0)
         )
         })
    
    output$map <- renderLeaflet({
      
      if(input$admin == "Rivers"){
        
        leaflet(map_data(), options = leafletOptions(zoomControl = FALSE)) |>
          setView(lng = -99.808835,
                  lat = 30.997210,
                  zoom = 6)  |>
          addProviderTiles(providers$CartoDB.Positron) |>
          clearShapes() |>
          clearControls() |>
          addPolylines(
            color = "blue",
            weight = 1,
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
            label = map_data()$name)
        
        
      } else {
        leaflet(map_data(), options = leafletOptions(zoomControl = FALSE)) |>
          setView(lng = -99.808835,
                  lat = 30.997210,
                  zoom = 6)  |>
          addProviderTiles(providers$CartoDB.Positron) |>
          clearShapes() |>
          clearControls() |>
          addPolygons(
            color = "#444444",
            weight = 1,
            smoothFactor = 0.5,
            opacity = 0,
            fillOpacity = 0.7,
            fillColor = ~ pal()(map_data()$name),
            highlightOptions = highlightOptions(
              color = "white",
              weight = 2,
              bringToFront = TRUE
            ),
            popup = map_data()$name,
            label = map_data()$name)
        
        
      }
      
      
      
    })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# mod_map_server("map_ui_1")

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
#' @import ggmap
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
                          choices = c("Aquifers","Counties", "GCDs", "Rivers",
                                      "River Basins", "RWPAs"), 
                          multiple  = FALSE, selected = "River Basins",
                          width = "200px"
                        ),
                        div(style = "margin-top:-15px;",
                        textInput(ns("search_bar"), "", "Enter Address...",
                                  width = "200px"))
                        
                      ),
                      
                        
                  ))
 
  )
  )
}
    
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  
    # Admin Area selection -----------------------------------------------------
    
    # reactive to select geographic data
    map_data <- reactive({
      if (input$admin == "River Basins") {
        readRDS("./data/rb.rds")
      } else if (input$admin == "GCDs") {
        readRDS("./data/gcd.rds")
      } else if (input$admin == "Rivers") {
        readRDS("./data/riv.rds")
      } else if (input$admin == "Aquifers") {
        readRDS("./data/aqu.rds")
      } else if (input$admin == "River Basins") {
        readRDS("./data/rb.rds")
      } else if (input$admin == "RWPAs") {
        readRDS("./data/rwpa.rds")
      } else if (input$admin == "Counties") {
        readRDS("./data/counties.rds")
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
     
     pal_org <- reactive({
       colorFactor(
         palette = "Set1",
         reverse = FALSE,
         domain = data()$Type,
         na.color=rgb(0,0,0,0)
       )
     })
     
     gon_line <- function(x){
       
       if(input$admin == "Rivers"){ 
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
         label = map_data()$name)
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
           label = map_data()$name)
         }

     }
    
    output$map <- renderLeaflet({
      
        # Get latitude and longitude
        if(input$search_bar=="" | input$search_bar == "Enter Address..."){
          ZOOM=6
          LAT=30.997210
          LONG=-99.808835
        }else{
          target_pos=ggmap::geocode(input$search_bar)
          LAT=target_pos$lat
          LONG=target_pos$lon
          ZOOM=11
        }
        
      
        leaflet(map_data(), options = leafletOptions(zoomControl = FALSE)) |>
          setView(lng = LONG,
                  lat = LAT,
                  zoom = ZOOM)  |>
          addProviderTiles(providers$CartoDB.Positron) |>
          clearShapes() |>
          clearControls() %>%
        {if(input$admin == "Rivers") addPolylines(
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
          label = map_data()$name) else 
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
              label = map_data()$name)
            
          } %>% 
          clearMarkers() |>#you have to clear previously drawn markers
          addCircleMarkers(data = data(), lng =  ~ lon,lat =  ~ lat,
                           stroke = FALSE,
                           weight = 3,
                           opacity = .7,
                           color = "black",
                           fillColor = ~ "black",
                           fillOpacity = .7,
                           radius = 4,
                           label = ~paste0(data()$Organization),
                           popup = ~paste0(data()$Organization)
                           )
        
      
      
    })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# mod_map_server("map_ui_1")


# addPolylines(
#   color = "lightblue",
#   weight = 3,
#   smoothFactor = 0.5,
#   opacity = 0.7,
#   fillOpacity = 0.7,
#   fillColor = ~ pal()(map_data()$name),
#   highlightOptions = highlightOptions(
#     color = "white",
#     weight = 2,
#     bringToFront = TRUE
#   ),
#   popup = map_data()$name,
#   label = map_data()$name)
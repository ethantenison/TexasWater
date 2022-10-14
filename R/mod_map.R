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
  tagList(leafletOutput(ns("map"), height = height))
}
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id,data,geo, focus, search) {
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
      } else if (geo() == "PWS") {
        readRDS("./data/pws.rds")
      } else if (geo() == "SWCD") {
        readRDS("./data/swcd.rds")        
      }
    })
    # Color Palettes -----------------------------------------------------
    met_pallete_geo <- MetBrewer::met.brewer(name="Isfahan2",n=20)
    met_pallete_geo <- as.character(met_pallete_geo)
    
    met_pallete_org <- MetBrewer::met.brewer(name="Isfahan2",n=8)
    met_pallete_org <- as.character(met_pallete_org)
    
    shadesOfGrey <- colorRampPalette(c("grey50","#324E92", "#35A7A3"))
    shadesOfGrey2 <- colorRampPalette(c("grey0", "grey50"))
    
    pal <- reactive({
      if(focus() == FALSE) {
      colorFactor(
        palette = met_pallete_geo,
        reverse = FALSE,
        domain = map_data()$name,
        na.color = rgb(0, 0, 0, 0)
      )
      } else {
        colorFactor(
          palette = shadesOfGrey(20),
          reverse = FALSE,
          domain = map_data()$name,
          na.color = rgb(0, 0, 0, 0)
        )
      }
    })
    pal_org <- reactive({
      if(focus() == FALSE) { 
        colorFactor(
          palette = shadesOfGrey2(5),
          reverse = FALSE,
          domain = data()$Sector,
          na.color = rgb(0, 0, 0, 0)
        )
        } else {
      colorFactor(
        palette = met_pallete_org,
        reverse = FALSE,
        domain = data()$Sector,
        na.color = rgb(0, 0, 0, 0)
      )
}
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
      if (search() == "All") {
        ZOOM = 6
        LAT = 30.997210
        LONG = -99.808835
      } else { 
        county_selected = county_lonlat |> filter(County == search())
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
            addPolygons(
              map = .,
              data = counties_shape,
              color = "grey",
              weight = 3,
              smoothFactor = 0.5,
              opacity = 0.3,
              fillOpacity = 0
            ) %>%
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
          color = ~ pal_org()(data()$Sector),
          fillColor = ~ pal_org()(data()$Sector),
          fillOpacity = .7,
          radius = 4,
          label = ~ paste0(data()$Organization),
          popup = ~ paste0(data()$Organization)
        ) |> 
        addLegend(
          data = data(),
          "bottomright",
          pal = pal_org(),
          values = data()$Sector,
          title = "Sector"
        )
    })
  })
}
## To be copied in the UI
# mod_map_ui("map_ui_1")
    
## To be copied in the server
# mod_map_server("map_ui_1")
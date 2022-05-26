#' controls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import reactable
#' @import shinyWidgets
#' @import readr
#' @import dplyr
#' @importFrom stringr str_detect
#' @import glue
#' @import shinyjs
mod_controls_ui <- function(id) {
  ns <- NS(id)
  
  jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
     var element = document.documentElement,
 enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
 exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
 if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
 enterFS.call(element);
 } else {
 exitFS.call(document);
 }
 }'
  zoom_choices <- readr::read_csv("data/organization.csv") |> 
    select(County, county_name, lat_c, long) %>% 
    add_row(County = "All", county_name = "All", lat_c = 30.997210, 
            long = -99.808835)
    
  
  tagList(
    fluidRow(
      column(width =6,
             pickerInput(
               ns("sector"),
               label = strong("Sector"),
               choices = c("All", "Rural", "Agriculture",
                           "Groundwater"),
               multiple  = FALSE,
               selected = "All",
               width = "100%"
             )),
      column(width =6, 
             pickerInput(
               ns("search"),
               label = strong("County"),
               choices = unique(sort(zoom_choices$County)),
               multiple = FALSE,
               selected = "All",
               width = "100%",
               options = pickerOptions(
                 liveSearch = TRUE,
                 liveSearchPlaceholder = "Search..."
               )
             ))),
      fluidRow(
        column(
          width = 12,
             p("Etiam purus enim, accumsan vel tortor in, sollicitudin porttitor
               sapien. Aliquam vitae dignissim felis."))
    ),
    fluidRow(
        column(
          width = 12,
        pickerInput(
          ns("admin"),
          label = strong("Geography"),
          choices = c(
            "Aquifers",
            "Counties",
            "Ground Water Conservation Districts" = "GCDs",
            "Regional Water Planning Groups" = "RWPGs",
            "Regional Flood Planning Groups" ="RFPGs",
            "Rivers",
            "River Basins"
          ),
          multiple  = FALSE,
          selected = "River Basins",
          width = "100%"
        ))
    ),
    fluidRow(
      column(
        width = 12,
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer
          bibendum lacinia magna, in ornare felis cursus at.")
      )
    ),
    fluidRow(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jsToggleFS, functions = "toggleFullScreen"),
      hr(style = "margin-top: 5px; margin-bottom: 5px; width:100%"),
      column(
        width = 12,
      HTML(
        "<button type='button' class='btn btn-default action-button shiny-bound-input' style='display: block;
        margin: 6px 6px 6px 0px; width: 100%;' onclick = 'shinyjs.toggleFullScreen();
        '><i class='fa fa-expand fa-pull-left'></i> Fullscreen</button>"
      ))
    )
  )
}

#' table Server Functions
#'
#' @noRd
mod_controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    orgs <- readr::read_csv("data/organization.csv")
    
    # Sector selection ---------------------------------------------------------
    # reactive to store choices of the org input field
    org_choices <- reactive({
      req(orgs$Sector)
        d <- orgs %>% filter(search == input$sector) %>% arrange(Organization)
        d
    })
    
    
    #Objects sent to other modules 
    list(
      org_choices = org_choices,
      geo = reactive(input$admin),
      focus = reactive(input$focus),
      search = reactive(input$search)
    )
    
  })
}
    
## To be copied in the UI
# mod_controls_ui("controls_1")
    
## To be copied in the server
# mod_controls_server("controls_1")

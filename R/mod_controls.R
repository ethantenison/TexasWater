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
  zoom_choices <- readr::read_csv("data/organization_updated.csv") |> 
    select(County, county_name, lat_c, long) %>% 
    add_row(County = "All", county_name = "All", lat_c = 30.997210, 
            long = -99.808835)
    
  
  tagList(
    fluidRow(
      column(width =6,
             pickerInput(
               ns("sector"),
               label = strong("Sector"),
               choices = c("All", "Agriculture","Energy", "Flood",
                           "Groundwater", "Policy", "Rural"),
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
        reactableOutput(ns("table"), width = "auto", height = "400px", inline = FALSE)
      )
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
    
    orgs <- readr::read_csv("data/organization_updated.csv")
    
    # Sector selection ---------------------------------------------------------
    # reactive to store choices of the org input field
    org_choices <- reactive({
      req(orgs$Sector)
        d <- orgs %>% filter(search == input$sector) %>% arrange(Organization)
        d
    })
    
    # Table --------------------------------------------------------------------
    
    output$table <- renderReactable({
        onclick_js <- JS(
          glue(
            "function(rowInfo, colInfo) {
            // Only handle click events on the 'mb' column
            if (colInfo.id !== 'Organization') {
              return
            }

            // Send the click event to Shiny, which will be available in input$show_details
            // Note that the row index starts at 0 in JavaScript, so we add 1
            if (window.Shiny) {
              Shiny.setInputValue('<<< ns('show_details') >>>', { index: rowInfo.index + 1, rnd: Math.random() })
            }
          }",
          .open = "<<<",
          .close = ">>>"
          )
        )
        reactable(
          org_choices(),
          compact = TRUE,
           theme = reactableTheme(
             backgroundColor = "#264D96",
             highlightColor = "#2BADBD",
             color = "#FFFFFF"
           ),
          defaultColDef = colDef(minWidth = 20,
                                 footerStyle = "font-weight: bold"),
          highlight = TRUE,
          defaultPageSize = 10,
          paginationType = "simple",
          searchable = TRUE,
          wrap = TRUE,
          onClick = onclick_js,
          columns = list(
            Organization = colDef(
              minWidth = 50,
              class = "area-link",
              style = list(cursor = "pointer")
            ),
            # overrides the default
            Address = colDef(show = F),
            lon = colDef(show = F),
            lat = colDef(show = F),
            search = colDef(show = F),
            Sector = colDef(show = T),
            county_name = colDef(show = F),
            lat_c = colDef(show = F),
            long = colDef(show = F),
            Type = colDef(show = T)
          ),
        )
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

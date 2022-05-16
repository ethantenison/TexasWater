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
  
  tagList(
    fluidRow(
      column(width =10,
             pickerInput(
               ns("sector"),
               label = strong("Sector"),
               choices = c("All", "Rural", "Agriculture",
                           "Groundwater"),
               multiple  = FALSE,
               selected = "All",
               width = "100%"
             )),
      column(width = 2,
             materialSwitch(
               ns("focus"),
               label = strong("Org Focus"),
               value = TRUE,
               inline = TRUE
             )
      )),
      fluidRow(
        column(
          width = 12,
             p("Etiam purus enim, accumsan vel tortor in, sollicitudin porttitor
               sapien. Aliquam vitae dignissim felis. Nulla facilisi.
               Quisque laoreet sagittis nunc et maximus. Proin nec consectetur tellus."))
    ),
    fluidRow(
        column(
          width = 10,
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
        )),
        column(width = 2,
        materialSwitch(
          ns("counties"),
          label = strong("Counties"),
          value = TRUE,
          inline = TRUE
        )
        )
    ),
    fluidRow(
      column(
        width = 12,
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer
          bibendum lacinia magna, in ornare felis cursus at. Duis id pharetra
          elit. Interdum et malesuada fames ac ante ipsum primis in faucibus.
          Suspendisse sed porta odio, id sagittis leo.")
      )
    ),
    fluidRow(
      column(
        width = 10,
        prettyRadioButtons(
          inputId = ns("search_control"),
          choices = c("County", "Organization"),
          selected = "County",
          label = strong("Search by:"),
          width = "100%",
          animation = "jelly",
          inline = TRUE
        ),
        pickerInput(
          ns("search"),
          label = NULL,
          choices = "",
          multiple = FALSE,
          selected = NULL,
          width = "100%",
          options = pickerOptions(
                         liveSearch = TRUE,
                         liveSearchPlaceholder = "Search...")
        )
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
        margin: 6px 5px 6px 15px; width: 100%;' onclick = 'shinyjs.toggleFullScreen();
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
    
    # populate the selectizeInput choices
    observe({
      current_selected <- isolate(input$search)
      if (input$search_control == "Organization") {
        updatePickerInput(
          session,
          "search",
          choices = unique(sort(org_choices()$Organization)),
          selected = current_selected
        )
      } else if (input$search_control == "County") {
        updatePickerInput(
          session,
          "search",
          choices = unique(sort(org_choices()$County)),
          selected = current_selected
        )
      }
    })
    
    print(reactive(input$search))
    
    #Objects sent to other modules 
    list(
      org_choices = org_choices,
      geo = reactive(input$admin),
      county = reactive(input$counties),
      focus = reactive(input$focus),
      search = reactive(input$search),
      search_control = reactive(input$search_control)
    )
    
  })
}
    
## To be copied in the UI
# mod_controls_ui("controls_1")
    
## To be copied in the server
# mod_controls_server("controls_1")

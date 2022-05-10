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
      column(width =8,
             style = "margin: -5px 0px 5px 0",
             pickerInput(
               ns("sector"),
               label = strong("Select Sector"),
               choices = c("All", "Rural", "Agriculture",
                           "Groundwater"),
               multiple  = FALSE,
               selected = "All",
               width = "175px"
             )),
      column(width = 4,
             div(
               style = "float:right",
             materialSwitch(
               ns("focus"),
               label = strong("Focus"),
               value = TRUE,
               inline = TRUE
             ))
      )),
      fluidRow(
             p("Etiam purus enim, accumsan vel tortor in, sollicitudin porttitor
               sapien. Aliquam vitae dignissim felis. Nulla facilisi. Fusce
               ultricies nulla massa. Sed convallis ante mi, vel mattis turpis
               convallis eu. Suspendisse ac metus nec augue porta facilisis.
               Quisque laoreet sagittis nunc et maximus. Proin nec consectetur tellus.")
    ),
    fluidRow(
        column(
          width = 8,
        pickerInput(
          ns("admin"),
          label = strong("Select Geography"),
          choices = c(
            "Aquifers",
            "Counties",
            "GCDs",
            "Rivers",
            "River Basins",
            "RWPGs",
            "RFPGs"
          ),
          multiple  = FALSE,
          selected = "River Basins",
          width = "175px"
        )),
        column(width = 4,
               div(
                 style = "float:right",
        materialSwitch(
          ns("counties"),
          label = strong("Counties"),
          value = TRUE,
          inline = TRUE
        )
        ))
    ),
    fluidRow(
      column(
        width = 12,
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer
          bibendum lacinia magna, in ornare felis cursus at. Duis id pharetra
          elit. Interdum et malesuada fames ac ante ipsum primis in faucibus.
          Suspendisse sed porta odio, id sagittis leo. Proin ut purus in purus
          tincidunt feugiat vitae suscipit metus. Praesent ullamcorper odio id
          ex sodales congue. Mauris sed tempus justo. Fusce volutpat sollicitudin
          dolor eu imperdiet. Donec in nisl vitae leo efficitur ornare. Nulla
          facilisi. Nullam pretium sed libero quis efficitur.")
      )
    ),
    fluidRow(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jsToggleFS, functions = "toggleFullScreen"),
      hr(style = "margin-top: 5px; margin-bottom: 5px; width:90%"),
      column(
        width = 6,
      HTML(
        "<button type='button' class='btn btn-default action-button shiny-bound-input' style='display: block;
        margin: 6px 5px 6px 15px; width: 100%;' onclick = 'shinyjs.toggleFullScreen();
        '><i class='fa fa-expand fa-pull-left'></i> Fullscreen</button>"
      )),
      column(
        width = 6,
        style = "margin-top: 6px; width: 100%;",
        actionButton("full", 
                     label = "Submit Organization",
                     class = "btn-default",
                     icon = icon("plus"))
      )
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
      county = reactive(input$counties),
      focus = reactive((input$focus))
      
    )
    
  })
}
    
## To be copied in the UI
# mod_controls_ui("controls_1")
    
## To be copied in the server
# mod_controls_server("controls_1")

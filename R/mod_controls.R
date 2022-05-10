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
mod_controls_ui <- function(id) {
  ns <- NS(id)
  
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

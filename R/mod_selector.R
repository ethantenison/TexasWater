#' selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selector_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' selector Server Functions
#'
#' @noRd 
mod_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_selector_ui("selector_ui_1")
    
## To be copied in the server
# mod_selector_server("selector_ui_1")

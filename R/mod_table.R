#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_ui <- function(id){
  ns <- NS(id)
  library(reactable)
  tagList(
    
    div(
      style = "margin: 5px -5px 0 -5px; height: calc(100% - 130px)", 
      reactable::reactableOutput(ns("table"), height = "100%")
    )
 
  )
}
    
#' table Server Functions
#'
#' @noRd 
mod_table_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$table <- renderReactable({
      
      
      reactable(data)
      
    })
 
  })
}
    
## To be copied in the UI
# mod_table_ui("table_ui_1")
    
## To be copied in the server
# mod_table_server("table_ui_1")

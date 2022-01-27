#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#'     
#' @import shiny
#' @import readr
#' 
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  mod_selector_server("selector")
  
  
  table <- mod_table_server("table")
  
  mod_map_server("map", table)

}

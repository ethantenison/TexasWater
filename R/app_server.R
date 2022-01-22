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
  
  mod_map_server("map")
  
  mod_table_server("table")
  
}

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
  
  
  controls <- mod_controls_server("controls")
  
  org_choices <- controls$org_choices
  geo <- controls$geo
  county <- controls$county
  focus <- controls$focus
  search <- controls$search
  
  mod_map_server("map", org_choices, geo, county, focus, search)

}

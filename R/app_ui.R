#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' 
#' @noRd
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      fluidRow(
        style = "margin: 0; height: 100%",
        column(
          width = 4,
          style = "height: 100%; padding-bottom: 65px",
          mod_table_ui("table")
        ),
        column(
          width = 8,
          style = "height: 100%; padding: 0 0 65px 0",
          mod_map_ui("map", height = "900"),
          mod_selector_ui("selector")
        ),
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'TexasWater'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


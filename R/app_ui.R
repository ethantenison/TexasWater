#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyjs
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    tags$style(
      "@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"
    ),
    fluidPage(
      theme = bslib::bs_theme(
        bg = "#264D96",
        fg = "#FFFFFF",
        primary = "#2BADBD",
        #secondary = "#2BADBD",
        success = "#55A353",
        base_font = font_google("Prompt"),
        code_font = font_google("JetBrains Mono")
      ),
      navbarPage(
        "Texas Water Sector",
        id = "nav",
        tabPanel(
          title = shiny::tags$u("Map"),
          sidebarLayout(
          sidebarPanel(width = 4,
                       fluidRow(
                         style = "margin: 0; height: 100%",
                         column(
                           width = 12,
                           style = "height: 100%; padding-bottom: 10px",
                           mod_controls_ui("controls")
                         )
                       )),
          mainPanel(width = 8,
                    fluidRow(
                      style = "margin: 0;",
                      column(
                        width = 12,
                        style = "height: 100%;",
                        mod_map_ui("map", height = "750"),
                        mod_selector_ui("selector")
                      )
                    ))),
        ),
        tabPanel(
          title = shiny::tags$u("Feedback"),
          "Enter definitions here!"
      )
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
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))
  
  tags$head(favicon(),
            bundle_resources(path = app_sys('app/www'),
                             app_title = 'TexasWater')
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert() 
            )
            }

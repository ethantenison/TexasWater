#' feedback UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_feedback_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    fluidRow(
             column(width = 2),
             column(width = 8,
                    align = "center",
                    style = "vertical-align: middle;",
                    h1("We want to hear from you!")),
             column(width = 2)),
    br(),
    br(),
    fluidRow(
      column(width =2),
      column(width =4,
             align = "center",
             style = "width: 100%;",
             actionButton("full", 
                          width = "100%",
                          label = "Add Organization",
                          class = "btn-default",
                          icon = icon("plus"))),
      column(width =4,
             align = "center",
             style = "width: 100%;",
             actionButton("full", 
                          width = "100%",
                          label = "User Feedback",
                          class = "btn-default",
                          icon = icon("user"))),
      column(width =2),
    )
    
    
 
  )
}
    
#' feedback Server Functions
#'
#' @noRd 
mod_feedback_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_feedback_ui("feedback_1")
    
## To be copied in the server
# mod_feedback_server("feedback_1")

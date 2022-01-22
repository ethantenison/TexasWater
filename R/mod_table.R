#' table UI Function
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
mod_table_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      style = "margin: 0px -12px 0 -12px",
      column(
        style = "margin: 0px 0 0 0; font-size: 14px;",
        width = 8,
      h3("Texas Water")),
      column(
        width = 4,
        style = "margin: 7px 0 0 0; font-size: 14px;",
        div(
          style = "float:right",
          pickerInput(
            ns("sector"), 
            label = NULL, 
            choices = c("All","Rural", "Agriculture",
                        "Groundwater"), 
            multiple  = FALSE, selected = "All",
            width = "100px"
          )
        )
      ))
    ,
    fluidRow(
      column(
        width = 12,
        style = "    margin: -5px 0px 5px 0",
        selectizeInput(
          ns("search"), 
          label = NULL, 
          choices = "", multiple = FALSE, selected = character(0),
          width = "100%", options = list(allowEmptyOption = FALSE, placeholder = "SEARCH..."))
      )
    ),
    
    div(
      style = "margin: 5px -5px 0 -5px; height: calc(100% - 130px)", 
      reactable::reactableOutput(ns("table"), height = "100%")
    )
 
  )
}
    
#' table Server Functions
#'
#' @noRd 
mod_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    orgs <- readr::read_csv("data-raw/org_table.csv")
    
    werk <- reactive({
      
      orgs |>  
        dplyr::filter(search == input$sector) |> 
        select(-c(search))
  
      
    })
    
    output$table <- renderReactable({
      
      
      reactable(werk())
      
    })
 
  })
}
    
## To be copied in the UI
# mod_table_ui("table_ui_1")
    
## To be copied in the server
# mod_table_server("table_ui_1")

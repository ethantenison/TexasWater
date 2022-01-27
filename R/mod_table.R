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
#' @importFrom stringr str_detect
mod_table_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      style = "margin: 0px -12px 0 -12px",
      column(
        style = "margin: 0px 0 0 0; font-size: 14px;",
        width = 7,
        prettyRadioButtons(
          inputId = ns("search_control"),
          choices = c("Zip", "County", "Organization"),
          selected = "Zip",
          label = "",
          width = "auto",
          animation = "jelly",
          inline = TRUE
        )
      ),
      column(
        width = 5,
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
    
    orgs <- readr::read_csv("data-raw/organization.csv")
    
    
    # Sector selection ---------------------------------------------------------
    
    # reactive to store choices of the org input field
    org_choices <- reactive({
      req(orgs$Sector)
      d <- orgs %>% filter(search == input$sector) %>% arrange(Organization)
      d
    })
    
    # populate the selectizeInput choices
    observe({
      req(org_choices())
      current_selected <- isolate(input$search)
      updateSelectizeInput(session, "search", choices = org_choices()$Organization, selected = current_selected, server = TRUE)
    })
    
    # Data for table 
    to_table <- reactive({
      
      req(orgs$Organization, orgs$search)
      
      if(input$search == "") {
        
        orgs |>  
          dplyr::filter(search == input$sector) |> 
          select(-c(search, lon, lat))
        
      } else {
        
        orgs |>  
          dplyr::filter(search == input$sector) |> 
          select(-c(search, lon, lat)) |> 
          filter(str_detect(Organization, input$search))
      }
      
      
    })
    
    # Render Table ------------------------------------------------------------
    
    output$table <- renderReactable({
      
      
      reactable(to_table())
      
    })
    
    return(org_choices)
 
  })
}
    
## To be copied in the UI
# mod_table_ui("table_ui_1")
    
## To be copied in the server
# mod_table_server("table_ui_1")

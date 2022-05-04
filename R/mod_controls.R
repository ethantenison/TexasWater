#' controls UI Function
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
#' @import glue
mod_controls_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        style = "    margin: -5px 0px 5px 0",
      h5("Organization Controls"))
      ),
    fluidRow(
      style = "margin: 0px -12px 0 -12px",
      column(
        style = "margin: 0px 0 0 0; font-size: 14px;",
        width = 8,
        prettyRadioButtons(
          inputId = ns("search_control"),
          choices = c("County", "Organization"),
          selected = "Organization",
          label = "",
          width = "auto",
          animation = "jelly",
          inline = TRUE
        )
      ),
      column(
        width = 4,
        style = "margin: 7px 0 0 0; font-size: 14px;",
        div(
          style = "float:right",
          pickerInput(
            ns("sector"),
            label = NULL,
            choices = c("All", "Rural", "Agriculture",
                        "Groundwater"),
            multiple  = FALSE,
            selected = "All",
            width = "100px"
          )
        )
      )
    )
    ,
    fluidRow(
      column(
        width = 12,
        style = "    margin: -5px 0px 5px 0",
        selectizeInput(
          ns("search"),
          label = NULL,
          choices = "",
          multiple = FALSE,
          selected = character(0),
          width = "100%",
          options = list(allowEmptyOption = FALSE, placeholder = "SEARCH...")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        style = "    margin: -5px 0px 5px 0",
        h5("Boundary Controls")),
        column(
          width = 8,
        pickerInput(
          ns("admin"),
          label = "Select Geography",
          choices = c(
            "Aquifers",
            "Counties",
            "GCDs",
            "Rivers",
            "River Basins",
            "RWPGs",
            "RFPGs"
          ),
          multiple  = FALSE,
          selected = "River Basins",
          width = "150px"
        )),
        column(width = 4,
        materialSwitch(
          ns("counties"),
          label = "Counties",
          value = TRUE,
          inline = TRUE
        )
        )
    ),
    
    
    # div(style = "margin: 5px -5px 0 -5px; height: calc(100% - 130px)",
    #     reactable::reactableOutput(ns("table"), height = "640px"))
    
  )
}

#' table Server Functions
#'
#' @noRd
mod_controls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    orgs <- readr::read_csv("data/organization.csv")
    
    # Sector selection ---------------------------------------------------------
    
    # reactive to store choices of the org input field
    org_choices <- reactive({
      req(orgs$Sector)
      if (input$search_control == "Organization") {
        d <-
          orgs %>% filter(search == input$sector) %>% arrange(Organization)
        d
      } else if (input$search_control == "County") {
        d <- orgs %>% filter(search == input$sector) %>% arrange(County)
        d
      }
    })
    
    
    # populate the selectizeInput choices
    observe({
      req(org_choices())
      current_selected <- isolate(input$search)
      if (input$search_control == "Organization") {
        updateSelectizeInput(
          session,
          "search",
          choices = org_choices()$Organization,
          selected = current_selected,
          server = TRUE
        )
      } else if (input$search_control == "County") {
        updateSelectizeInput(
          session,
          "search",
          choices = org_choices()$County,
          selected = current_selected,
          server = TRUE
        )
        
      }
    })
    
    # Data for table
    to_table <- reactive({
      req(orgs$Organization, orgs$search)
      
      if (input$search == "") {
        orgs |>
          dplyr::filter(search == input$sector)
        
      } else if (input$search != "" &
                 input$search_control == "Organization") {
        orgs |>
          dplyr::filter(search == input$sector) |>
          filter(str_detect(Organization, input$search))
      } else if (input$search != "" &
                 input$search_control == "County") {
        orgs |>
          dplyr::filter(search == input$sector) |>
          filter(str_detect(County, input$search))
      }
      
    })
    
    # This code takes the javascript code and sends it to the map if clicked
    observeEvent(input$show_details, {
      req(input$show_details)
      
      selected_row <- to_table()[input$show_details$index, ]
      
      print(selected_row)
      return(selected_row)
      
      # change the app state
      # state$state <- list(
      #   id = STATE_MB_SELECTED,
      #   store = list(selected_mb = selected_row$code, event_source = "table")
      # )
    })
    
    show_condition <- function(code) {
      tryCatch(
        code,
        error = function(c)
          "error",
        warning = function(c)
          "warning",
        message = function(c)
          "message"
      )
    }
    
    if (show_condition(selected_row) == "error") {
      org = ""
    } else {
      org = selected_row
    }
    
    list(
      org_choices = org_choices,
      org = reactive(org),
      geo = reactive(input$admin),
      county = reactive(input$counties)
      
    )
    
  })
}
    
## To be copied in the UI
# mod_controls_ui("controls_1")
    
## To be copied in the server
# mod_controls_server("controls_1")

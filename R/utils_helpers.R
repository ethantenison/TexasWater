#' helpers 
#'
#' @description utils functions

#' material_card
#' @description provides a card for major UI elements 
material_card <- function(..., header = NULL, bgcolor = "#191a1a") {
div(
  class = "card",
  header, 
  div(class = "card-content", ..., style = sprintf("background-color: %s", bgcolor))
)
}
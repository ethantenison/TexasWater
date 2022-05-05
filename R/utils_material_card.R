#' material_card 
#'
#' @description Wraps around columns
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

material_card <- function(..., header = NULL, bgcolor = "#191a1a") {
  div(
    class = "card",
    header, 
    div(class = "card-content", ..., style = sprintf("background-color: %s", bgcolor))
  )
}
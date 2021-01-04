#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  list(mod_catch_prob_server("catch_prob_ui_1"),
       mod_player_card_server("player_card_ui_1"))
}

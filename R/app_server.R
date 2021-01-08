#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  list(mod_player_card_server("player_card_ui_1"),
       mod_overall_rankings_server('overall_rankings_ui_1'))
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    navbarPage(
      shinyWidgets::setBackgroundColor(
        color = "ghostwhite",
        gradient = c("linear", "radial"),
        direction = c("bottom", "top", "right", "left"),
        shinydashboard = FALSE
      ),
      tabPanel(
        "Player Cards",
        mod_player_card_ui("player_card_ui_1")
      ),
      tabPanel(
        'Overall Rankings',
        mod_overall_rankings_ui('overall_rankings_ui_1')
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
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "bdb2021shiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

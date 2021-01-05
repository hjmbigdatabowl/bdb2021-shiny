#' player_card UI function
#'
#' @description A shiny module for displaying player cards
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
`%>%` <- magrittr::`%>%`

mod_player_card_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      fluid = T,
      sidebarPanel(
        shinyjs::useShinyjs(),
        tags$head(
          tags$style(
            HTML("#shiny-notification-panel {
                  top: 10%;
                  bottom: unset;
                  left: 30%;
                  right: 0;
                  margin-left: auto;
                  margin-right: auto;
                  width: 100%;
                  max-width: 450px;}")
          )
        ),
        shinyWidgets::pickerInput(NS(id, "playerDropdown"),
                                  label = "Players",
                                  choices = c(),
                                  multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(maxOptions = 1, liveSearch = T)
        ),
        actionButton(NS(id, "selectrender"), label = "Select Player", width = "100%", style = "margin-bottom:10px"),
        # uiOutput("playerDropdown")
      ),
      mainPanel(
        plotOutput(ns("gg"))
      )
    )
  )}

#' player_card Server Function
#'
#' @noRd
mod_player_card_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      df <- load_player_card_data()
      shinyWidgets::updatePickerInput(session, "playerDropdown", choices = df$dropdownName, label = "Players")

      data <- reactiveValues()

      observeEvent(input$selectrender, {
        tryCatch(
          {
            if (is.na(input$playerDropdown)) {
              showNotification("You must select a player", duration = 5, type = "error")
            }
            data[["playerId"]] <- df %>% dplyr::filter(.data$dropdownName == input$playerDropdown) %>% dplyr::pull(nflId)

            plt <- build_player_card(df, data[["playerId"]])
            output$gg <- renderPlot(plt)
          },
          error = function(err) {
            message(err)
            showNotification("You must select a type of output to render", type = "error", duration = 5)
          }
        )
      })
    }
  )
}

#' load_player_card_data function to build data used in cards
#' @importFrom magrittr %>%
#'
#'
load_player_card_data <- function(){
  `%>%` <- magrittr::`%>%`
  engine <- bdb2021::connect_to_heroku_postgres()

  catch_throw_agg <- engine %>%
    dplyr::tbl('drops_added_throw') %>%
    dplyr::rename(plays_throw = plays) %>%
    dplyr::collect()

  catch_arrival_agg <- engine %>%
    dplyr::tbl('drops_added_arrival') %>%
    dplyr::rename(plays_arrival = plays) %>%
    dplyr::collect()

  target_agg <- engine %>%
    dplyr::tbl('target_data_aggregated') %>%
    dplyr::collect()


  speed_dat <- engine %>%
    dplyr::tbl('speed_summary') %>%
    dplyr::collect() %>%
    dplyr::select(-.data$plays)

  df <- target_agg %>%
    dplyr::filter(plays > 50) %>%
    dplyr::left_join(catch_throw_agg %>% dplyr::select(.data$nflId, .data$plays_throw, .data$drops_added_throw),
                     by = "nflId") %>%
    dplyr::left_join(catch_arrival_agg %>% dplyr::select(.data$nflId, .data$plays_arrival, .data$drops_added_arrival),
                     by = "nflId") %>%
    dplyr::left_join(speed_dat, by="nflId") %>%
    dplyr::mutate(regressedDropsThrow = .data$drops_added_throw / .data$plays_throw,
                  regressedDropsArrival = .data$drops_added_arrival / .data$plays_arrival,
                  dropdownName = paste(.data$position, " ", .data$displayName, " (", .data$defendingTeam, ")", sep=""))

  summary_stats <- df %>%
    dplyr::group_by(position) %>%
    dplyr::slice_max(.data$plays_throw, n=120) %>%
    dplyr::summarise(meanCoverage = mean(regressedCoverage),
                     sdCoverage = sd(regressedCoverage),
                     meanDeterrence = mean(regressedDeterrence),
                     sdDeterrence = sd(regressedDeterrence),
                     meanDropsThrow = mean(regressedDropsThrow, na.rm = T),
                     sdDropsThrow = sd(regressedDropsThrow, na.rm = T),
                     meanDropsArrival = mean(regressedDropsArrival, na.rm = T),
                     sdDropsArrival = sd(regressedDropsArrival, na.rm = T),
                     .groups = 'drop')

  df <- df %>%
    dplyr::inner_join(summary_stats, by="position") %>%
    dplyr::mutate(
      coverageZ = (regressedCoverage - meanCoverage) / sdCoverage,
      coverageGrade = 100 * pnorm(.data$coverageZ),
      deterrenceZ = (regressedDeterrence - meanDeterrence) / sdDeterrence,
      deterrenceGrade = 100 * pnorm(.data$deterrenceZ),
      dropsThrowZ = (regressedDropsThrow - meanDropsThrow) / sdDropsThrow,
      dropsThrowGrade = 100 * pnorm(.data$dropsThrowZ),
      dropsArrivalZ = (regressedDropsArrival - meanDropsArrival) / sdDropsArrival,
      dropsArrivalGrade = 100 * pnorm(.data$dropsArrivalZ),
      totalGrade = (.data$coverageGrade + .data$deterrenceGrade + .data$dropsThrowGrade + .data$dropsArrivalGrade) / 4,
      totalGrade = 100 * pnorm((.data$totalGrade - mean(.data$totalGrade, na.rm = T)) / sd(.data$totalGrade, na.rm = T))
    ) %>%
    dplyr::select(-(meanCoverage:sdDropsArrival)) %>%
    dplyr::filter(plays > 200, position == "DB")

  rm(summary_stats)
  return(df)
}

#' build_player_card function to create plots for players
#'
#' @param df all data
#' @param player_id id of player
#'
build_player_card <- function(df, player_id) {
  library(patchwork)  # BOOOOOOOOOOOOOOOOOOOO

  player_row <- df %>% dplyr::filter(player_id == .data$nflId)

  player_bio_geom <- build_player_bio(player_row)
  radar_geom <- build_player_radar(df, player_id)
  percentile_goem <- build_percentile_geom(player_row)
  team_logo <- get_team_logo(player_row %>% dplyr::pull(.data$defendingTeam))

  plot_layot <- "
  DAAA#C
  BBBBBC
  BBBBBC
  BBBBBC
  BBBBBC
  "

  return(player_bio_geom
         + radar_geom
         + percentile_goem
         + team_logo
         + plot_layout(design = plot_layot)
         + plot_annotation(caption="Inspired by player cards from evolving-hockey.com; data courtesy NFL"))
}

#' build_percentile_geom take the player grade percentiles and build a geom to display ratings boxes
#'
#' @param player_row row of data corresponding to the player
#'
build_percentile_geom <- function(player_row){
  library(patchwork)

  total_grade_geom <- build_rating_box(player_row %>% dplyr::pull(totalGrade), "Overall")
  coverage_grade_geom <- build_rating_box(player_row %>% dplyr::pull(coverageGrade), "Coverage")
  deterrence_grade_geom <- build_rating_box(player_row %>% dplyr::pull(deterrenceGrade), "Deterrence")
  drops_arrival_grade_geom <- build_rating_box(player_row %>% dplyr::pull(dropsArrivalGrade), "Breakups")
  drops_throw_grade_geom <- build_rating_box(player_row %>% dplyr::pull(dropsThrowGrade), "Closing")

  return(total_grade_geom + coverage_grade_geom + deterrence_grade_geom + drops_throw_grade_geom + drops_arrival_grade_geom + plot_layout(ncol=1))
}

#' build_rating_box function to create ggplot object given a grade
#'
#' @param rating scaled percentile grade from 0-100
#' @param label string of the box label
#'
build_rating_box <- function(rating, label) {
  return(ggplot2::ggplot() +
           ggplot2::geom_rect(ggplot2::aes(xmin=0, xmax=2, ymin=0, ymax=2, fill=rating), color="black", size=2) +
           ggplot2::scale_fill_gradient2(midpoint = 50, limits=c(0, 100)) +
           ggplot2::geom_text(ggplot2::aes(x=1, y=1, label = round(rating)), size=20) +
           ggplot2::theme_void() +
           ggplot2::labs(title=label) +
           ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(size=16, hjust = 0.5))
  )

}

#' get_team_logo get logo from nflfastR
#'
get_team_logo <- function(team_abbr) {
  team_logo <- nflfastR::teams_colors_logos %>%
    dplyr::filter(.data$team_abbr == team_abbr) %>%
    dplyr::pull(.data$team_logo_espn)

  logo <- cowplot::draw_image(team_logo)
  logo_geom <- cowplot::ggdraw() + logo
  rm(logo)

  return(logo_geom)
}

#' build_player_bio function to create a ggplot object for a players bio information
#'
#' @param player_row record in total data with players information
#'
build_player_bio <- function(player_row) {
  bio_geom <- player_row %>%
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin=0, xmax=2, ymin=0, ymax=2, fill=50)) +
    ggplot2::scale_fill_gradient2(midpoint = 50, limits=c(0, 100)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    # ggplot2::geom_text(ggplot2::aes(x = 1, y = 1.5, label = paste(position, "-", defendingTeam)), size=6) +
    # ggplot2::geom_text(ggplot2::aes(x = 1, y = 1, label = displayName), size=6) +
    # ggplot2::geom_text(ggplot2::aes(x = 1, y = 0.5, label = paste(round(playsTarget), " plays | ", games, " games", sep="")), size=6)
    ggplot2::geom_text(ggplot2::aes(x = 1, y = 1.3, label = paste(position, displayName)), size=8) +
    ggplot2::geom_text(ggplot2::aes(x = 1, y = 0.7, label = paste(round(plays_throw), " plays | ", games, " games", sep="")), size=6)

  return(bio_geom)
}

#' build_player_radar
#'
#' @param df entire dataframe
#' @param player_id player to return radar for
#'
build_player_radar <- function(df, player_id) {
  radar_geom <- df %>%
    dplyr::mutate_each(list(scales::rescale),
                       c(.data$plays_throw, .data$averageSeperation, .data$wrStrength, .data$medianSpeed, .data$medianAccel)) %>%
    dplyr::mutate(wrStrength = 1 - .data$wrStrength,
                  averageSeperation = 1 - .data$averageSeperation) %>%
    dplyr::filter(.data$nflId == player_id) %>%
    dplyr::select(.data$nflId,
                  Plays = .data$plays_throw,
                  Speed = .data$medianSpeed,
                  Accel = .data$medianAccel,
                  `Asmgt Diff` = .data$wrStrength,
                  `WR Sep` = .data$averageSeperation) %>%
    ggradar::ggradar() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

  return(radar_geom)
}

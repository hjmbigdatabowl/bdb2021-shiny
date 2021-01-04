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
df <- load_player_card_data()

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
        shinyWidgets::pickerInput(NS(id, "playerid"),
                                  label = "Output",
                                  choices = sorted(df$dropdownName),
                                  multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(maxOptions = 1)
        ),

        actionButton(NS(id, "loadmodel"), label = "Load model", width = "100%", style = "margin-bottom:10px"),
        shinyWidgets::pickerInput(NS(id, "rendertype"),
                                  label = "Render",
                                  choices = c("Plot", "Table"),
                                  multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(maxOptions = 1)
        ),
        actionButton(NS(id, "selectrender"), label = "Finalize Selection", width = "100%", style = "margin-bottom:10px"),
        uiOutput("xaxis"),
        uiOutput("yaxis"),
        uiOutput("plottype"),
        uiOutput("plotcolor"),
        uiOutput("variables"),
        actionButton(NS(id, "render"), "Render", width = "100%", style = "margin-bottom:10px")
      ),
      mainPanel(
        DT::dataTableOutput(ns("tab")),
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

      shinyWidgets::updatePickerInput(session, "cols_to_select", choices = sort(colnames(df)), label = "Variables")
    }
  )
}

#' load_player_card_data function to build data used in cards
#' @importFrom magrittr %>%
#'
#'
load_player_card_data <- function(){
  `%>%` <- magrittr::`%>%`

  # catch_agg <- bdb2021::load_encrypted("model_data/drops_added.Rdata")
  catch_agg <- NULL
  load("model_data/drops_added.Rdata")
  catch_agg <- results
  rm(results)

  target_agg <- bdb2021::load_encrypted("model_data/target_aggregated.Rdata")

  df <- target_agg %>%
    dplyr::left_join(catch_agg %>% dplyr::select(.data$nflId, .data$plays, .data$drops_added),
                     by = "nflId",
                     suffix = c("Target", "Drop")) %>%
    dplyr::mutate(regressedDrops = .data$drops_added / .data$playsDrop,
                  dropdownName = paste(.data$position, " ", .data$displayName, " (", .data$defendingTeam, ")", sep=""))

  summary_stats <- df %>%
    dplyr::group_by(position) %>%
    dplyr::slice_max(.data$playsTarget, n=120) %>%
    dplyr::summarise(meanCoverage = mean(regressedCoverage),
                     sdCoverage = sd(regressedCoverage),
                     meanDeterrence = mean(regressedDeterrence),
                     sdDeterrence = sd(regressedDeterrence),
                     meanDrops = mean(regressedDrops),
                     sdDrops = sd(regressedDrops))

  df <- df %>%
    dplyr::inner_join(summary_stats, by="position") %>%
    dplyr::mutate(
      coverageZ = (regressedCoverage - meanCoverage) / sdCoverage,
      coverageGrade = 100 * pnorm(.data$coverageZ),
      deterrenceZ = (regressedDeterrence - meanDeterrence) / sdDeterrence,
      deterrenceGrade = 100 * pnorm(.data$deterrenceZ),
      dropsZ = (regressedDrops - meanDrops) / sdDrops,
      dropsGrade = 100 * pnorm(.data$dropsZ),
      totalGrade = (coverageGrade + deterrenceGrade + dropsGrade) / 3
    ) %>%
    dplyr::select(-(meanCoverage:sdDrops))

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

  plot_layot <- "
  AAAAC
  BBBBC
  BBBBC
  BBBBC
  BBBBC
  "

  return(player_bio_geom + radar_geom + percentile_goem + plot_layout(design = plot_layot))
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
  drops_grade_geom <- build_rating_box(player_row %>% dplyr::pull(dropsGrade), "Drops")

  return(total_grade_geom + coverage_grade_geom + deterrence_grade_geom + drops_grade_geom + plot_layout(ncol=1))
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
    ggplot2::geom_text(ggplot2::aes(x = 1, y = 1.5, label = paste(position, "-", defendingTeam)), size=6) +
    ggplot2::geom_text(ggplot2::aes(x = 1, y = 1, label = displayName), size=6) +
    ggplot2::geom_text(ggplot2::aes(x = 1, y = 0.5, label = paste(round(playsTarget), " plays | ", games, " games", sep="")), size=6)

}

#' build_player_radar
#'
#' @param df entire dataframe
#' @param player_id player to return radar for
#'
build_player_radar <- function(df, player_id) {
  radar_geom <- df %>%
    dplyr::mutate_each(list(scales::rescale),
                       c(.data$playsTarget, .data$coverageTargetsAdded, .data$deterrenceTargetsAdded, .data$drops_added)) %>%
    dplyr::filter(.data$nflId == player_id) %>%
    dplyr::select(.data$nflId,
                  plays = .data$playsTarget,
                  coverage = .data$coverageTargetsAdded,
                  deterrence = .data$deterrenceTargetsAdded,
                  drops = .data$drops_added) %>%
    ggradar::ggradar()

  return(radar_geom)
}

#' target_prob UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
`%>%` <- magrittr::`%>%`

mod_catch_prob_ui <- function(id) {
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
        shinyWidgets::pickerInput(NS(id, "outtype"),
          label = "Output",
          choices = c("Rankings", "Features"),
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
  )
}

#' target_prob Server Function
#'
#' @noRd
mod_catch_prob_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      df <- NULL
      ns <- session$ns

      model_data <- reactiveValues()

      observeEvent(input$loadmodel, {
        tryCatch(
          {
            if (is.na(input$outtype)) {
              showNotification("You must select an output type", duration = 5, type = "error")
            }

            if (input$outtype == "Rankings") {
              load("model_data/drops_added.Rdata")
              df <- results %>%
                dplyr::arrange(dplyr::desc(drops_added))
              rm(results)
            } else {
              ## change this
              load("model_data/catch_prob_preds.Rdata")
              df <- bdb2021::load_encrypted("model_data/catch_prob_features.Rdata") %>%
                dplyr::mutate(dplyr::across(dplyr::everything(), function(x) ifelse(x == 999, NA_real_, x))) %>%
                dplyr::rowwise() %>%
                dplyr::transmute(
                  gameId = gameId,
                  playId = playId,
                  avg_dist_to_defs = mean(dplyr::c_across(dist_to_def_1:dist_to_def_11), na.rm = T),
                  min_dist_to_nearest_def = min(dplyr::c_across(dist_to_def_1:dist_to_def_11), na.rm = T),
                  defs_within_five_yards = sum(dplyr::c_across(dist_to_def_1:dist_to_def_11) <= 5, na.rm = T),
                  throw_distance = throwdist,
                  throw_velocity = max_throw_velo,
                  receiver_height = height,
                  receiver_skill = skill,
                  conditions = conditions,
                  temperature = temperature,
                  football_y_position_at_pass_arrival = footballYArr,
                  football_x_position_at_pass_arrival = footballXArr,
                  target_x_position_at_throw = targetXThrow,
                  target_y_position_at_throw = targetYThrow,
                  target_speed_at_throw = targetSThrow,
                  target_acceleration_at_throw = targetAThrow,
                  outcome = outcome
                ) %>%
                dplyr::ungroup() %>%
                dplyr::left_join(model_preds %>% dplyr::rename(catch_probability = predprob), by = c("gameId", "playId"))
              ## add more loads
            }

            shinyWidgets::updatePickerInput(session, "cols_to_select", choices = sort(colnames(df)), label = "Variables")
            model_data[["data"]] <- df
            ## add more model data

            showNotification("The model loaded!", duration = 3, type = "message")
          },
          error = function(err) {
            message(err)
            showNotification("You must select a model to load", type = "error", duration = 5)
          }
        )
      })

      observeEvent(input$selectrender, {
        tryCatch(
          {
            if (is.na(input$rendertype)) {
              showNotification("You must select an render type", duration = 5, type = "error")
            }

            removeUI("#xaxis *")
            removeUI("#yaxis *")
            removeUI("#plottype *")
            removeUI("#plotcolor *")
            removeUI("#variables *")

            model_data[["rendertype"]] <- input$rendertype

            if (input$rendertype == "Plot") {
              insertUI(
                selector = "#xaxis",
                where = "beforeEnd",
                ui = selectInput(NS(id, "xaxis"), "X-Axis", choices = c("None", colnames(model_data[["data"]]))),
                session = session
              )
              insertUI(
                selector = "#yaxis",
                where = "beforeEnd",
                ui = selectInput(NS(id, "yaxis"), "Y-Axis", choices = c("None", colnames(model_data[["data"]]))),
                session = session
              )
              insertUI(
                selector = "#plottype",
                where = "beforeEnd",
                ui = selectInput(NS(id, "plottype"), "Plot Type", choices = c("Boxplot", "Scatter")),
                session = session
              )
              insertUI(
                selector = "#plotcolor",
                where = "beforeEnd",
                ui = selectInput(NS(id, "plotcolor"), "Color", choices = c("None", colnames(model_data[["data"]]))),
                session = session
              )
            } else {
              insertUI(
                selector = "#variables",
                where = "beforeEnd",
                ui = shinyWidgets::pickerInput(NS(id, "cols_to_select"),
                  label = "Variables",
                  choices = colnames(model_data[["data"]]),
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `multiple-separator` = ", "
                  )
                ),
                session = session
              )
            }
          },
          error = function(err) {
            message(err)
            showNotification("You must select a type of output to render", type = "error", duration = 5)
          }
        )
      })

      rendered_out <- observeEvent(input$render, {
        tryCatch(
          {
            if (model_data[["rendertype"]] == "Plot") {
              col <- if (input$plotcolor == "None") NULL else input$plotcolor

              ptype <- function(pt) {
                if (pt == "Boxplot") {
                  ggplot2::geom_boxplot()
                } else if (pt == "Scatter") {
                  ggplot2::geom_point()
                }
              }
              plt <- ggplot2::ggplot(model_data[["data"]], ggplot2::aes_string(x = input$xaxis, y = input$yaxis, color = col)) +
                ptype(input$plottype) +
                ggthemes::theme_fivethirtyeight() +
                ggplot2::theme(axis.title = ggplot2::element_text())

              shinyjs::hide('tab')
              shinyjs::show('gg')

              output$gg <- renderPlot(plt)
            } else {
              tab <- model_data[["data"]] %>%
                dplyr::select(input$cols_to_select)

              output$tab <- DT::renderDataTable(tab)
              shinyjs::hide('gg')
              shinyjs::show('tab')
            }
          },
          error = function(err) {
            message(err)
            showNotification(paste0("Error message: ", err), type = "err")
          }
        )
      })
    }
  )
}

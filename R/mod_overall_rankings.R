#' overall_rankings UI function
#'
#' @description A shiny module for displaying player cards
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
`%>%` <- magrittr::`%>%`
message('got to top line')
mod_overall_rankings_ui <- function(id){
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
        shinyWidgets::pickerInput(NS(id, "rendertype2"),
                                  label = "What to Render",
                                  choices = c('Plot', 'Table'),
                                  multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(maxOptions = 1, liveSearch = T)
        ),
        actionButton(NS(id, 'select2'), 'Confirm', width = '100%', style = 'margin-bottom:10px'),
        uiOutput("xaxis2"),
        uiOutput("yaxis2"),
        uiOutput("plottype2"),
        uiOutput("plotcolor2"),
        uiOutput("variables2"),
        actionButton(NS(id, "render"), "Render", width = "100%", style = "margin-bottom:10px")
      ),
      mainPanel(
        plotOutput(ns("gg")),
        DT::dataTableOutput(ns('tab'))
      )
    )
  )}

#' overall_rankings Server Function
#'
#' @noRd
mod_overall_rankings_server <- function(id){
  message('got inside server')
  moduleServer(
    id,
    function(input, output, session){
      message('server started')
      ns <- session$ns

      model_data_2 <- reactiveValues()
      message('data loading...')
      model_data_2[['data']] <- load_player_card_data()
      message('data loaded')

      observeEvent(input$select2, {
        tryCatch(
          {
            if (is.na(input$rendertype2)) {
              showNotification("You must select something to render", duration = 5, type = "error")
            }
            message('got inside select click')
            removeUI("#xaxis2 *")
            removeUI("#yaxis2 *")
            removeUI("#plottype2 *")
            removeUI("#plotcolor2 *")
            removeUI("#variables2 *")
            message(input$rendertype2)
            model_data_2[["rendertype2"]] <- input$rendertype2
            message('updated reactive')
            if (input$rendertype2 == "Plot") {
              message('got to insertUI')
              insertUI(
                selector = "#xaxis2",
                where = "beforeEnd",
                ui = selectInput(NS(id, "xaxis2"), "X-Axis", choices = c("None", colnames(model_data_2[["data"]]))),
                session = session
              )
              insertUI(
                selector = "#yaxis2",
                where = "beforeEnd",
                ui = selectInput(NS(id, "yaxis2"), "Y-Axis", choices = c("None", colnames(model_data_2[["data"]]))),
                session = session
              )
              insertUI(
                selector = "#plottype2",
                where = "beforeEnd",
                ui = selectInput(NS(id, "plottype2"), "Plot Type", choices = c("Boxplot", "Scatter")),
                session = session
              )
              insertUI(
                selector = "#plotcolor2",
                where = "beforeEnd",
                ui = selectInput(NS(id, "plotcolor2"), "Color", choices = c("None", colnames(model_data_2[["data"]]))),
                session = session
              )
              message('got through UI inserts')
            } else {
              insertUI(
                selector = "#variables2",
                where = "beforeEnd",
                ui = shinyWidgets::pickerInput(NS(id, "cols_to_select2"),
                                               label = "Variables",
                                               choices = colnames(model_data_2[["data"]]),
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
            message(model_data_2[['rendertype2']])
            if (model_data_2[["rendertype2"]] == "Plot") {
              col <- if (input$plotcolor2 == "None") NULL else input$plotcolor2
              message('got through color')
              ptype <- function(pt) {
                if (pt == "Boxplot") {
                  ggplot2::geom_boxplot()
                } else if (pt == "Scatter") {
                  ggplot2::geom_jitter()
                }
              }
              plt <- withProgress(
                message = 'Rendering plot...',
                {
                  ggplot2::ggplot(model_data_2[["data"]], ggplot2::aes_string(x = input$xaxis2, y = input$yaxis2, color = col)) +
                  ptype(input$plottype2) +
                  ggthemes::theme_fivethirtyeight() +
                  ggplot2::theme(axis.title = ggplot2::element_text())
                })
              message('got through plt')
              shinyjs::hide('tab')
              shinyjs::show('gg')

              output$gg <- renderPlot(plt)
            } else {
              tab <- model_data_2[["data"]] %>%
                dplyr::select(input$cols_to_select2) %>%
                dplyr::mutate(dplyr::across(dplyr::ends_with('Grade'), function(x) round(x, digits = 0)))

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

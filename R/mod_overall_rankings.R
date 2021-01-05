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
        shinyWidgets::pickerInput(NS(id, "rendertype"),
                                  label = "What to Render",
                                  choices = c('Plot', 'Table'),
                                  multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(maxOptions = 1, liveSearch = T)
        ),
        actionButton(NS(id, 'select'), 'Confirm', width = '100%', style = 'margin-bottom:10px'),
        uiOutput("xaxis"),
        uiOutput("yaxis"),
        uiOutput("plottype"),
        uiOutput("plotcolor"),
        uiOutput("variables"),
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
  moduleServer(
    id,
    function(input, output, session){
      message('server started')
      ns <- session$ns

      model_data <- reactiveValues()
      message('data loading...')
      model_data[['data']] <- load_player_card_data()
      message('data loaded')

      observeEvent(input$select, {
        tryCatch(
          {
            if (is.na(input$rendertype)) {
              showNotification("You must select something to render", duration = 5, type = "error")
            }
            message('got inside select click')
            removeUI("#xaxis *")
            removeUI("#yaxis *")
            removeUI("#plottype *")
            removeUI("#plotcolor *")
            removeUI("#variables *")
            message(input$rendertype)
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

      rendered_out <- observeEvent(input$select, {
        tryCatch(
          {
            if (model_data[["rendertype"]] == "Plot") {
              col <- if (input$plotcolor == "None") NULL else input$plotcolor

              ptype <- function(pt) {
                if (pt == "Boxplot") {
                  ggplot2::geom_boxplot()
                } else if (pt == "Scatter") {
                  ggplot2::geom_jitter()
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

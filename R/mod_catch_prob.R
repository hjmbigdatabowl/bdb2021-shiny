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

mod_catch_prob_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      fluid = T,
      sidebarPanel(
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
        shinyWidgets::pickerInput(NS(id, "mod"),
                                  label = "Model",
                                  choices = c('Target Model', 'Catch Model'),
                                  multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(maxOptions = 1)),
        shinyWidgets::pickerInput(NS(id, "outtype"),
                                  label = "Output",
                                  choices = c('Rankings', 'Features'),
                                  multiple = TRUE,
                                  options = shinyWidgets::pickerOptions(maxOptions = 1)),
        actionButton(NS(id, 'loadmodel'), label = "Load model", width = '100%', style = 'margin-bottom:10px'),
        shinyWidgets::pickerInput(NS(id, "cols_to_select"),
                                  label = "Features",
                                  choices = "Please select a model first",
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE,
                                                 `multiple-separator` = ", ")),
        actionButton(NS(id, "rendertable"), "Render Table", width = '100%', style = 'margin-bottom:10px')
      ),
      mainPanel(
        gt::gt_output(ns("featuretab"))
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
    function(input, output, session){
      df <- NULL
      ns <- session$ns

      model_data <- reactiveValues()

      observeEvent(input$loadmodel, {
        tryCatch({
          if (is.na(input$mod)) {
            showNotification('You must select a model to load', duration = 5, type = 'error')
          } else if (is.na(input$outtype)) {
            showNotification('You must select an output type', duration = 5, type = 'error')
          }

          if (input$mod == 'Target Model' & input$outtype == 'Rankings') {
            df <- load_encrypted('model_data/catch_prob_features.Rdata')
          } else if (input$mod == 'Target Model' & input$outtype == 'Features') {
            df <- load_encrypted('model_data/catch_prob_features.Rdata')
          } else if (input$mod == 'Catch Model' & input$outtype == 'Rankings') {
            load('model_data/drops_added.Rdata')
            df <- results
            rm(results)
          } else {
            ## change this
            df <- load_encrypted('model_data/catch_prob_features.Rdata')
            ## add more loads
          }

          shinyWidgets::updatePickerInput(session, 'cols_to_select', choices = sort(colnames(df)), label = "Features")
          model_data[['features']] <- df
          ## add more model data

          showNotification("The model loaded!", duration = 3, type = "message")
        },
        error = function(err){
          message(err)
          showNotification('You must select a model to load', type = 'error', duration = 5)
        })
      })



      rendered_table <- observeEvent(input$rendertable, {
        tryCatch({

          if (length(input$cols_to_select) == 0) {
            showNotification('You must add at least one school to your list', duration = 5, type = 'error')
          }

          tab <- model_data[['features']] %>%
            dplyr::select(input$cols_to_select) %>%
            gt::gt() %>%
            bdb2021::gt_theme_538()

          output$featuretab <- gt::render_gt(tab)
        },
        error = function(err){
          message(err)
          showNotification(paste0("Error message: ", err), type = 'err')
        })

      })
    }
  )
}



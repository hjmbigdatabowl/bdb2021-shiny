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
load('model_data/sodium_key.Rdata')
key <- cyphr::key_sodium(k)

load_encrypted <- function(file, key) {
  cyphr::decrypt(load(file, envir = globalenv()), key)
}

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
        actionButton(NS(id, 'loadmodel'), label = "Load model", width = '100%', style = 'margin-bottom:10px'),
        shinyWidgets::pickerInput(NS(id, "feats"),
                                  label = "Features",
                                  choices = "Please select a model first",
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE,
                                                 `multiple-separator` = ", ")),
        actionButton(NS(id, "rendertable"), "Render Table", width = '100%', style = 'margin-bottom:10px')
      ),
      mainPanel(
        DT::dataTableOutput(ns("featuretab"))
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
          }

          if (input$mod == 'Target Model') {
            load_encrypted('model_data/catch_prob_features.Rdata', key)
            ## add more loads
          } else {
            ## change this
            load_encrypted('model_data/catch_prob_features.Rdata', key)
            ## add more loads
          }

          shinyWidgets::updatePickerInput(session, 'feats', choices = sort(colnames(df)), label = "Features")
          model_data[['features']] <- df
          ## add more model data

          showNotification("The model loaded!", duration = 3, type = "message")
        },
        error = function(err){
          showNotification('You must select a model to load', type = 'error', duration = 5)
        })
      })



      rendered_table <- observeEvent(input$rendertable, {
        tryCatch({

          if (length(input$feats) == 0) {
            showNotification('You must add at least one school to your list', duration = 5, type = 'error')
          }



          tab <- df %>%
            dplyr::select(input$feats)

          output$featuretab <- DT::renderDataTable(tab)
        },
        error = function(err){
          showNotification(paste0("Error message: ", err), type = 'err')
        })

      })
    }
  )
}



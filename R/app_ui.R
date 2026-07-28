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
    # Your application UI logic
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          class = "HaDeX-tab-content-element",
          br(),
          img(src='./www/logo.png', width = "40%", align = "center"),
          br(),
          br(),
          br(),
          div(
            fileInput(inputId = ("experimental_data"),
                      label = "Experimental data:"),
            fileInput(inputId = ("fit_params"),
                      label = "Fit parameters:"),
            "Fit params are in `fit params` tab from HRaDeX."
          ),
          div(
            mod_settings_ui("settings")
          )
          
        ),
        mainPanel(
          tabsetPanel( 
            tabPanel("Overview",
                     mod_overview_ui("overview")
                     
                     
            ),
            tabPanel("Peptides",
                     mod_peptides_ui("peptides")
                     
                     
            )
          )
          
        )
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
    "www",
    app_sys("app/www")
  )

  library(HRaDeX)
  library(ggiraph)
  library(ggplot2)
  library(dplyr)
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ctrlHRaDeX"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("time_0"),
                label = "Select no deut timepoint",
                choices = c(0, 0.1),
                selected = 0),
    selectInput(inputId = ns("time_100"),
                label = "Select FD timepoint",
                choices = c(1000, 1440),
                selected = 1440),
    checkboxInput(inputId = ns("is_fractional"),
                  label = "Fitting was done for fractional values?",
                  value = TRUE),
    selectInput(input = ns("agg_method"),
                label = "Select method of aggregation",
                choices = c("shortest", "weighted"),
                selected = "weighted")
 
  )
}
    
#' settings Server Functions
#'
#' @noRd 
mod_settings_server <- function(id, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    times <- reactive({ unique(dat()[["Exposure"]])})
    
    observe({
      
      updateSelectInput(session = session, 
                        inputId = "time_0",
                        choices = times(),
                        selected = min(times()))
    })
    
    observe({
      
      updateSelectInput(session = session, 
                        inputId = "time_100",
                        choices = times(),
                        selected = max(times()))
    })
 
    return(
      reactive(
      data.frame(
        time_0 = as.numeric(input[["time_0"]]),
        time_100 = as.numeric(input[["time_100"]]),
        agg_method = input[["agg_method"]],
        is_fractional = input[["is_fractional"]]
      )
     )
    ) 
    
  })
  
 
}
    
## To be copied in the UI
# mod_settings_ui("settings_1")
    
## To be copied in the server
# mod_settings_server("settings_1")

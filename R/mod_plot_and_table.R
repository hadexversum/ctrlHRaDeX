#' plot_and_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_and_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel( 
      tabPanel("Plot",
               ggiraph::girafeOutput(ns("plot"), width = "80%")
      ),
      tabPanel("Data",
               DT::dataTableOutput(ns("data"))
      )
    )
 
  )
}
    
#' plot_and_table Server Functions
#'
#' @noRd 
mod_plot_and_table_server <- function(id, plt, dat){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    
    output[["data"]] <- DT::renderDataTable({
      
      nicer_table(dat())
      
    })
    
    output[["plot"]] <- ggiraph::renderGirafe({
      
      if(any(class(plt()) == "girafe")) {
        plt()
      } else {
        girafe(ggobj = plt())
        
      }
      
    })
  })
}
    
## To be copied in the UI
# mod_plot_and_table_ui("plot_and_table_1")
    
## To be copied in the server
# mod_plot_and_table_server("plot_and_table_1")

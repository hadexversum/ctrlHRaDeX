#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ggiraph::girafeOutput(outputId = ns("hires_plot"), width = "80%"),
    ggiraph::girafeOutput(outputId = ns("validation"), width = "80%"),
    textOutput(outputId = ns("rmse_stat")),
    ggiraph::girafeOutput(outputId = ns("histogram"), width = "80%")
    
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, kin_dat, hires_dat, fit_params){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output[["hires_plot"]] <- ggiraph::renderGirafe({
      
      HRaDeX::plot_hires(hires_dat(), 
                         interactive = TRUE)
    })
 
 
    
    
    rec_uc_dat_alpha <- reactive({
      
      # browser()
      
      HRaDeX::create_uc_from_hires_dataset(kin_dat(),
                                   fit_params(),
                                   hires_method = "weighted")
      
  
    
    })
    
    rec_uc_rmse_dat_alpha <- reactive({
      
     
    
      HRaDeX::calculate_recovered_uc_rmse(rec_uc_dat_alpha(), sort = "ID")
    
    })
    
    rmse_mean <- reactive({ mean(rec_uc_rmse_dat_alpha()[["rmse"]]) })
    
    
    rmse_median <- reactive({ median(rec_uc_rmse_dat_alpha()[["rmse"]]) })
    
    output[["rmse_stat"]] <- renderText({
      
      
      paste0("Mean RMSE: ", round(rmse_mean(), 4), " and median RMSE: ", round(rmse_median(), 4))
      
    })

    output[["validation"]] <- ggiraph::renderGirafe({

      girafe(ggobj = HRaDeX::plot_recovered_uc_coverage(rec_uc_rmse_dat_alpha(), 
                                                        interactive = TRUE,
                                                        style = "coverage"))
      
    })
    
    output[["histogram"]] <- ggiraph::renderGirafe({
      
      plt <- ggplot(rec_uc_rmse_dat_alpha(), aes(rmse)) +
        geom_histogram()
      
      girafe(ggobj = plt)
      
    })
    
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")

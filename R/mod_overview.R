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
    ggiraph::girafeOutput(outputId = ns("histogram"), width = "80%"),
    mod_plot_and_table_ui(ns("hist_plot"))
    
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, kin_dat, hires_dat, fit_params, settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output[["hires_plot"]] <- ggiraph::renderGirafe({
      
      HRaDeX::plot_hires(hires_dat(), 
                         interactive = TRUE)
    })
 
 
    
    
    rec_uc_dat_alpha <- reactive({
      
      HRaDeX::create_uc_from_hires_dataset(kin_dat(),
                                           fit_params(),
                                           hires_method = settings()[["agg_method"]])
      
  
    
    })
    
    rec_uc_rmse_dat_alpha <- reactive({
      
      HRaDeX::calculate_recovered_uc_rmse(rec_uc_dat_alpha(), sort = "ID")
    
    })
    
    rmse_mean <- reactive({ mean(rec_uc_rmse_dat_alpha()[["rmse"]], na.rm = TRUE) })
    
    
    rmse_median <- reactive({ median(rec_uc_rmse_dat_alpha()[["rmse"]], na.rm = TRUE) })
    
    output[["rmse_stat"]] <- renderText({
      
      
      paste0("Mean RMSE: ", round(rmse_mean(), 4), " and median RMSE: ", round(rmse_median(), 4))
      
    })

    output[["validation"]] <- ggiraph::renderGirafe({

      girafe(ggobj = HRaDeX::plot_recovered_uc_coverage(rec_uc_rmse_dat_alpha(), 
                                                        interactive = TRUE,
                                                        style = "coverage"))
      
    })
    

    
    histogram_plot <- reactive({
      
      ggplot(rec_uc_rmse_dat_alpha(), aes(rmse)) +
        geom_histogram()
      
    })
    
    histogram_data <- reactive({
      
      mutate(rec_uc_rmse_dat_alpha(), 
             rmse = round(rmse, 4))
    })
    
    mod_plot_and_table_server("hist_plot",
                              plt = histogram_plot,
                              dat = histogram_data)
    
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")

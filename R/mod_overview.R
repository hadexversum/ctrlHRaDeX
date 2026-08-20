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
    mod_plot_and_table_ui(ns("hires")),
    mod_plot_and_table_ui(ns("rmse")),
    textOutput(outputId = ns("rmse_stat")),
    ggiraph::girafeOutput(outputId = ns("histogram"), width = "80%"),
    
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, kin_dat, hires_dat, fit_params, settings, fit_state){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    hires_plot <- reactive({
      
      # browser()
      HRaDeX::plot_hires(hires_dat(), 
                         interactive = TRUE)
    })
    
    output[["hires_plot"]] <- ggiraph::renderGirafe({
      
      hires_plot()
      
    })
 
    nice_hires_dat <- reactive({
      
      hires_dat() %>%
        mutate(k_est = round(k_est, 6),
               n_1 = round(n_1, 6),
               n_2 = round(n_2, 6),
               n_3 = round(n_3, 6),
               k_1 = round(k_1, 6),
               k_2 = round(k_2, 6),
               k_3 = round(k_3, 6))
      
    }) 
    
    mod_plot_and_table_server(id = "hires", 
                              plt = hires_plot, 
                              dat = nice_hires_dat)
    
    
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
    
    rmse_plot <- reactive({
      
      HRaDeX::plot_recovered_uc_coverage(rec_uc_rmse_dat_alpha(), 
                                         interactive = TRUE,
                                         style = "coverage") +
               labs(title = paste0("RMSE of recovered UC for ", fit_state(), " state"))
      
    }) 

    mod_plot_and_table_server("rmse",
                              plt = rmse_plot,
                              dat = nicer_rmse_dat)
    
    
    nicer_rmse_dat <- reactive({
      
      mutate(rec_uc_rmse_dat_alpha(), 
             rmse = round(rmse, 6))
    })

    output[["histogram"]] <- ggiraph::renderGirafe({
      
      
      girafe(ggobj = ggplot(rec_uc_rmse_dat_alpha(), aes(rmse)) +
               geom_histogram())
    })
    
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")

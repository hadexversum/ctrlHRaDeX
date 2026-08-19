#' peptides UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_peptides_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("peptide_list")),
    ggiraph::girafeOutput(ns("peptide_uc"), width = "80%"),
    DT::dataTableOutput(ns("peptide_fit_params"))
    # ggiraph::girafeOutput(ns("peptide_uc_2"), width = "80%")
  )
}
    
#' peptides Server Functions
#'
#' @noRd 
mod_peptides_server <- function(id, kin_dat, fit_params, settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    
    peptide_list <- reactive({
      
      unique(select(kin_dat(), Sequence, Start, End))
      
    })
    
    output[["peptide_list"]] <- DT::renderDataTable({
      
      nicer_table(tbl_dat = peptide_list(), 
                  filename = "peptides",
                  selection = "single")
      
    }, server = FALSE)
    
    pep_kin_dat <- reactive({
      
      kin_dat() %>%
        filter(Sequence == peptide_list()[input[["peptide_list_rows_selected"]], 1],
               Start ==  peptide_list()[input[["peptide_list_rows_selected"]], 2],
               End == peptide_list()[input[["peptide_list_rows_selected"]], 3])
      
     
      
    })
    
    # output[["peptide_uc"]] <- ggiraph::renderGirafe({
    #   
    #   validate(need(!is.null(input[["peptide_list_rows_selected"]]), "Select peptide to see its uptake curve."))
    #   
    #   girafe(ggobj = HRaDeX::recreate_uc(fit_dat = pep_kin_dat(), 
    #                                      fit_values_all = fit_params(), 
    #                                      hires_method = settings()[["agg_method"]],
    #                                      interactive = TRUE))
    #   
    # })
    
    hires_dat <- reactive({
      
      calculate_hires(fit_values = fit_params(), method = settings()[["agg_method"]], fractional = TRUE)
      
    })
    
    peptide_recovered_fit_params <- reactive({
      
      recreate_fit_values(peptide_sequence = peptide_list()[input[["peptide_list_rows_selected"]], 1],
                          peptide_start = peptide_list()[input[["peptide_list_rows_selected"]], 2], 
                          peptide_end = peptide_list()[input[["peptide_list_rows_selected"]], 3],
                          hires_dat = hires_dat())
    })
    
    peptide_fit_params <- reactive({
      
      fit_params() %>%
        filter(sequence == peptide_list()[input[["peptide_list_rows_selected"]], 1],
               start ==  peptide_list()[input[["peptide_list_rows_selected"]], 2],
               end == peptide_list()[input[["peptide_list_rows_selected"]], 3])
      
      
    })
    
    output[["peptide_uc"]] <- ggiraph::renderGirafe({
      
      validate(need(!is.null(input[["peptide_list_rows_selected"]]), "Select peptide to see its uptake curve."))
      
      HRaDeX::plot_recreated_uc(fit_dat = pep_kin_dat(),
                                fit_values = peptide_fit_params(),
                                recreated_fit_values = peptide_recovered_fit_params(),
                                hires_method = settings()[["agg_method"]],
                                if_girafe = TRUE, 
                                interactive = TRUE)
      
    })
    
    output[["peptide_fit_params"]] <- DT::renderDataTable({
      
      validate(need(!is.null(input[["peptide_list_rows_selected"]]), "Select peptide to see its uptake curve."))
      
      # browser()
      
      og <- peptide_fit_params() %>%
        mutate(type = "original parameters") %>%
        select(type, n_1, k_1, n_2, k_2, n_3, k_3) 
      
      rec <- peptide_recovered_fit_params() %>%
        mutate(type = "recovered parameters") %>%
        select(type, n_1, k_1, n_2, k_2, n_3, k_3)
      
      bind_rows(og, rec) %>%
        mutate(n_1 = round(n_1, 6),
               n_2 = round(n_2, 6),
               n_3 = round(n_3, 6),
               k_1 = round(k_1, 6),
               k_2 = round(k_2, 6),
               k_3 = round(k_3, 6)) %>%
        nicer_table(.,
                    filename = "fit_params",
                    selection = "none")
        
    })
    
  
    
    
  })
}
    
## To be copied in the UI
# mod_peptides_ui("peptides_1")
    
## To be copied in the server
# mod_peptides_server("peptides_1")

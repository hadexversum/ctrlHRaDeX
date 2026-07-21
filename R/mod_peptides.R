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
    ggiraph::girafeOutput(ns("peptide_uc"), width = "80%")
  )
}
    
#' peptides Server Functions
#'
#' @noRd 
mod_peptides_server <- function(id, kin_dat, fit_params){
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
    
    output[["peptide_uc"]] <- ggiraph::renderGirafe({
      
      validate(need(!is.null(input[["peptide_list_rows_selected"]]), ""))
      
      girafe(ggobj = HRaDeX::recreate_uc(fit_dat = pep_kin_dat(), 
                                         fit_values_all = fit_params(), 
                                         hires_method = "weighted",
                                         interactive = TRUE))
      
    })
    
  })
}
    
## To be copied in the UI
# mod_peptides_ui("peptides_1")
    
## To be copied in the server
# mod_peptides_server("peptides_1")

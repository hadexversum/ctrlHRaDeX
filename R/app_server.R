#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  
  exp_dat <- reactive({
    
    data_file <- input[["experimental_data"]]
    
    if(is.null(data_file)){
      HaDeX2::read_hdx("./data/alpha_cut.csv") %>%
        filter(State == "Alpha_KSCN")
    } else {
      validate(need(try({
        file <- HaDeX2::read_hdx(data_file[["datapath"]])
      }), "File does not fullfill requirements. Check file requirements!"))
      file
      
    }
    
  })
  
  output[["state_info"]] <- renderText({
    
    if(is.null(fit_state())){
      "No state detected"
    } else {
      paste0("Detected state: ", fit_state())
    }
    
  })
  
  fit_params <- reactive({
    
    data_file <- input[["fit_params"]]
    
    dat <- if(is.null(data_file)){
      read.csv("./data/fit_data_db_eEF1Ba-Alpha_KSCN.csv")
    } else {
      # validate(need(try({
        # file <- 
        read.csv(data_file[["datapath"]])
      # }), "File does not fullfill requirements. Check file requirements!"))
      # file
    }

    dat %>%
      dplyr::rename(id = X)
    
    
    
  })
  
  
  hires_dat <- reactive({

    
    calculate_hires(fit_params(), method = settings()[["agg_method"]])
    
  })
  
  fit_state <- reactive({ unique(fit_params()[["State"]]) })
  
  kin_dat <- reactive({
    
    validate(need(settings()[["time_0"]] %in% unique(exp_dat()[["Exposure"]]), "Select correct no deut time"))
    validate(need(settings()[["time_100"]] %in% unique(exp_dat()[["Exposure"]]), "Select correct full deut time"))
    
    HRaDeX::prepare_kin_dat(exp_dat(), 
                            state = fit_state(),
                            time_0 = settings()[["time_0"]],
                            time_100 = settings()[["time_100"]])
    
  })
  
  settings <- mod_settings_server("settings",
                                  dat = exp_dat)
  
  mod_peptides_server("peptides",
                      kin_dat = kin_dat,
                      fit_params = fit_params)
  
  mod_overview_server("overview",
                      kin_dat = kin_dat,
                      hires_dat = hires_dat, 
                      fit_params = fit_params, 
                      settings = settings)
  
}

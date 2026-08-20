#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  
  exp_dat_raw <- reactive({
    
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
  
  is_convention <- reactive({
    check_convention_usage(fit_params())
    
  })
  
  exp_dat <- reactive({

    if(is_convention()){
       HRaDeX::replace_sequences(exp_dat_raw()) 
    } else { exp_dat_raw() }

  })
  
  is_compatibile <- reactive({
    
    fit_state() %in% exp_dat_raw()[["State"]]
    
  })
  
  output[["state_info"]] <- renderText({
    
    if(is.null(fit_state())){
      "No state detected"
    } else {
      paste0("Detected state: ", fit_state())
    }
    
  })
  
  output[["convention_info"]] <- renderText({
    
    paste0("Convention detected in fit data and propagated to experimental data? ", is_convention())
    
  })
  
  fit_protein <- reactive({ unique(fit_params()[["Protein"]]) })
  exp_protein <- reactive({ unique(exp_dat()[["Protein"]]) })
  
  output[["file_info"]] <- renderText({
    
    if(is_compatibile()) { "Both files containt the same state."
      } else { "The files are not compatibile! "} 
    
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

    validate(need(is_compatibile(), "Load compatibile files and check the settings!"))
    
    calculate_hires(fit_params(), 
                    method = settings()[["agg_method"]],
                    fractional = settings()[["is_fractional"]])
    
  })
  
  fit_state <- reactive({ unique(fit_params()[["State"]]) })
  
  kin_dat <- reactive({
    
    validate(need(is_compatibile(), "Load compatibile files and check the settings!"))
    
    validate(need(settings()[["time_0"]] %in% unique(exp_dat()[["Exposure"]]), "Select correct no deut time"))
    validate(need(settings()[["time_100"]] %in% unique(exp_dat()[["Exposure"]]), "Select correct full deut time"))
    
    HRaDeX::prepare_kin_dat(exp_dat(), 
                            state = fit_state(),
                            time_0 = settings()[["time_0"]],
                            time_100 = settings()[["time_100"]])
    
  })
  
  settings <- mod_settings_server("settings",
                                  dat = exp_dat_raw) 
  
  mod_peptides_server("peptides",
                      kin_dat = kin_dat,
                      fit_params = fit_params, 
                      hires_dat = hires_dat,
                      settings = settings)
  
  mod_overview_server("overview",
                      fit_state = fit_state, 
                      kin_dat = kin_dat,
                      fit_params = fit_params, 
                      hires_dat = hires_dat, 
                      settings = settings)
  
}

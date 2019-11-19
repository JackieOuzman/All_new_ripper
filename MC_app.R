library(shiny)
library(snow)
library(MonteCarlo)
library(dplyr)
library(ggplot2)
library(purrr)
library(actuar)

#####################################################################################################################################
ui <- fluidPage(
  numericInput("area", label = h3("Area of production ha"), value = 400),
  numericInput("port_price", label = h3("port price $/t"), value = 250),
  numericInput("wheat_yld", label = h3("wheat yield t/ha"), value = 2.0),
  numericInput("N_applied", label = h3("N applied kg/ha"), value = 200),
  numericInput("cost_N", label = h3("cost of N $/t"), value = 560),
  numericInput("variable_costs", label = h3("variable costs $/ha"), value = 185),
  numericInput("location", label = h3("P50 medium yield"), value = 2),
  
  selectizeInput("year_for_ripping", 
                 label = h4("Ripping applied in which year?"),
                 choices = list('year 1' = "1",
                                'year 2' = "2",
                                'year 3' = "3",
                                'year 4' = "4",
                                'year 5' = "5"),
                 selected = 1,
                 multiple = TRUE),
  
  checkboxGroupInput(
    "mangement_option",
    label = h3("What are you considering?"),
    choices = list("shallow ripping with inputs" = "shallow_ripping_with_inputs"), 
    selected = "shallow_ripping_with_inputs"),
  
  #1a. collect base farm values:
  tableOutput("table1"),
  
  # 1b. collect parameter grids in list display it: 
  #tableOutput("value"),
  
  # 1d. display df for year of ripping: 
  tableOutput("year_for_ripping"),
  
  # 2. after using MonteCarlo simulations return a df and display it:
  tableOutput("MC_DF"),
  
   #1c. after using MonteCarlo simulations return a df and display it with additional cals:
  tableOutput("MC_results1_5"),
  
  # 3. calulate summary stats on the MC simulation and display it:
  tableOutput("MC_Summary_stats"),
  
  # 4. Plot the results
  plotOutput("plot")
)



######################################################################################################################################
server <- function(input, output, session) {
  
  ############################################################
  ################   reactive elements   #####################
  ############################################################
  
  ###############   Farm and crop values ###########################
  
  # 1a. function that collect base farm values:
  making_base_farm <- reactive({
    #browser()
    function_making_base_farm(input$port_price,
                              input$wheat_yld, 
                              input$area, 
                              input$N_applied,
                              input$cost_N,
                              input$variable_costs) 
    
  })
  
    # 1b. collect parameter grids in list and make it reactive:
  #the parameter setting will now be hard coded
  #n_obs_input <- rep(seq(2,5,by = 0.2), 10) #this is the rough max and min with number of samples
  n_obs_input <- 10
  shape = 5 #the shape controls the shape of the distrbution, smaller numbers will make the distribution skew to the left
  scale = 5 #controls the varaibility of the data, sharp curves with small tails or flats curves with long tails smaller values give you flatter curves
  #location is selected by the users and shifts the yield distrbution left or right
  param_list <- reactive({
    param_list = list("n" = n_obs_input, "shape" = shape, "scale" = scale, "location" = input$location)
  })
  
  # 1d year of ripping as reactive
  year_for_ripping <- reactive({
    function_year_ripping(input$mangement_option, input$year_for_ripping)
  })
  
  # 2. use MonteCarlo simulations and make the output reactive: Note I have hard coded the number of reps and I am returning a df
  ## Note I am having trouble accessing the function jax2 - needs to be run first??
  MC_results <- reactive({
    MC_result <- MonteCarlo(func=jaxs2, nrep=10, param_list=param_list(),  ncpus=1)
    MC_result_df<-MakeFrame(MC_result)
    MC_result_df 
    })  
   
  
  # 1c. function that uses output from MC base yld to cal adjusted yld and GM:
  MC_results1_5 <- reactive({
    #browser()
    function_MC_results1_5(MC_results(),year_for_ripping(),
                            #yr1_factor,
                             input$port_price,
                             input$wheat_yld,
                             input$area,
                             input$N_applied,
                             input$cost_N,
                             input$variable_costs)
    
    
  })
  
  # 3. calulate summary stats on the MC simulation - 
  # this is calling the function MC_summary stats and used results from 2 as input.
  MC_results_summary_stats <- reactive({
    MC_summary_stats(MC_results1_5())
    })  
  
  ############################################################
  ################       outputs         #####################
  ############################################################ 
  
  #1a. 
  output$table1 <- renderTable({
    making_base_farm()
  })
  
  # 1b. collect parameter grids in list display it: 
  output$value <- renderTable({
    param_list() 
  })
  
  # 1d. df for year of ripping for display: 
  output$year_for_ripping <- renderTable({
    year_for_ripping()
  })
  
  # 2. after using MonteCarlo simulations return a df and display it:
  output$MC_DF <- renderTable({
    MC_results() 
  })
  
  # 1c. Using MonteCarlo simulations df to modify yld and run GM:
  output$MC_results1_5 <- renderTable({
    MC_results1_5() 
  })
  
  # 3. calulate summary stats on the MC simulation and display it:
  output$MC_Summary_stats <- renderTable({
    MC_results_summary_stats() 
  })
  
  # 4. plot the results
  output$plot <- renderPlot({
    MC_results1_5() %>% 
      ggplot(aes(x = GM_yr1)) + 
      geom_density()
      
  })
  
######################################################################################################################################  
######                                            global file   - fuctions    ########################################################
######################################################################################################################################

  
  yr1_factor <- 1
  
  # 1a. function that collect base farm values:
  function_making_base_farm <-  function(port_price, wheat_yld, area, 
                                         N_applied, cost_N, variable_costs){
    
    base_farm <- data.frame(site_number = "xxxx", #user defined
                            year = 1:5,
                            crop = "wheat",
                            port_price =  port_price,
                            wheat_yld = wheat_yld,
                            area = area,
                            N_applied = N_applied,
                            cost_N = cost_N,
                            variable_costs = variable_costs)
    
    base_farm_GM <- mutate(base_farm,
                           wheat_revenue = (wheat_yld*  area)*port_price,
                           direct_expenses = ((N_applied * (cost_N / 1000)) * area) + (variable_costs * area) ,
                           GM = wheat_revenue - direct_expenses
    )
    return(base_farm_GM)
  }
  
  
  # 1b. function that collect parameter grids in list display it: 
  
  jaxs2<-function(n,shape,scale, location){
    
    #this sets up the input yield distribution
    #set.seed(16)
    base_yld1 <- (rllogis(n, shape , scale))
    base_yld <- base_yld1 + location #this step convert my distrubution into values I want mean yield
   
    #Now I can acess values from the yield distribution and run a calulation
    #It is important to access one value at a time to run the calulations on - here I have pulled out random value from the yield distrubution
    base_yld <-  (sample (base_yld, size=1))  
    
    return(list("base_yld" = base_yld))
  }
  
  # 1c. Take the MC simulation of base yield and run yield modification and GM, one clm for each year
  
  function_MC_results1_5 <-  function(MC_result_df, shallow_input_cost_year, 
                                      #yr1_factor,
                                      port_price, wheat_yld, area, 
                                      N_applied, cost_N, variable_costs)
                                      {
    
    MC_results_yr1_5_base_yld <- mutate(MC_result_df,
                             yld_yr1 = base_yld)
    
    #adjust the base yield based on FACTOR_yr1 here its 2 or 3
    MC_results_yr1_5_adjust_yld <-  mutate(MC_results_yr1_5_base_yld,
                                           yld_yr1_adjust =  yld_yr1 +2,
                                           yld_yr2_adjust =  yld_yr1 +3)  
    
    #access the cost to the farm values in the year ripping cost dataframe
    cost_yr1 <- shallow_input_cost_year[1,6]
    cost_yr2 <- shallow_input_cost_year[2,6]
    
    #Cal the GM on adjusted yr 1 and 2 yield 
    MC_results_yr1_5_GM <-  mutate(MC_results_yr1_5_adjust_yld,
                                     GM_yr1 = ((yld_yr1_adjust *area)*port_price) -
                                     ((N_applied * (cost_N /1000) *area) + (variable_costs *area)),
                                     GM_yr2 = ((yld_yr2_adjust *area)*port_price) -
                                     ((N_applied * (cost_N /1000) *area) + (variable_costs *area)),
                                     check_cost_to_farm = cost_yr1 #this will need to be added into the GM
                                   )
                                    
    MC_results_yr1_5_GM
  }
  
  # 1d.
  function_year_ripping <-function(mangement_option, 
                                   year_for_ripping){
    #shallow_input_cost, 
    #deep_input_cost, 
    #year_for_ripping_deep) 
    
    
    #create a list of treatments the user wants to explore
    list <- data.frame(managment_option = mangement_option)
    
    #create a dataframe of cost and years applied for treatment option 1 
    shallow_input_cost_year <- data.frame(year = 1:5,
                                          mangement_option = "shallow_ripping_with_inputs",
                                          mangement_input_cost = 20) %>% 
      mutate(ID = paste0(year,"_", mangement_option))
    
    #assign years applied to xxx_input_cost_year
    years <- 1:5
    years[!years %in% year_for_ripping] <- NA #look in years variable and see if we have match in what the user selected
    shallow_input_cost_year$years_applied <- years
    
    shallow_input_cost_year <- mutate(shallow_input_cost_year,
                                       cost_to_farm = case_when(
                                         years_applied > 0 ~ mangement_input_cost
                                       ))
    
    shallow_input_cost_year
  }
  
  # 3. function that will run some summary stats on MC_df results:
  MC_summary_stats <- function(MC_results){
    
    # A. Summary stats using dplyr - first step is adding in the percentiles- this requires additional work
    p <- c(0.1, 0.5, 0.9) #create the percentiles that you want to plot
    p_names <- map_chr(p, ~paste0(.x*100, "%")) #turns into names - used for headings
    
    p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
      set_names(nm = p_names) #purrr partial function turns uses quantiles for my list of data (p), everthing in brackets are argumnets for quatile function
    
    # B. Use step A in dplyr pipe workflow for Yr1
    MC_result_df_summary_stats_yr1 <- MC_results %>% 
      summarize_at(vars(GM_yr1), funs(!!!p_funs))
    # B. Use step A in dplyr pipe workflow for Yr2
    MC_result_df_summary_stats_yr2 <- MC_results %>% 
      summarize_at(vars(GM_yr2), funs(!!!p_funs))
    
    # C. cal other summary stats using dplyr workflow for Yr1
    MC_result_df_summary_stats2_yr1 <- MC_results %>%
      summarise(IQR = IQR(GM_yr1),
                mean = mean(GM_yr1),
                median = median(GM_yr1))
    # C. cal other summary stats using dplyr workflow for Yr2
    MC_result_df_summary_stats2_yr2 <- MC_results %>%
      summarise(IQR = IQR(GM_yr2),
                mean = mean(GM_yr2),
                median = median(GM_yr2))
    
    # D. Join together summary stats part B and C together and add name Yr1
    MC_result_df_summary_stats_1 <- cbind(MC_result_df_summary_stats_yr1, MC_result_df_summary_stats2_yr1)                                      
    MC_result_df_summary_stats_1 <- mutate(MC_result_df_summary_stats_1,
                                         name = "GM_yr1")
    # D. Join together summary stats part B and C together and add name Yr2
    MC_result_df_summary_stats_2 <- cbind(MC_result_df_summary_stats_yr2, MC_result_df_summary_stats2_yr2)                                      
    MC_result_df_summary_stats_2 <- mutate(MC_result_df_summary_stats_2,
                                           name = "GM_yr2")
    
    # E. Append the summary data togthers for years
    MC_result_df_summary_stats <- rbind(MC_result_df_summary_stats_1,
                                        MC_result_df_summary_stats_2)
  }
  
  
  
  
}
shinyApp(ui, server)


#run this in the console
# runApp("C:/Users/ouz001/Desktop/day_1/hello/hello_name/",display.mode = 'showcase')

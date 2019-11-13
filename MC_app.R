library(shiny)
library(snow)
library(MonteCarlo)
library(dplyr)
library(ggplot2)
library(purrr)


ui <- fluidPage(
  numericInput("n_obs_input", label = h3("Number of observation"), value = 1),
  numericInput("mean_input", label = h3("mean yield"), value = 1),
  numericInput("sd_input", label = h3("SD yield"), value = 1),

  tableOutput("value"),
  #tableOutput("MC_DF"),
  tableOutput("MC_Summary_stats"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  #### reactive elements
  # 1. collect parameter grids in list and make it reactive:
  param_list <- reactive({
    param_list = list(n = input$n_obs_input, "loc" = input$mean_input, "scale" = input$sd_input)
    })
  
  # 2. use MonteCarlo simulations and make the output reactive: Note I have hard coded the number of reps and I am returning a df
  MC_results <- reactive({
    MC_result <- MonteCarlo(func=jaxs2, nrep=100, param_list=param_list(), ncpus=1)
    MC_result_df<-MakeFrame(MC_result)
    })  
   
  # 3. calulate summary stats on the MC simulation
  MC_results_summary_stats <- reactive({
    #Note adding in the percentiles requires additional steps
    p <- c(0.1, 0.5, 0.9) #create the percentiles that you want to plot
    p_names <- map_chr(p, ~paste0(.x*100, "%")) #turns into names ?used for headings
    
    p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
      set_names(nm = p_names) #purrr partial function turns uses quantiles for my list of data (p), everthing in brackets are argumnets for quatile function
    
    #now I can use it in my dplyr pipe workflow
    MC_result_df_summary_stats1 <- MC_result_df %>% 
      summarize_at(vars(yeild_gain), funs(!!!p_funs))
    
    ### add other summary stats
    MC_result_df_summary_stats2 <- MC_result_df %>%
      summarise(IQR = IQR(yeild_gain),
                mean = mean(yeild_gain),
                median = median(yeild_gain))
    ##Join together
    MC_result_df_summary_stats1
    MC_result_df_summary_stats2
    MC_result_df_summary_stats <- cbind(MC_result_df_summary_stats1, MC_result_df_summary_stats2)                                      
    MC_result_df_summary_stats
    })  
  
 
  #### outputs ####
  output$value <- renderTable({
    param_list() 
  })
  
  output$MC_DF <- renderTable({
    MC_results() 
  })
  
  output$MC_Summary_stats <- renderTable({
    MC_results_summary_stats() 
  })
  
  # plot the results
  output$plot <- renderPlot({
    MC_results() %>% 
      ggplot(aes(x = yeild_gain)) + 
      geom_density()
      
  })
###### could put this bit in the global file??
  ####This is the function used to calulate the base yield histogram and how to run Economic analysis on it
  
  jaxs2<-function(n,loc,scale){
    
    #this sets up the input yield distribution
    sample<-rnorm(n, loc, scale) 
    
    #Now I can acess values from the yield distribution and run a calulation
    #It is important to access one value at a time to run the calulations on - here I have pulled out random value from the yield distrubution
    yeild_gain<- 100 * sample (sample1, size=1) #this works
    
    return(list("yeild_gain" = yeild_gain))
  }
  
  
}
shinyApp(ui, server)


#run this in the console
# runApp("C:/Users/ouz001/Desktop/day_1/hello/hello_name/",display.mode = 'showcase')

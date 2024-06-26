#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("An Example of Monte Carlo Simulation of sweet cherry production"),
  theme = bs_theme(version = 5, bootswatch = 'journal'), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 6, 
                 numericInput("num_simulations", 
                              "Number of Simulations", 
                              min = 1000, 
                              max = 1000000,
                              value = 1000),
                 sliderInput("Yield",
                             "Yield",
                             min = 1000,
                             max = 20000,
                             value = c(6000,14000)),
                 sliderInput("Market_price",
                             "Market price",
                             min = 1,
                             max = 10,
                             value = c(3, 8)),
                 sliderInput("Management_cost",
                             "Management_cost",
                             min = 100,
                             max = 2000,
                             value = c(500,1000)),
                 sliderInput("Labor_cost",
                             "Labor cost",
                             min = 100,
                             max = 10000,
                             value = c(100,5000)),
                 sliderInput("var_CV",
                             "coeff. Variation",
                             min = 1,
                             max = 100,
                             value =  c(1,10),
                             step=1),
                 sliderInput("n_years",
                             "Project time horizon (years)",
                             min = 1,
                             max = 30,
                             value = 10),
                 sliderInput("discount_rate",
                             "Discount rate (%)",
                             min = 1,
                             max = 100,
                             value =  c(1,10),
                             step=1),
                 
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(width = 6, 
              tabsetPanel(
                tabPanel("PLOT",
                         fluidRow(plotOutput("distPlot"),
                                  plotOutput("distPlot2")
                         )
                ))
              
              
    )))



# Define server logic required to draw a histogram
server <- function(input, output) {
  library(decisionSupport)
  library(tidyverse)
  
  output$distPlot <- renderPlot({
    
    input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost","Management_cost","var_CV","n_years", "discount_rate"),
                                  lower = c(min(input$Yield),min(input$Market_price),
                                            min(input$Labor_cost),min(input$Management_cost),min(input$var_CV),min(input$n_years),min(input$discount_rate)),
                                  median = NA,
                                  upper = c(max(input$Yield),max(input$Market_price),
                                            max(input$Labor_cost),max(input$Management_cost),max(input$var_CV),max(input$n_years),max(input$discount_rate)),
                                  distribution = c("posnorm", "posnorm", "posnorm","posnorm","posnorm","const","posnorm"),
                                  label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)","Management cost (USD/ha)","coeff. Variation",
                                            "Project time horizon (years)","Discount rate (%)"))
    
    input_estimates
    
    model_function <- function(){
      
      Yield_vv <- vv(Yield, var_CV, n_years)
      Market_price_vv <- vv(Market_price, var_CV, n_years)
      Labor_cost_vv <- vv(Labor_cost, var_CV, n_years)
      Management_cost_VV <- vv(Management_cost, var_CV, n_years)
      
      # Estimate the income in a normal season
      
      income <- Yield_vv * Market_price_vv
      
      # Estimate the overall_costs
      
      overall_costs <- Labor_cost_vv + Management_cost_VV
      
      # Estimate the final results from the model
      final_result <- income - overall_costs
      
      
      ##NPV calculate
      
      NPV_income <-
        discount(income, discount_rate, calculate_NPV = T)
      
      NPV_cost  <-
        discount(overall_costs, discount_rate, calculate_NPV = T)
      
      # Generate the list of outputs from the Monte Carlo simulation
      return(list(income_NPV= NPV_income,
                  Labor_cost_NPV = NPV_cost,
                  Total_NPV = NPV_income + NPV_cost,
                  Cashflow_MC = final_result))
    }
    
    # Run the Monte Carlo simulation using the model function
    example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                          model_function = model_function,
                                          numberOfModelRuns = input$num_simulations,
                                          functionSyntax = "plainNames")
    
    plot1 <-  plot_distributions(mcSimulation_object = example_mc_simulation,
                                 vars = "Total_NPV",
                                 method = "boxplot_density",
                                 colors = "maroon4")
    plot1
    
    ### Need code below if I want to combine many plots in a tabPanel ###
    
    # plot2 <-  plot_distributions(mcSimulation_object = example_mc_simulation, 
    #                     vars = c("apple_profits_NPV", "sheep_profits_NPV"),
    #                     method = 'smooth_simple_overlay', 
    #                     base_size = 16)
    
    # plot3 <- plot_cashflow(mcSimulation_object = example_mc_simulation,
    #               cashflow_var_name = "Cashflow_MC",
    #               x_axis_name = "Month",
    #               y_axis_name = "Cashflow in Dollar",
    #               base_size = 16)
    
    # 
    # library(patchwork)
    # Plots_combined <- list(plot1, plot2)
    # wrap_plots(Plots_combined, nrow = 2) +
    #   plot_layout(guides = "keep")
    
    
    
    
  })
  
  output$distPlot2 <- renderPlot({
    
    
    
    input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost","Management_cost","var_CV","n_years", "discount_rate"),
                                  lower = c(min(input$Yield),min(input$Market_price),
                                            min(input$Labor_cost),min(input$Management_cost),min(input$var_CV),min(input$n_years),min(input$discount_rate)),
                                  median = NA,
                                  upper = c(max(input$Yield),max(input$Market_price),
                                            max(input$Labor_cost),max(input$Management_cost),max(input$var_CV),max(input$n_years),max(input$discount_rate)),
                                  distribution = c("posnorm", "posnorm", "posnorm","posnorm","posnorm","const","posnorm"),
                                  label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)","Management cost (USD/ha)","coeff. Variation",
                                            "Project time horizon (years)","Discount rate (%)"))
    
    input_estimates
    
    model_function <- function(){
      
      Yield_vv <- vv(Yield, var_CV, n_years)
      Market_price_vv <- vv(Market_price, var_CV, n_years)
      Labor_cost_vv <- vv(Labor_cost, var_CV, n_years)
      Management_cost_VV <- vv(Management_cost, var_CV, n_years)
      
      # Estimate the income in a normal season
      
      income <- Yield_vv * Market_price_vv
      
      # Estimate the overall_costs
      
      overall_costs <- Labor_cost_vv + Management_cost_VV
      
      # Estimate the final results from the model
      final_result <- income - overall_costs
      
      
      ##NPV calculate
      
      NPV_income <-
        discount(income, discount_rate, calculate_NPV = T)
      
      NPV_cost_vv  <-
        discount(overall_costs, discount_rate, calculate_NPV = T)
      
      # Generate the list of outputs from the Monte Carlo simulation
      return(list(income_NPV= NPV_income,
                  Labor_cost_NPV = NPV_cost_vv,
                  Total_NPV = NPV_income + NPV_cost_vv,
                  Cashflow_MC = final_result))
    }
    
    # Run the Monte Carlo simulation using the model function
    example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                          model_function = model_function,
                                          numberOfModelRuns = input$num_simulations,
                                          functionSyntax = "plainNames")
    
    plot3 <- plot_cashflow(mcSimulation_object = example_mc_simulation,
                           cashflow_var_name = "Cashflow_MC",
                           x_axis_name = "Month",
                           y_axis_name = "Cashflow in Dollar",
                           color_25_75 = "darkorchid",
                           color_5_95 = "lavender",
                           color_median = "grey88",
                           base_size = 16)
    plot3
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

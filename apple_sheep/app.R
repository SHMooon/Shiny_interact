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
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("An Example of Monte Carlo Simulation with apples and sheep"),
  theme = bs_theme(version = 5, bootswatch = "minty"), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 6, 
                 numericInput("num_simulations", 
                              "Number of Simulations", 
                              min = 1000, 
                              max = 1000000,
                              step = 100,
                              value = 1000),
                 
                 sliderInput("apple_income_range",
                             "Apple income range",
                             min = 30000,
                             max = 90000,
                             value = c(30000, 60000),
                             step = 100),
                 sliderInput("apple_costs_range",
                             "Apple costs range",
                             min = 15000,
                             max = 50000,
                             value = c(15000,30000)),
                 sliderInput("sheep_income",
                             "Sheep income range",
                             min = 1000,
                             max = 10000,
                             value = c(2000,5000)),
                 sliderInput("sheep_costs",
                             "Sheep costs range",
                             min = 1000,
                             max = 5000,
                             value = c(1000,3000)),
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
                         fluidRow(plotOutput("distPlot",height = "800px",
                                             width = "150%"),
                                  plotOutput("distPlot2"),
                                  plotOutput("distPlot3"))
                ))
              
              
    )))



# Define server logic required to draw a histogram
server <- function(input, output) {
  library(decisionSupport)
  
  output$distPlot <- renderPlot({
    
    input_estimates <- data.frame(variable = c("apple_income", "apple_costs", "sheep_income", "sheep_costs","var_CV","n_years", "discount_rate"),
                                  lower = c(min(input$apple_income_range), min(input$apple_costs_range),min(input$sheep_income),
                                            min(input$sheep_costs),min(input$var_CV),min(input$n_years),min(input$discount_rate)),
                                  median = NA,
                                  upper = c(max(input$apple_income_range), max(input$apple_costs_range),max(input$sheep_income),
                                            max(input$sheep_costs),max(input$var_CV),min(input$n_years),max(input$discount_rate)),
                                  distribution = c("posnorm", "posnorm", "posnorm", "posnorm","posnorm","const","posnorm"),
                                  label = c("Price (USD/kg)", "Price (USD/kg)", "Price (USD/kg)","Price (USD/kg)", "coeff. Variation",
                                            "Project time horizon (years)","Discount rate (%)"))
    
    
    
    
    model_function <- function(){
      
      apple_income_vv <- vv(apple_income, var_CV, n_years)
      apple_costs_vv <- vv(apple_costs, var_CV, n_years)
      sheep_income_vv <- vv(sheep_income, var_CV, n_years)
      sheep_costs_vv <- vv(sheep_costs, var_CV, n_years)
      
      
      # Calculate profit for each simulation
      apple_profits <- apple_income_vv - apple_costs_vv
      
      sheep_profits <- sheep_income_vv - sheep_costs_vv
      
      # Estimate the final results from the model
      total_results <- apple_profits + sheep_profits
      
      ##NPV calculate
      
      NPV_apple_profits <-
        discount(apple_profits, discount_rate, calculate_NPV = T)
      
      NPV_sheep_profits  <-
        discount(sheep_profits, discount_rate, calculate_NPV = T)
      
      # Generate the list of outputs from the Monte Carlo simulation
      return(list(apple_profits_NPV= NPV_apple_profits,
                  sheep_profits_NPV = NPV_sheep_profits,
                  total_NPV = NPV_apple_profits + NPV_sheep_profits,
                  Cashflow_MC = total_results 
                  
      ))
    }
    
    # Run the Monte Carlo simulation using the model function
    example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                          model_function = model_function,
                                          numberOfModelRuns = input$num_simulations,
                                          functionSyntax = "plainNames")
    
    
    plot1 <-  plot_distributions(mcSimulation_object = example_mc_simulation,
                                 vars = "total_NPV",
                                 method = "boxplot_density",
                                 colors = "mediumseagreen",
                                 title ("apples and sheep"),
                                 ggtitle("Net Present Value of the system"))
    plot1
    
    ### Need code below if I want to combine many plots in a tabPanel ###
    
    # plot2 <-  plot_distributions(mcSimulation_object = example_mc_simulation,
    #                      vars = c("apple_profits_NPV", "sheep_profits_NPV"),
    #                      method = 'smooth_simple_overlay',
    #                      base_size = 16)
    # plot2
    # 
    #  plot3 <- plot_cashflow(mcSimulation_object = example_mc_simulation,
    #                 cashflow_var_name = "Cashflow_MC",
    #                 x_axis_name = "Month",
    #                 y_axis_name = "Cashflow in Dollar",
    #                 base_size = 16)
    # 
    # 
    # library(patchwork)
    # Plots_combined <- list(plot1, plot2)
    # wrap_plots(Plots_combined, nrow = 2) +
    #   plot_layout(guides = "keep")
    
    
    
    
  })
  
  output$distPlot2 <- renderPlot({
    
    input_estimates <- data.frame(variable = c("apple_income", "apple_costs", "sheep_income", "sheep_costs","var_CV","n_years", "discount_rate"),
                                  lower = c(min(input$apple_income_range), min(input$apple_costs_range),min(input$sheep_income),
                                            min(input$sheep_costs),min(input$var_CV),min(input$n_years),min(input$discount_rate)),
                                  median = NA,
                                  upper = c(max(input$apple_income_range), max(input$apple_costs_range),max(input$sheep_income),
                                            max(input$sheep_costs),max(input$var_CV),min(input$n_years),max(input$discount_rate)),
                                  distribution = c("posnorm", "posnorm", "posnorm", "posnorm","posnorm","const","posnorm"),
                                  label = c("Price (USD/kg)", "Price (USD/kg)", "Price (USD/kg)","Price (USD/kg)", "coeff. Variation",
                                            "Project time horizon (years)","Discount rate (%)"))
    
    
    
    
    model_function <- function(){
      
      apple_income_vv <- vv(apple_income, var_CV, n_years)
      apple_costs_vv <- vv(apple_costs, var_CV, n_years)
      sheep_income_vv <- vv(sheep_income, var_CV, n_years)
      sheep_costs_vv <- vv(sheep_costs, var_CV, n_years)
      
      
      # Calculate profit for each simulation
      apple_profits <- apple_income_vv - apple_costs_vv
      
      sheep_profits <- sheep_income_vv - sheep_costs_vv
      
      # Estimate the final results from the model
      total_results <- apple_profits + sheep_profits
      
      ##NPV calculate
      
      NPV_apple_profits <-
        discount(apple_profits, discount_rate, calculate_NPV = T)
      
      NPV_sheep_profits  <-
        discount(sheep_profits, discount_rate, calculate_NPV = T)
      
      # Generate the list of outputs from the Monte Carlo simulation
      return(list(apple_profits_NPV= NPV_apple_profits,
                  sheep_profits_NPV = NPV_sheep_profits,
                  total_NPV = NPV_apple_profits + NPV_sheep_profits,
                  Cashflow_MC = total_results 
                  
      ))
    }
    
    # Run the Monte Carlo simulation using the model function
    example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                          model_function = model_function,
                                          numberOfModelRuns = input$num_simulations,
                                          functionSyntax = "plainNames")
    
    
    plot_distributions(mcSimulation_object = example_mc_simulation, 
                       vars = c("apple_profits_NPV", "sheep_profits_NPV"),
                       method = 'smooth_simple_overlay', 
                       colors = c("tomato","wheat1"),
                       base_size = 16)
    
  })
  
  
  output$distPlot3 <- renderPlot({
    
    input_estimates <- data.frame(variable = c("apple_income", "apple_costs", "sheep_income", "sheep_costs","var_CV","n_years", "discount_rate"),
                                  lower = c(min(input$apple_income_range), min(input$apple_costs_range),min(input$sheep_income),
                                            min(input$sheep_costs),min(input$var_CV),min(input$n_years),min(input$discount_rate)),
                                  median = NA,
                                  upper = c(max(input$apple_income_range), max(input$apple_costs_range),max(input$sheep_income),
                                            max(input$sheep_costs),max(input$var_CV),min(input$n_years),max(input$discount_rate)),
                                  distribution = c("posnorm", "posnorm", "posnorm", "posnorm","posnorm","const","posnorm"),
                                  label = c("Price (USD/kg)", "Price (USD/kg)", "Price (USD/kg)","Price (USD/kg)", "coeff. Variation",
                                            "Project time horizon (years)","Discount rate (%)"))
    
    
    
    
    model_function <- function(){
      
      apple_income_vv <- vv(apple_income, var_CV, n_years)
      apple_costs_vv <- vv(apple_costs, var_CV, n_years)
      sheep_income_vv <- vv(sheep_income, var_CV, n_years)
      sheep_costs_vv <- vv(sheep_costs, var_CV, n_years)
      
      
      # Calculate profit for each simulation
      apple_profits <- apple_income_vv - apple_costs_vv
      
      sheep_profits <- sheep_income_vv - sheep_costs_vv
      
      # Estimate the final results from the model
      total_results <- apple_profits + sheep_profits
      
      ##NPV calculate
      
      NPV_apple_profits <-
        discount(apple_profits, discount_rate, calculate_NPV = T)
      
      NPV_sheep_profits  <-
        discount(sheep_profits, discount_rate, calculate_NPV = T)
      
      # Generate the list of outputs from the Monte Carlo simulation
      return(list(apple_profits_NPV= NPV_apple_profits,
                  sheep_profits_NPV = NPV_sheep_profits,
                  total_NPV = NPV_apple_profits + NPV_sheep_profits,
                  Cashflow_MC = total_results 
                  
      ))
    }
    
    # Run the Monte Carlo simulation using the model function
    example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                          model_function = model_function,
                                          numberOfModelRuns = input$num_simulations,
                                          functionSyntax = "plainNames")
    
    
    plot_cashflow(mcSimulation_object = example_mc_simulation,
                  cashflow_var_name = "Cashflow_MC",
                  x_axis_name = "Month",
                  y_axis_name = "Cashflow in Dollar",
                  color_25_75 = "lightblue3",
                  color_5_95 = "aliceblue",
                  color_median = "midnightblue",
                  base_size = 16)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(ggplot2)

source("/home/lele/Documents/DUKE/721/simulation/utils.R")

################################## Define UI ###################################

ui = pageWithSidebar(
  
  titlePanel(title = div(img(src = "duke_logo.jpg"), 
                         "    Mis-Classification Simulation")),
             

  sidebarPanel(
    numericInput("n_trial", "Number of simulations (integer)", value = 500),
    sliderInput('n_total', 'Total number of observations (patients):',
                min = 0, max = 4000, value = 400),
    sliderInput("n_exp", "Number of patients in the exposure group:",
                min = 0, max = 4000, value = 200),
    
    fluidRow(splitLayout(cellWidths = c('50%', '50%'),
                         sliderInput("r1", "True risk (exp):",
                                     min = 0, max = 1, value = 0.5, width='80%'),
                         sliderInput("r2", "True risk (non-exp):",
                                     min = 0, max = 1, value = 0.5, width='80%')
    )),
    # sliderInput("r1", "True risk in the exposure group:",
    #             min = 0, max = 1, value = 0.5, width='50%'),
    # sliderInput("r2", "True risk in the non-exposure group:",
    #             min = 0, max = 1, value = 0.5, width='50%'),
    fluidRow(splitLayout(cellWidths = c('50%', '50%'),
                         numericInput("a", "Number of false negative", value = 0),
                         numericInput("b", "Number of false positive", value = 0)
    )),
    # numericInput("a", "Number of false negative (exposure)", value = 1),
    # numericInput("b", "Number of false positive (exposure)", value = 1),
    selectInput(
      "type",
      "Select one statistics to visualize:",
      c('Risk Ratio', 'Risk Difference', 'Odds Ratio', 'Odds Difference')
    ),
    actionButton("go", "Plot")
  ),
  
  mainPanel(
    fluidRow(splitLayout(cellWidths = c('50%', '50%'),
                         tableOutput("true_table"),
                         tableOutput('bias_table')
    )),
    # plotOutput('plot')
    plotOutput("sim_plot"),
    textOutput("end")
  )
  
)

# ############################ Define server ###################################

server = function(input, output) {
 
  # two tables
  true_df = eventReactive(input$go, {
    this_study = constructer(input$n_total, input$n_exp, input$r1, input$r2)
    true_table(this_study)
  })
  
  bias_df = eventReactive(input$go, {
    this_study = constructer(input$n_total, input$n_exp, input$r1, input$r2)
    bias_table(this_study, c(input$a, input$b))
  })

  
  output$true_table = renderTable({
    true_df()
  }, caption = "Hypothetical truth table",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", '130%'))

  output$bias_table = renderTable({
    bias_df()
  }, caption = "One example of the biased tables",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", '130%'))
  
  
  #violin plot for n simulations
  what_to_plot = eventReactive(input$go, {
    
    #initialize
    this_study = constructer(input$n_total, input$n_exp, input$r1, input$r2)
    bias = c(input$a, input$b)
    out = c()
    
    # simulations
    for (i in 1: input$n_trial){
      vector = bias_vector(this_study, bias)
      out = rbind(out, vector)
    }
    # end
    
    data.frame(out)
  })
  
  get_true_vector = eventReactive(input$go, {
    this_study = constructer(input$n_total, input$n_exp, input$r1, input$r2)
    true_vector(this_study@true_risks)
  })
  
  output$sim_plot <- renderPlot({
    df = what_to_plot()
    vector = get_true_vector()
    if (input$type == 'Risk Ratio') {
      ggplot(df, aes(x = '', y = X1)) +
        # geom_hline(vector[1]) + 
        geom_violin() + 
        geom_jitter(width = 0.3) + 
        geom_boxplot(width = 0.3) + 
        geom_hline(yintercept = vector[1], color = 'red') + 
        ylab('Risk Ratio') + 
        geom_text(aes(0.5, vector[1],label = 'true value', vjust = -1)) +
        ggtitle('simulation visualizations')
        
    } else if (input$type == 'Risk Difference') {
      ggplot(df, aes(x = '', y = X2)) +
        # geom_hline(vector[1]) + 
        geom_violin() + 
        geom_jitter(width = 0.3) + 
        geom_boxplot(width = 0.3) + 
        geom_hline(yintercept = vector[2], color = 'red') + 
        ylab('Risk Difference') + 
        geom_text(aes(0.5, vector[1],label = 'true value', vjust = -1)) +
        ggtitle('simulation visualizations')
    } else if (input$type == 'Odds Ratio') {
      ggplot(df, aes(x = '', y = X3)) +
        # geom_hline(vector[1]) + 
        geom_violin() + 
        geom_jitter(width = 0.3) + 
        geom_boxplot(width = 0.3) + 
        geom_hline(yintercept = vector[3], color = 'red') + 
        ylab('Odds Ratio') + 
        geom_text(aes(0.5, vector[1],label = 'true value', vjust = -1)) +
        ggtitle('simulation visualizations')
    } else if (input$type == 'Odds Difference') {
      ggplot(df, aes(x = '', y = X4)) +
        # geom_hline(vector[1]) + 
        geom_violin() + 
        geom_jitter(width = 0.3) + 
        geom_boxplot(width = 0.3) + 
        geom_hline(yintercept = vector[4], color = 'red') + 
        ylab('Odds Difference') + 
        geom_text(aes(0.5, vector[1],label = 'true value', vjust = -1)) +
        ggtitle('simulation visualizations')
      }
    # end
  })
  
  output$end = renderText("by乐乐, 09/21/2022")
}

shinyApp(ui, server)

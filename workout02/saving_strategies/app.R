#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  verticalLayout( 
   # Application title
   titlePanel("Investment Strategies for Managing Savings"),
   
   fluidRow(
     column(4,
            sliderInput(inputId = 'amount', label = "Initial Amount",
                        min = 0, max = 100000, step = 500,
                        value = 1000, pre = "$", sep = ","
              
            )),
     
     
     column(4, 
            sliderInput(inputId = "rate", label = "Return Rate (in %)",
                        min = 0, max = 20, step = 0.1, value = 5)
            ),
   
   
   column(4,
          sliderInput(inputId = "years", label = "Years",
                      min = 0, max = 50, step = 1, value = 20
                      
          ))
   ),
   
   fluidRow(
     column(4,
            sliderInput(inputId = "contrib", label = "Annual Contribution",
                        min = 0, max = 50000, step = 500, value = 2000,
                        pre = "$", sep = ","
            )),
     
     column(4, 
            sliderInput(inputId = "growth", label = "Growth Rate (in %)",
                        min = 0, max = 20, step = 0.1, value = 2
              
            )),
     
     column(4,
            selectInput(inputId = "facet", label = "Facet?", choices = c("No", "Yes"),
                        selected = "No"
              
            ))
     
   ),
      
      # Show a plot of the generated distribution
      fluidRow(column(12, 
         plotOutput("timelinePlot")
      )),
   
      wellPanel(h4("Balances"),
        DT::dataTableOutput('balances')
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #' @title Future Value
  #' @description takes inputs of present value, rate of return and time and returns the future value of the investment
  #' @param amount initial investment (numeric)
  #' @param rate annual rate of return (numeric)
  #' @param years number of years invested (numeric)
  #' @return the value of your investment after the specified years
  
  future_value <- function(amount, rate, years) {
    return(amount * (1 + rate)^years)
  }
  
  #' @title Future Value of Annuity
  #' @description takes the value of your annual contribution, the rate of return and years and outputs your investment value after the specified time period
  #' @param contrib annual contribution amount (numeric)
  #' @param rate annual rate of return (numeric)
  #' @param years number of years invested (numeric)
  #' @return the future value of the annuity
  
  annuity <- function(contrib, rate, years) {
    return(contrib * (((1 + rate)^years) - 1) / rate)
  }
  
  #' @title Future Value of Growing Annuity
  #' @description takes the value of annual contribution growing at a fixed rate each year and outputs the value of the invest
  #' @param Contrib initial contribution (numeric)
  #' @param rate annual rate of return (numeric)
  #' @param growth growth rate of annual investment (numeric)
  #' @param years time in years (numeric)
  #' @return FVGA future value of growing annuity the value of the total investment after the time period
  
  growing_annuity <- function(contrib, rate, growth, years) {
    return(contrib * (((1 + rate)^years) - ((1 + growth)^ years))/(rate - growth))
  }
  
  
  output$balances <- DT::renderDataTable({
    
    modalities <- tibble(
      year = 0:input$years,
      no_contrib = rep(0, input$years + 1),
      fixed_contrib = rep(0, input$years + 1),
      growing_contrib = rep(0, input$years + 1))
    
    for (i in 0:input$years) {
      modalities$no_contrib[i + 1] <- future_value(amount = input$amount, rate = input$rate / 100, years = i)
      modalities$fixed_contrib[i + 1] <- modalities$no_contrib[i + 1] + annuity(contrib = input$contrib, rate = input$rate / 100, years = i)
      modalities$growing_contrib[i + 1] <- modalities$no_contrib[i + 1] + growing_annuity(contrib = input$contrib, rate = input$rate / 100, growth = input$growth / 100, years = i)
    }  
    
    
    modalities })
   
  
  
   output$timelinePlot <- renderPlot({
     
     modalities <- tibble(
       year = 0:input$years,
       no_contrib = rep(0, input$years + 1),
       fixed_contrib = rep(0, input$years + 1),
       growing_contrib = rep(0, input$years + 1))
     
     for (i in 0:input$years) {
       modalities$no_contrib[i + 1] <- future_value(amount = input$amount, rate = input$rate / 100, years = i)
       modalities$fixed_contrib[i + 1] <- modalities$no_contrib[i + 1] + annuity(contrib = input$contrib, rate = input$rate / 100, years = i)
       modalities$growing_contrib[i + 1] <- modalities$no_contrib[i + 1] + growing_annuity(contrib = input$contrib, rate = input$rate / 100, growth = input$growth / 100, years = i)
     }  
     narrow_modalities <- modalities %>%
       gather(key = "investment_strategy", value = "investment_value", -year)
     
     narrow_modalities$investment_strategy <- factor(narrow_modalities$investment_strategy, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))
     
     nmodality_plot <- ggplot(narrow_modalities, aes(x = year, y = investment_value, color = investment_strategy))
     
     dope_plot <- nmodality_plot + geom_line() + theme_bw() + scale_color_manual(name = "Investment Strategy", values = c("#FF4100", "#1430CC", "#6FFF00"),
                                                                                 labels = c("No Annual Contribution", "Fixed Annuity", "Growing Annuity")) + 
       labs(x = "Year", y = "Value of Investment ($)", title = "Comparing Investment Strategies over Time")
     
     
     if (input$facet == "No") {
     
      dope_plot
      
      }
      
     
     
     else {
       dope_plot + facet_wrap( ~ investment_strategy) + geom_area(aes(fill = investment_strategy), alpha = 0.3, show.legend = FALSE) + scale_fill_manual(values = c("#FF4100", "#1430CC", "#6FFF00"))
     }
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)


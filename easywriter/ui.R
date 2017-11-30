#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Easywriter"),
 
  sidebarLayout(
    sidebarPanel(
        includeMarkdown("Doc.Rmd"),
        textInput(inputId="typed", label="Type your phrase here", value = "", width = NULL, placeholder = NULL),
        uiOutput("buttons"),
        tags$p("The most likely prediction given your input (entire next word or word completion) is:"),
        span(textOutput("bestpred"), style="color:red")
    ),
    
    mainPanel(

    )
  )
))

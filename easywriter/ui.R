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
  titlePanel("Easy Writer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        textInput(inputId="typed", label="Type your message here", value = "", width = NULL, placeholder = NULL),
        actionButton(inputId="sugg1", label="", icon = NULL, width = NULL),
        actionButton(inputId="sugg2", label="", icon = NULL, width = NULL),
        actionButton(inputId="sugg3", label="", icon = NULL, width = NULL),
        #uiOutput("moreControls")
        h3(""),
        actionButton(inputId="go", label="send message", icon = NULL, width = NULL)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       textOutput("sentMessage")
    )
  )
))

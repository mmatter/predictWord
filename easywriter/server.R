#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    sugg <- eventReactive(input$typed, {
        # taking input text and considering only the n last words:
        splitted <- strsplit(input$typed, split=" ")[[1]]
        
        if (length(splitted) == 0) {
            lastwords <- c("", "")
        } else if (length(splitted) == 1) {
            lastwords <- c("", splitted)
        } else {
            lastwords <- tail(splitted, n=2)
        }

        # compute prediction...
        output <- names(prediction(beg=lastwords[2], v=lastwords[1], nouts=3))
        
        return(output)
    } )
    
    # Updating action buttons with latest prediction given the input:
    observeEvent(input$typed, {
        print(sugg())
        updateActionButton(session, inputId="sugg1", label=sugg()[1])
        updateActionButton(session, inputId="sugg2", label=sugg()[2])
        updateActionButton(session, inputId="sugg3", label=sugg()[3])
    })
    
    # Updating the text input whenever one of the action button is selected:
    observeEvent(input$sugg1, {
        updateTextInput(session, inputId="typed", value=paste(input$typed, sugg()[1]))
    })
    
    observeEvent(input$sugg2, {
        updateTextInput(session, inputId="typed", value=paste(input$typed, sugg()[2]))
    })
    
    # Display text once send message button is selected
    observeEvent(input$go, {
        output$sentMessage <- renderText(input$typed)
    })
 
  
})

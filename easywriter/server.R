#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

#source("../ngrams.R")
source("../prediction.R")

NGR_all <- list()
load("../learned_data/learned_news_10000.Rdata")
NGR_all[[1]] <- NGR_model
rm(NGR_model)
load("../learned_data/learned_blogs_300000.Rdata")
NGR_all[[2]] <- NGR_model
rm(NGR_model)
load("../learned_data/learned_twitter_100000.Rdata")
NGR_all[[3]] <- NGR_model
rm(NGR_model)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    sugg <- eventReactive(input$typed, {
       
        # compute prediction...
        if (input$typed != "") {
            output <- names(prediction(tolower(input$typed), NGR_all, nouts=3))
        }
        else {
            output <- rep("", times=3)
        }
        return(output)
    } )
    
    # Updating action buttons with latest prediction given the input:
    observeEvent(input$typed, {
        updateActionButton(session, inputId="sugg1", label=sugg()[1])
        updateActionButton(session, inputId="sugg2", label=sugg()[2])
        updateActionButton(session, inputId="sugg3", label=sugg()[3])
    })
    
    # Updating the text input according to the selected action button:
    observeEvent(input$sugg1, {
        splitted_bylast <- strsplit(input$typed, " (?=[^ ]+$)", perl=TRUE)[[1]]
        if (length(splitted_bylast) == 2) { # the next word is being typed
            newval <- paste(splitted_bylast[1], sugg()[1])
        } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) > 0) { # phrase ends by a space
            newval <- paste0(input$typed, sugg()[1])
        } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) == 0) { # the first word is being typed
            newval <- sugg()[1]
        }
        updateTextInput(session, inputId="typed", value=newval)
    })
    
    observeEvent(input$sugg2, {
        splitted_bylast <- strsplit(input$typed, " (?=[^ ]+$)", perl=TRUE)[[1]]
        if (length(splitted_bylast) == 2) { # the next word is being typed
            newval <- paste(splitted_bylast[1], sugg()[2])
        } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) > 0) { # phrase ends by a space
            newval <- paste0(input$typed, sugg()[2])
        } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) == 0) { # the first word is being typed
            newval <- sugg()[2]
        }
        updateTextInput(session, inputId="typed", value=newval)
    })
    
    observeEvent(input$sugg3, {
        splitted_bylast <- strsplit(input$typed, " (?=[^ ]+$)", perl=TRUE)[[1]]
        if (length(splitted_bylast) == 2) { # the next word is being typed
            newval <- paste(splitted_bylast[1], sugg()[2])
        } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) > 0) { # phrase ends by a space
            newval <- paste0(input$typed, sugg()[2])
        } else if (length(splitted_bylast) == 1 & length(grep(" ", splitted_bylast)) == 0) { # the first word is being typed
            newval <- sugg()[2]
        }
        updateTextInput(session, inputId="typed", value=newval)
    })
    
    # Display text once send message button is selected
    observeEvent(input$go, {
        output$sentMessage <- renderText(input$typed)
    })
 
  
})

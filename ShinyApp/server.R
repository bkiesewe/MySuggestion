# Birgit Kiesewetter, August 2016 for Capstone Project Shiny App server.R


library(shiny)
library(tm)

# getting functions
source("helper.R")

shinyServer(
    function(session, input, output) {
    suggestion_raw <- reactive(prediction(input$inputtext))
    suggestion <- reactive(unique(suggestion_raw()$Word)[1:3])
    
    # for behind the scenes conditional panel
    output$text <- renderTable(suggestion_raw(),
                               include.rownames=FALSE) 
    # for choosing suggested word through button 
    observeEvent(input$Button1,                  {
        newtext <- paste(input$inputtext, suggestion()[1])
        updateTextInput(session, "inputtext", value=newtext)
    })   
    
    observeEvent(input$Button2,                  {
        newtext <- paste(input$inputtext, suggestion()[2])
        updateTextInput(session, "inputtext", value=newtext)
    }) 
    
    observeEvent(input$Button3,                  {
        newtext <- paste(input$inputtext, suggestion()[3])
        updateTextInput(session, "inputtext", value=newtext)
    }) 
    
    
    # label the buttons with the suggested words    
    output$Button1 <- renderUI({
        actionButton("Button1", label = suggestion()[1], 
                     style="background-color:#ffb84d; border-color: #2e6da4; 
                     border-radius: 12px; font-size: 20px")})
    output$Button2 <- renderUI({
        actionButton("Button2", label = suggestion()[2],
                     style="background-color:#ffc266; border-color: #2e6da4;
                     border-radius: 12px; font-size: 20px")})
    output$Button3 <- renderUI({
        actionButton("Button3", label = suggestion()[3],
                     style="background-color:#ffcc80; border-color: #2e6da4;
                     border-radius: 12px; font-size: 20px")})

})


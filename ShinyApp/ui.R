# Birgit Kiesewetter, August 2016 for Capstone Project Shiny App ui.R

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("My Next Word Suggestion"),
hr(),
img(src = "my_image.png", height = 80, width = 80),
br(),
hr(),


    mainPanel(wellPanel(
        h4("How does it work?"),
        h4("Just start typing some text and you will get 3 words 
           suggested you can choose from to proceed with ..."),
        hr(),
        # textbox
        textInput("inputtext", h3("Type your text here: "), width='100%'),
        # buttons
        fluidRow(
             column(3, uiOutput('Button1')),
             column(3, uiOutput('Button2')),
             column(3, uiOutput('Button3'))),
        br(),
        h5("Just click on the word you would like to input next or continue typing in the box.")
        , style="background-color:#e1e1d1"),
        br(),
        hr(),
        
        # option to show raw data frame
        checkboxInput("checkbox", strong("Tick the box, if you want to see the raw suggestion results behind the scenes."), FALSE, width='100%'),
        conditionalPanel(
            condition = "input.checkbox == true",
            wellPanel(p("As soon as you type some text a table appears below showing where the 
                        suggested words come from."),
                      helpText(div("The algorithm first goes through the Trigram Dictionary and gets the words 
                        following the last 2 words you entered. 
                                   If the result is less than 3 words, it backs off to the Bigram Dictionary and gets the words 
                                   following the last word you entered. If the resulting list still does not contain 3 unique hits, it fills up 
                                   with the most common words: the, to, end.
                                   Based on the result printed below the final suggested words are the top 3 unique words. 
                                   Basically this is a simplified", a("Katz's back-off model", href = "https://en.wikipedia.org/wiki/Katz%27s_back-off_model"),".",style = "font-size:80%" ))),
                tableOutput("text")
        )# end conditionalPanel 
        # end wellPanel
        
        
        )# end mainPanel
  )
)



# server.R

## Johns Hopkins Data Science Certification Capstone Project via Coursera
## Kevin McCue

## Shiny server script
## 1/16/2019

## Packages
source('prediction.R')

if (!require("shiny")) {install.packages("shiny")}; require(shiny)

# Define application ####

shinyServer(function(input, output) {
  
  # Reactive statement for prediction function when user input changes ####
  prediction =  reactive( {
    
    # Get input
    inputText = input$text
    input1 =  fun.input(inputText)[1, ]
    input2 =  fun.input(inputText)[2, ]
    input3 =  fun.input(inputText)[3, ]
    nSuggestion = input$slider
    
    # Predict
    prediction = fun.predict(input1, input2, input3, n = nSuggestion)
  })
  
  # Output data table ####
  output$table = renderDataTable(prediction(),
                                 option = list(pageLength = 5,
                                               lengthMenu = list(c(5, 10, 100), c('5', '10', '100')),
                                               columnDefs = list(list(visible = F, targets = 1))
                                               #searching = F
                                 )
  )
  
  # Output word cloud ####
  wordcloud_rep = repeatable(wordcloud)
  output$wordcloud = renderPlot(
    wordcloud_rep(
      prediction()$NextWord,
      prediction()$freq,
      colors = brewer.pal(8, 'Dark2'),
      scale=c(4, 0.5),
      max.words = 300
    )
  )
})
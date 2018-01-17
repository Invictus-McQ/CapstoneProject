# ui.R 

## Johns Hopkins Data Science Certification Capstone Project via Coursera
## Kevin McCue
## 1/16/2017

## Shiny UI


## Packages to load
if (!require("shiny")) {install.packages("shiny")}; require(shiny)
if (!require("shinythemes")) {install.packages("shinythemes")}; require(shinythemes)


# Define the app
shinyUI(fluidPage(
  
  # Theme
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Word Predictor"),
  
  # Sidebar ####    
  sidebarLayout(
    
    sidebarPanel(
      
      # Text input
      textInput("text", label = ('Please type some text'), value = ''),
      
      # Number of words slider input
      sliderInput('slider',
                  'Maximum number of words',
                  min = 0,  max = 1000,  value = 10
      ),
      
      # Table output
      dataTableOutput('table')),
    
    # Mainpanel ####
    
    mainPanel(
      
      wellPanel(
        
        ## Link to milestone
        helpText(a('More information on the project',
                   href='http://rpubs.com/Invictus_AD1978/349375', 
                   target = '_blank')
        ),
        
        # Link to github
        helpText(a('Github Code Repo',
                   href='https://github.com/Invictus-McQ/CapstoneProject',
                   target = '_blank')
        ),
        
        # Wordcloud output
        plotOutput('wordcloud')
      )
    ) 
  )
)
)
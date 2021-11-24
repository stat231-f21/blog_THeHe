library(tidyverse)
library(tidyverse)

######
# ui #
######

ui <- navbarPage(
  
  theme = shinytheme("lumen"),
  
  title = "Text Analysis of COVID-19 Related Local News",
  
  tabPanel(
    
    title = "Common Words",
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "week_slider",
                    label = "Choose a week to look at:",
                    min = 22,
                    max = 39,
                    value = 22,
                    step = 1,
                    animate = TRUE,
                    pre = "Week "),
        
        sliderInput(inputId = "words_slider",
                    label = "Choose how many words you would like to look at:",
                    min = 1,
                    max = 5,
                    value = 1,
                    step = 1)
      )
    ),
    
    mainPanel(
      
      plotOutput(outputId = "wordmap")
    )
  )
)

##########
# server #
##########



############
# shinyApp #
############


library(tidyverse)
library(tidytext)
library(sf)

###########
# mapping #
###########

state_map <- maps::map("state", plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

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
  ),
  
  tabPanel(
    
    title = "Sentiment Analysis",
    
    mainPanel(
      
      plotOutput(outputId = "sentmap")
    )
  )
)

##########
# server #
##########

server <- function(input, output) {
  
  output$wordmap <- renderPlot({
    
    ggplot(data = state_map) +
      geom_sf(fill = "white", color = "black") +
      geom_text(aes(label = ))
      theme_void()
    
    
  })
  
  
}

############
# shinyApp #
############


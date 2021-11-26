library(tidyverse)
library(tidytext)
library(sf)
library(shinythemes)

words_wrangled <- read_csv("data/news/words_wrangled.csv")

###########
# mapping #
###########

sf::sf_use_s2(FALSE)

state_map <- maps::map("state", plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

state_points <- data.frame(st_coordinates(st_centroid(state_map$geom)))

state_map <- state_map %>% 
  bind_cols(state_points)

##############
# sentiments #
##############

afinn_lexicon <- get_sentiments("afinn")

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
        sliderInput(inputId = "week_slider1",
                    label = "Choose a week to look at:",
                    min = 22,
                    max = 39,
                    value = 22,
                    step = 1,
                    animate = TRUE,
                    pre = "Week "),
        
        sliderInput(inputId = "words_slider1",
                    label = "Choose how many words you would like to look at:",
                    min = 1,
                    max = 5,
                    value = 1,
                    step = 1)
      ),
      
      mainPanel(
        
        plotOutput(outputId = "wordmap")
      )
     ),
    ),
  
  tabPanel(
    
    title = "Sentiment Analysis",
    
    tabPanel(
      
      title = "Common Words",
      
      sidebarLayout(
        sidebarPanel(
          sliderInput(inputId = "week_slider2",
                      label = "Choose a week to look at:",
                      min = 22,
                      max = 39,
                      value = 22,
                      step = 1,
                      animate = TRUE,
                      pre = "Week "),
        ),
    
    mainPanel(
      
      plotOutput(outputId = "sentmap")
    )
  )
)))

##########
# server #
##########

server <- function(input, output) {
  
  wordmap_words <- reactive({
    
    words_wrangled %>% 
    filter(week == input$week_slider1) %>% 
    arrange(state, desc(n)) %>% 
    nest(word_list = c(word, n)) %>% 
    mutate(state = tolower(state), 
           reactive_words = map(word_list, ~
                                    .x %>% 
                                    select(word) %>% 
                                    slice_head(n = input$words_slider1) %>% 
                                    pull(word))) %>% 
    right_join(state_map, by = c("state" = "ID")) %>% 
      st_as_sf()

  })
    
  output$wordmap <- renderPlot({
    word_data <- wordmap_words()
    ggplot(data = word_data) +
      geom_sf(fill = "white", color = "black") +
      geom_label_repel(aes(x = X, y = Y, label = word_data$reactive_words),
                       point.padding = .5) +
      theme_void()

  })
  
  sent_words <- reactive({
    
    words_wrangled %>% 
    filter(week == input$week_slider2) %>% 
    arrange(state, desc(n)) %>% 
    inner_join(afinn_lexicon, by = "word") %>% 
    nest(word_list = c(word, n, value)) %>% 
    mutate(state = tolower(state)) %>% 
    right_join(state_map, by = c("state" = "ID")) %>% 
    mutate(avg_sent = map_dbl(word_list, ~ mean(.x$value * .x$n, na.rm = TRUE))) %>% 
      st_as_sf()
    
  })
      
  output$sentmap <- renderPlot({
    sent_data <- sent_words()
    ggplot(sent_data) +
      geom_sf(aes(fill = sent_data$avg_sent)) +
      scale_fill_distiller(palette = "RdBu") +
      theme_void() +
      labs(fill = "Average Sentiment of Headlines")
    
  })
  
  
}

############
# shinyApp #
############

shinyApp(ui = ui, server = server)

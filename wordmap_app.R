library(tidyverse)
library(tidytext)
library(sf)
library(shinythemes)

#############
# wrangling #
#############

all_headlines <- read_csv("data/news/headlines.csv")
data("stop_words")
custom_stops <- read_csv("data/news/custom_stops.csv")

word_frequencies <- all_headlines %>% 
  unnest_tokens(output = word, input = headline_text, drop = TRUE) %>% 
  anti_join(stop_words, by = "word") %>% 
  add_count(state, week, word) %>% 
  distinct() %>% 
  arrange(state, week, desc(n))

word_frequencies_trimmed <- word_frequencies %>% 
  filter(!str_detect(word, "[:digit:]"),
         !str_detect(word, "\\."),
         !str_detect(word, "covid"),
         !str_detect(word, tolower(as.character(word_frequencies$state)))) %>% 
  anti_join(custom_stops, by = "word")

###########
# mapping #
###########

state_map <- maps::map("state", plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

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
    
    word_frequencies_trimmed %>% 
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
      geom_sf_label(label = word_data$reactive_words) +
      theme_void()

  })
  
  sent_words <- reactive({
    
    word_frequencies_trimmed %>% 
    filter(week == input$week_slider2) %>% 
    arrange(state, desc(n)) %>% 
    inner_join(afinn_lexicon, by = "word") %>% 
    nest(word_list = c(word, n, value)) %>% 
    mutate(state = tolower(state)) %>% 
    right_join(state_map, by = c("state" = "ID")) %>% 
    mutate(avg_sent = map_dbl(word_list, ~ mean(.x$value, na.rm = TRUE))) %>% 
      st_as_sf()
    
  })
      
  output$sentmap <- renderPlot({
    sent_data <- sent_words()
    ggplot(sent_data) +
      geom_sf(aes(fill = sent_data$avg_sent, color = "black")) +
      scale_fill_distiller(palette = "RdBu", direction = -1) +
      theme_void()
    
  })
  
  
}

############
# shinyApp #
############

shinyApp(ui = ui, server = server)

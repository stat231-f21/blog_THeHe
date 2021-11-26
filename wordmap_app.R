library(tidyverse)
library(tidytext)
library(sf)
library(shinythemes)

#############
# wrangling #
#############

all_headlines <- read_csv("data/news/headlines.csv")
data("stop_words")

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
         !str_detect(word, tolower(as.character(word_frequencies$state))))

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
      ),
      
      mainPanel(
        
        plotOutput(outputId = "wordmap")
      )
     ),
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
  
  wordmap_words <- reactive({
    
    word_frequencies_trimmed %>% 
    filter(week == input$week_slider) %>% 
    arrange(state, desc(n)) %>% 
    nest(word_list = c(word, n))
    
  })
  
  wordmap_words <- reactive({
    
    head(wordmap_words[[3]], input$words_slider)
    
  })
  
  full_map <- reactive({
    
    state_map %>% 
      left_join(wordmap_words, by = c(id, tolower(state)))
  })
  
  output$wordmap <- renderPlot({
    
    ggplot(data = state_map) +
      geom_sf(fill = "white", color = "black") +
      theme_void()
    
    
  })
  
  sent_data <- reactive({
    
    word_frequencies_trimmed %>% 
      filter(week == input$week_slider) %>% 
      arrange(state, desc(n)) %>% 
      inner_join(afinn_lexicon, by = "word") %>% 
      nest(word_list = c(word, n, value)) %>% 
      mutate(state = tolower(state))
    
    sent_data <- sent_data %>% 
      inner_join(state_map, by = c("state" = "ID"))
    
    sent_data <- sent_data %>% 
      mutate(avg_sent = map_dbl(word_list, ~ .x %>% 
                                  select(value) %>% 
                                  mean()))
    
  })
  
  output$sentmap <- renderPlot({
    
   ggplot(data = sent_data) +
      geom_sf(fill = avg_sent, color = "black") +
      theme_void()
    
  })
  
  
}

############
# shinyApp #
############

shinyApp(ui = ui, server = server)

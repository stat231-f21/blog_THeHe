library(tidyverse)
library(tidytext)
library(sf)
library(shinythemes)

# Read in the text-wrangled words
words_wrangled <- read_csv("data/news/words_wrangled.csv")

###########
# mapping #
###########

# Do not use spherical geometries for mapping
# (Needed to allow finding centroids; does not affect states mapping)
sf::sf_use_s2(FALSE)

# Import sf object for US states
state_map <- maps::map("state", plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

# Find centerpoint (centroid) of each state
state_points <- data.frame(st_coordinates(st_centroid(state_map$geom)))

# Add centroids to sf data
state_map <- state_map %>% 
  bind_cols(state_points)

##############
# sentiments #
##############

# Obtain AFINN sentiments lexicon (this rates words from -5 to +5)
afinn_lexicon <- get_sentiments("afinn")

######
# ui #
######

ui <- navbarPage(
  
  # Specify theme for Shiny app
  theme = shinytheme("lumen"),
  # Add title for entire Shiny app
  title = "Text Analysis of COVID-19 Related Local News",
  # TAB 1: WORD FREQUENCY MAP
  tabPanel(
    # Add title for tab page
    title = "Common Words",
    
    sidebarLayout(
      sidebarPanel(
        # Add slider to specify week of interest
        sliderInput(inputId = "week_slider1",
                    label = "Choose a week to look at:",
                    min = 22,
                    max = 39,
                    value = 22,
                    step = 1,
                    animate = TRUE,
                    pre = "Week "),
        # Add slider to specify how many words are desired
        sliderInput(inputId = "words_slider1",
                    label = "Choose how many words you would like to look at:",
                    min = 1,
                    max = 5,
                    value = 1,
                    step = 1)
      ),
      
      mainPanel(
        # Make space for word frequency map
        plotOutput(outputId = "wordmap")
      )
     ),
    ),
  # TAB 2: SENTIMENT ANALYSIS MAP
  tabPanel(
    # Add title for second tab page
    title = "Sentiment Analysis",
      
      sidebarLayout(
        sidebarPanel(
          # Add slider to specify weeks of interest
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
        # Make space for sentiment map
        plotOutput(outputId = "sentmap")
      )
    )
  )
)

##########
# server #
##########

server <- function(input, output) {
  
  # Perform reactive wrangling for word frequency map
  wordmap_words <- reactive({
    
    words_wrangled %>%
    # Filter only weeks specified by slider
    filter(week == input$week_slider1) %>% 
    # Arrange words first by state, and then by how often they occur  
    arrange(state, desc(n)) %>% 
    # Create a list-column of words & frequency for each state  
    nest(word_list = c(word, n)) %>%
           # Make state names lowercase  
    mutate(state = tolower(state),
           # Pull out the top however-so-many words from each state
           reactive_words = map(word_list, ~
                                    .x %>% 
                                    select(word) %>% 
                                    slice_head(n = input$words_slider1) %>% 
                                    pull(word))) %>%
    # Add mapping sf data to text data  
    right_join(state_map, by = c("state" = "ID")) %>% 
    # Coerce back into sf object
    st_as_sf()

  })
  
  # Create word frequency map PLOT  
  output$wordmap <- renderPlot({
    # Create data frame from reactive function
    word_data <- wordmap_words()
    # Create ggplot object for word frequency map
    ggplot(data = word_data) +
      # Create polygon geoms
      geom_sf(fill = "white", color = "black") +
      # Create labels at center of each state with selected top words
      geom_label_repel(aes(x = X, y = Y, label = word_data$reactive_words),
                       point.padding = .5) +
      # Erase axes, ticks, etc.
      theme_void()

  })
  
  # Perform reactive wrangling for sentiment analysis map
  sent_words <- reactive({
    
    words_wrangled %>% 
    # Filter only weeks specified  
    filter(week == input$week_slider2) %>% 
    # Arrange words first by state, and then by how often they occur
    arrange(state, desc(n)) %>%
    # Add sentiment data
    inner_join(afinn_lexicon, by = "word") %>%
    # Create list-column of word, frequency, and sentiment value
    nest(word_list = c(word, n, value)) %>%
    # Make state names lowercase  
    mutate(state = tolower(state)) %>% 
    # Add state sf geometry objects  
    right_join(state_map, by = c("state" = "ID")) %>% 
    # Compute & mutate the weighted mean of sentiment values for each state
    mutate(avg_sent = map_dbl(word_list, ~ mean(.x$value * .x$n, na.rm = TRUE))) %>%
    # Coerce back into sf object
    st_as_sf()
    
  })
  
  # Create sentiment map PLOT    
  output$sentmap <- renderPlot({
    # Create data frame from reactive function
    sent_data <- sent_words()
    # Create ggplot object for sentiment map
    ggplot(sent_data) +
      # Create map polyhons; fill with average sentiment values
      geom_sf(aes(fill = sent_data$avg_sent)) +
      # Add color scales
      scale_fill_distiller(palette = "RdBu") +
      # Remove axes, ticks, etc.
      theme_void() +
      # Add labels
      labs(fill = "Average Sentiment of Headlines")
    
  })
}

############
# shinyApp #
############

shinyApp(ui = ui, server = server)

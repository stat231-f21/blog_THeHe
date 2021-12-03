library(tidyverse)
library(tidytext)

#############
# wrangling #
#############

# Read in scraped headlines
all_headlines <- read_csv("data/news/headlines.csv")
# Read in typical stop words
data("stop_words")
# Read in custom list of stop words
custom_stops <- read_csv("data/news/custom_stops.csv")

word_frequencies <- all_headlines %>% 
  # Break headlines up into unigrams
  unnest_tokens(output = word, input = headline_text, drop = TRUE) %>% 
  # Remove typical stop words
  anti_join(stop_words, by = "word") %>% 
  # Add word frequencies
  add_count(state, week, word) %>% 
  # Remove duplicate entries
  distinct() %>% 
  # Group words by state & week, then sort by frequency
  arrange(state, week, desc(n))

words_wrangled <- word_frequencies %>% 
         # Filter out any numbers
  filter(!str_detect(word, "[:digit:]"),
         # Filter out any phrase with "." (mostly urls)
         !str_detect(word, "\\."),
         # Filter out "covid"
         !str_detect(word, "covid"),
         # Filter out the state names
         !str_detect(word, tolower(as.character(word_frequencies$state)))) %>%
  # Remove custom stop words
  anti_join(custom_stops, by = "word")

# Write csv of wrangled words
write_csv(words_wrangled, "data/news/words_wrangled.csv")
write_csv(words_wrangled, "shiny_word-map/words_wrangled.csv")

library(tidyverse)
library(ggwordcloud)
library(tidytext)


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
         !str_detect(word, "\\."))

wordcloud_words <- word_frequencies_trimmed %>% 
  filter(state == "Alabama",
         week == 22) %>% 
  select(word, n)

m <- ggplot(data = wordcloud_words,
       aes(label = word, size = n)) +
  geom_text_wordcloud_area(mask = png::readPNG("img/states/Alabama.png"), rm_outside = TRUE) +
  scale_size_area(max_size = 40) +
  theme_minimal()

print(m)
show(m)

# wordcloud2(wordcloud_words, figPath = "img/states/Alabama.jpg", size = 1.5, color = "skyblue", backgroundColor = "white")


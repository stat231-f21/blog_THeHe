library(tidyverse)
library(rvest)
library(robotstxt)
library(tidyRSS)

# Read in urls of local newspapers from all 50 states
news_sites <- read_csv("data/news/us_newspapers.csv")
# Read in date values for the weeks from PULSE data
weeks <- read.csv("data/news/weeks.csv")
# Create an empty list that will be populated with scraped headlines
headlines <- list(state = rep(state.name, each = 18),
                        week = rep(22:39, 50),
                        headline_text = rep(NA, 50*18))
# Loop for 50 states
for (i in 1:50) {
  
  # Set current state
  state <- news_sites$state[i]
  #Set current news url
  site <- news_sites$url[i]
  
  # Loop for 18 weeks
  for(j in 1:18){
    
    # Set current start date (of time period in question)
    start_date <- weeks$start[j]
    # Set current end date (of time period in question)
    end_date <- weeks$end[j]
    
    # Create Google News RSS url to scrape
    rss_url <- paste0("https://news.google.com/rss/search?q=before%3A", end_date, "%20after%3A", start_date, "%20covid%20", state, "%20site%3A", site, "&hl=en-US&gl=US&ceid=US%3Aen")
    
    # I don't believe that paths_allowed() gives the correct answer here; see blog for details.
    # if (paths_allowed(rss_url) == FALSE) {
    # 
    #   break
    # 
    # }
    
    # Scrape data from RSS feed
    headlines_scraped <- tryCatch(
      
      # Return NA instead of headline text when error is thrown (usually because there are no articles that meet criteria)
      error = function(cnd) {
        return(NA)
      },
      
      # Otherwise, pull headline text from the RSS feed
      scraped_text <- rss_url %>% 
        tidyfeed() %>% 
        select(item_title)
    )
    
    # Add scraped headline into out pre-made list
    headlines$headline_text[(i-1)*18 + j] <- headlines_scraped
    
  }
}

# Make filled list into data frame
headlines_data <- as.data.frame(do.call(cbind, headlines))

headlines_long <- headlines_data %>% 
  # Unnest the headlines to give each headline its own row
  unnest(headline_text) %>% 
  # Tidy up other data in table
  mutate(state = as.character(state),
         week = as.numeric(week),
         headline_text = as.character(headline_text))

# Create csv with all scraped headlines
write_csv(headlines_long, "data/news/headlines.csv")


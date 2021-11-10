library(tidyverse)
library(rvest)
library(robotstxt)
library(tidyRSS)

news_sites <- read_csv("data/news/us_newspapers.csv")
weeks <- read.csv("data/news/weeks.csv")
headlines <- list(state = rep(state.name, each = 18),
                        week = rep(22:39, 50),
                        headline_text = rep(NA, 50*18))

for (i in 1:50) {
  
  state <- news_sites$state[i]
  site <- news_sites$url[i]
  
  for(j in 1:18){
    
    start_date <- weeks$start[j]
    end_date <- weeks$end[j]
    
    rss_url <- paste0("https://news.google.com/rss/search?q=before%3A", end_date, "%20after%3A", start_date, "%20covid%20", state, "%20site%3A", site, "&hl=en-US&gl=US&ceid=US%3Aen")
    
    # if (paths_allowed(rss_url) == FALSE) {
    # 
    #   break
    # 
    # }
    
    headlines_scraped <- tryCatch(
      
      # Return "Missing" instead of poem text when error is thrown
      error = function(cnd) {
        return(NA)
      },
      
      # Scrape text otherwise
      scraped_text <- rss_url %>% 
        tidyfeed() %>% 
        select(item_title)
    )
    
    headlines$headline_text[(i-1)*18 + j] <- headlines_scraped
    
  }
  
}

library(tidyverse)
library(rvest)
library(robotstxt)
library(tidyRSS)

news_sites <- read_csv("data/news/us_newspapers.csv")
weeks <- read.csv("data/news/weeks.csv")
headlines <- list(state = rep(state.name, each = 18),
                        week = rep(22:39, 50),
                        headline_text = rep(NA, 50*18))

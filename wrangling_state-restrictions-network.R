# Data Wrangling for COVID State Restrictions Networks

#######################
#    Load Datasets    #
#######################
# Load packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(igraph)
library(visNetwork)
library(grDevices)

# Load COVID state restrictions datasets
restaurants <- read.csv("data/restrictions/Restaurants.csv")
bars <- read.csv("data/restrictions/Bars.csv")
mask_mandates <- read.csv("data/restrictions/Mask_Mandates.csv")
gathering_bans <- read.csv("data/restrictions/Gathering_Bans.csv")
stay_at_home_orders <- read.csv("data/restrictions/Stay-At-Home_Orders.csv")

# Set color palette
eigScalePal <- colorRampPalette(c("blue", "red"), bias = 5)
num_colors <- 5

# Create plot for color legend
png(file = "www/covid_legend.png")
legend_image <- as.raster(matrix(eigScalePal(5), ncol=1))
plot(c(0,4),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
legend_image <- rasterImage(legend_image, 1, 1, 0, 0)
dev.off()

#############################
#  COVID Annual Cases Data  #
#############################
# Load datasets
covid_cases <- read.csv("data/Cases.csv")
state_populations <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv")

# Wrangle state population data
state_pop_20 <- state_populations %>% 
  select(NAME, POPESTIMATE2020) %>% 
  # Filter for state names
  filter(NAME %in% state.name) %>% 
  # Get column for state abbreviations
  mutate(state = state.abb[which(state.name == NAME)]) %>%
  select(-NAME)

# Sum up cases for each week and divide by state population
annual_cases <- covid_cases %>% 
  # Change date format
  mutate(date = mdy(submission_date)) %>%
  # Filter for relevant dates
  dplyr::filter(date >= "2020-02-06" & date <= "2020-12-31") %>%
  group_by(state) %>%
  summarize(annual_covid_case = sum(new_case)) %>%
  # Join state and COVID cases data
  left_join(state_pop_20, by = "state") %>% 
  # Remove territories
  drop_na() %>%
  # Create new column for proportion of cases over state population
  mutate(covid_prop = annual_covid_case/POPESTIMATE2020) %>%
  select(state, covid_prop)

# Convert data into csv file
write.csv(annual_cases, file = "shiny_state-restrictions-network/proportion_annual_covid_cases_data.csv")

#################################
#    Restaurant Restrictions    #
#################################
# Recode restriction values to match ones on CDC website
restaurants$Action[is.na(restaurants$Action)] <- "No restriction found"
restaurants$Action <- gsub("Authorized to fully reopen", 
                           "No restriction found", 
                           restaurants$Action)

# Wrangle restaurants dataset
restaurants_new <- restaurants %>% 
  as_tibble() %>%
  rename(Restaurant_Action = Action) %>%
  # Mutate dates into year-month-day format using lubridate function
  mutate(Day = mdy(restaurants$date)) %>%
  # Filter to include all dates in 2020
  dplyr::filter(Day >= "2020-03-15" & Day <= "2020-12-31",
                State_Tribe_Territory != "AS" &
                  State_Tribe_Territory != "GU" & 
                  State_Tribe_Territory != "MP" &
                  State_Tribe_Territory != "PR" & 
                  State_Tribe_Territory != "VI" &
                  State_Tribe_Territory != "DC") %>%
  select(Restaurant_Action, State_Tribe_Territory, Day) %>%
  # Count the number of instances of each restriction by each state and day
  group_by(State_Tribe_Territory, Day) %>%
  count(Restaurant_Action) %>%
  # Only keep the restriction that was enacted the most for each state and day
  group_by(State_Tribe_Territory, Day) %>%
  slice(which.max(n)) %>%
  # Remove counts column
  select(-n) %>%
  # Pivot to create columns for each state
  pivot_wider(names_from = State_Tribe_Territory, 
              values_from = Restaurant_Action)


# Restaurants Network Data
# Create a matrix with all state combinations
nameMat <- t(combn(names(restaurants_new[,-1]), 2))

# Pre-allocate space for a count vector
restaurants_overLap <- integer(nrow(nameMat))

# Loop through name combos 
for(i in 1:nrow(nameMat)) {
  data_combo <- restaurants_new[, nameMat[i,]]
  # get the number of days each pair of states shared and put result in count vector
  restaurants_overLap[i] <- sum(data_combo[1] == data_combo[2])
}

# Construct data.frame 
restaurants_final <- data.frame("state_one" = nameMat[,1], 
                                "state_two" = nameMat[,2], 
                                "restaurants_overLap"= restaurants_overLap)

# Create an igraph object
restaurants_igraph <- graph_from_data_frame(restaurants_final, directed = FALSE)

# Convert igraph to visNetwork format
restaurants_visNetwork <- toVisNetworkData(restaurants_igraph)

# Wrangle edges data
restaurants_edges <- restaurants_visNetwork$edges %>%
  # Create a new column for edge width
  mutate(value = as.numeric(restaurants_overLap),
         width = value/100) %>%
  select(-value) 

restaurants_edges <- restaurants_edges %>%
  # Create a new column for edge labels that appear when hovered over
  mutate(title = paste0(restaurants_edges$restaurants_overLap, " days shared in 2020"))

# Wrangle nodes data
restaurants_nodes <- restaurants_visNetwork$nodes %>%
  # Join annual cases dataset
  inner_join(annual_cases, by = c("id" = "state")) %>%
  # Create a new column for node color, colored by annual COVID cases in relation to color palette legend
  mutate(color = eigScalePal(num_colors)[cut(covid_prop, breaks = num_colors)]) %>%
  select(-c(covid_prop))

# Convert data into csv file
write.csv(restaurants_edges, 
          file = "shiny_state-restrictions-network/restaurants_edges_data.csv")
write.csv(restaurants_nodes, 
          file = "shiny_state-restrictions-network/restaurants_nodes_data.csv")

# Repeat the wrangling above for the four other datasets

##########################
#    Bar Restrictions    #
##########################
# Recode restriction values to match ones on CDC website
bars$Action[is.na(bars$Action)] <- "No restriction found"
bars$Action <- gsub("Authorized to fully reopen", 
                    "No restriction found", 
                    bars$Action)

# Wrangle bars dataset
bars_new <- bars %>% 
  as_tibble() %>%
  rename(Bar_Action = Action) %>%
  mutate(Day = mdy(bars$date)) %>%
  dplyr::filter(Day >= "2020-03-15" & Day <= "2020-12-31",
                State_Tribe_Territory != "AS" &
                  State_Tribe_Territory != "GU" & 
                  State_Tribe_Territory != "MP" &
                  State_Tribe_Territory != "PR" & 
                  State_Tribe_Territory != "VI" &
                  State_Tribe_Territory != "DC") %>%
  select(Bar_Action, State_Tribe_Territory, Day) %>%
  group_by(State_Tribe_Territory, Day) %>%
  count(Bar_Action) %>%
  group_by(State_Tribe_Territory, Day) %>%
  slice(which.max(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = State_Tribe_Territory, 
              values_from = Bar_Action)

# Bars Network Data
nameMat <- t(combn(names(bars_new[,-1]), 2))

bars_overLap <- integer(nrow(nameMat))

for(i in 1:nrow(nameMat)) {
  data_combo <- bars_new[, nameMat[i,]]
  bars_overLap[i] <- sum(data_combo[1] == data_combo[2])
}

bars_final <- data.frame("state_one" = nameMat[,1], 
                         "state_two" = nameMat[,2], 
                         "bars_overLap"= bars_overLap)

# Create an igraph object
bars_igraph <- graph_from_data_frame(bars_final, directed = FALSE)

# Convert igraph to visNetwork format
bars_visNetwork <- toVisNetworkData(bars_igraph)

# Wrangle edges data
bars_edges <- bars_visNetwork$edges %>%
  mutate(value = as.numeric(bars_overLap),
         width = value/100) %>%
  select(-value) 

bars_edges <- bars_edges %>%
  mutate(title = paste0(bars_edges$bars_overLap, " days shared in 2020"))

# Wrangle nodes data
bars_nodes <- bars_visNetwork$nodes %>%
  inner_join(annual_cases, by = c("id" = "state")) %>%
  mutate(color = eigScalePal(num_colors)[cut(covid_prop, breaks = num_colors)]) %>%
  select(-c(covid_prop))

# Convert data into csv file
write.csv(bars_edges, 
          file = "shiny_state-restrictions-network/bars_edges_data.csv")
write.csv(bars_nodes, 
          file = "shiny_state-restrictions-network/bars_nodes_data.csv")

######################
#    Mask Mandates   #
######################
# Recode restriction values to match ones on CDC website
mask_mandates$Face_Masks_Required_in_Public[is.na(mask_mandates$Face_Masks_Required_in_Public)] <- "No"

# Wrangle mask mandates dataset
mask_mandates_new <- mask_mandates %>%
  as_tibble() %>%
  rename(Mask_Action = Face_Masks_Required_in_Public) %>%
  mutate(Day = mdy(mask_mandates$date)) %>%
  dplyr::filter(Day >= "2020-04-10" & Day <= "2020-12-31",
                State_Tribe_Territory != "AS" &
                  State_Tribe_Territory != "GU" & 
                  State_Tribe_Territory != "MP" &
                  State_Tribe_Territory != "PR" & 
                  State_Tribe_Territory != "VI" &
                  State_Tribe_Territory != "DC") %>%
  select(Mask_Action, State_Tribe_Territory, Day) %>%
  group_by(State_Tribe_Territory, Day) %>%
  count(Mask_Action) %>%
  group_by(State_Tribe_Territory, Day) %>%
  slice(which.max(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = State_Tribe_Territory, 
              values_from = Mask_Action)

# Mask Mandates Network Data
nameMat <- t(combn(names(mask_mandates_new[,-1]), 2))

mask_mandates_overLap <- integer(nrow(nameMat))

for(i in 1:nrow(nameMat)) {
  data_combo <- mask_mandates_new[, nameMat[i,]]
  mask_mandates_overLap[i] <- sum(data_combo[1] == data_combo[2])
}

mask_mandates_final <- data.frame("state_one" = nameMat[,1], 
                                  "state_two" = nameMat[,2], 
                                  "mask_mandates_overLap"= mask_mandates_overLap)

# Create an igraph object
mask_mandates_igraph <- graph_from_data_frame(mask_mandates_final, directed = FALSE)

# Convert igraph to visNetwork format
mask_mandates_visNetwork <- toVisNetworkData(mask_mandates_igraph)

# Wrangle edges data
mask_mandates_edges <- mask_mandates_visNetwork$edges %>%
  mutate(value = as.numeric(mask_mandates_overLap),
         width = value/100) %>%
  select(-value) 

mask_mandates_edges <- mask_mandates_edges %>%
  mutate(title = paste0(mask_mandates_edges$mask_mandates_overLap, " days shared in 2020"))

# Wrangle nodes data
mask_mandates_nodes <- mask_mandates_visNetwork$nodes %>%
  inner_join(annual_cases, by = c("id" = "state")) %>%
  mutate(color = eigScalePal(5)[cut(covid_prop, breaks = 5)]) %>%
  select(-c(covid_prop))

# Convert data into csv file
write.csv(mask_mandates_edges, 
          file = "shiny_state-restrictions-network/mask_mandates_edges_data.csv")
write.csv(mask_mandates_nodes, 
          file = "shiny_state-restrictions-network/mask_mandates_nodes_data.csv")

######################
#   Gathering Bans   #
######################
# Wrangle gathering bans dataset
gathering_bans_new <- gathering_bans %>% 
  as_tibble() %>%
  rename(GatheringBan_Action = General_GB_order_group) %>%
  mutate(Day = mdy(gathering_bans$date)) %>%
  dplyr::filter(Day >= "2020-03-11" & Day <= "2020-12-31",
                State_Tribe_Territory != "AS" &
                  State_Tribe_Territory != "GU" & 
                  State_Tribe_Territory != "MP" &
                  State_Tribe_Territory != "PR" & 
                  State_Tribe_Territory != "VI" &
                  State_Tribe_Territory != "DC") %>%
  select(GatheringBan_Action, State_Tribe_Territory, Day) %>%
  group_by(State_Tribe_Territory, Day) %>%
  count(GatheringBan_Action) %>%
  group_by(State_Tribe_Territory, Day) %>%
  slice(which.max(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = State_Tribe_Territory, 
              values_from = GatheringBan_Action)

# Gathering Bans Network Data
nameMat <- t(combn(names(gathering_bans_new[,-1]), 2))

gathering_bans_overLap <- integer(nrow(nameMat))

for(i in 1:nrow(nameMat)) {
  data_combo <- gathering_bans_new[, nameMat[i,]]
  gathering_bans_overLap[i] <- sum(data_combo[1] == data_combo[2])
}

gathering_bans_final <- data.frame("state_one" = nameMat[,1], 
                                   "state_two" = nameMat[,2], 
                                   "gathering_bans_overLap"= gathering_bans_overLap)

# Create an igraph object
gathering_bans_igraph <- graph_from_data_frame(gathering_bans_final, directed = FALSE)

# Convert igraph to visNetwork format
gathering_bans_visNetwork <- toVisNetworkData(gathering_bans_igraph)

# Wrangle edges data
gathering_ban_edges <- gathering_bans_visNetwork$edges %>%
  mutate(value = as.numeric(gathering_bans_overLap),
         width = value/100) %>%
  select(-value) 

gathering_ban_edges <- gathering_ban_edges %>%
  mutate(title = paste0(gathering_ban_edges$gathering_bans_overLap, " days shared in 2020"))

# Wrangle nodes data
gathering_ban_nodes <- gathering_bans_visNetwork$nodes %>%
  inner_join(annual_cases, by = c("id" = "state")) %>%
  mutate(color = eigScalePal(num_colors)[cut(covid_prop, breaks = num_colors)]) %>%
  select(-c(covid_prop))

# Convert data into csv file
write.csv(gathering_ban_edges, 
          file = "shiny_state-restrictions-network/gathering_ban_edges_data.csv")
write.csv(gathering_ban_nodes, 
          file = "shiny_state-restrictions-network/gathering_ban_nodes_data.csv")

##########################
#   Stay at Home Orders  #
##########################
# Recode restriction values to match ones on CDC website
stay_at_home_orders$Stay_at_Home_Order_Recommendation[is.na(stay_at_home_orders$Stay_at_Home_Order_Recommendation)] <- "No restriction found"
stay_at_home_orders$Stay_at_Home_Order_Recommendation <- gsub("Mandatory only for at-risk individuals in the jurisdiction", 
                                                              "Mandatory for some people", 
                                                              stay_at_home_orders$Stay_at_Home_Order_Recommendation)
stay_at_home_orders$Stay_at_Home_Order_Recommendation <- gsub("Mandatory only for all individuals in certain areas of the jurisdiction", 
                                                              "Mandatory for some people", 
                                                              stay_at_home_orders$Stay_at_Home_Order_Recommendation)
stay_at_home_orders$Stay_at_Home_Order_Recommendation <- gsub("No order for individuals to stay home", 
                                                              "No restriction found", 
                                                              stay_at_home_orders$Stay_at_Home_Order_Recommendation)


# Wrangle stay at home orders
stay_at_home_orders_new <- stay_at_home_orders %>% 
  as_tibble() %>%
  rename(StayHome_Action = Stay_at_Home_Order_Recommendation) %>%
  mutate(Day = mdy(stay_at_home_orders$date)) %>%
  dplyr::filter(Day >= "2020-03-15" & Day <= "2020-12-31",
                State_Tribe_Territory != "AS" &
                  State_Tribe_Territory != "GU" & 
                  State_Tribe_Territory != "MP" &
                  State_Tribe_Territory != "PR" & 
                  State_Tribe_Territory != "VI" &
                  State_Tribe_Territory != "DC") %>%
  select(StayHome_Action, State_Tribe_Territory, Day) %>%
  group_by(State_Tribe_Territory, Day) %>%
  count(StayHome_Action) %>%
  group_by(State_Tribe_Territory, Day) %>%
  slice(which.max(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = State_Tribe_Territory, 
              values_from = StayHome_Action)

# Stay-at-home Orders Network Data
nameMat <- t(combn(names(stay_at_home_orders_new[,-1]), 2))

stay_at_home_orders_overLap <- integer(nrow(nameMat))

for(i in 1:nrow(nameMat)) {
  data_combo <- stay_at_home_orders_new[, nameMat[i,]]
  stay_at_home_orders_overLap[i] <- sum(data_combo[1] == data_combo[2])
}

stay_at_home_orders_final <- data.frame("state_one" = nameMat[,1], 
                                        "state_two" = nameMat[,2], 
                                        "stay_at_home_orders_overLap"= stay_at_home_orders_overLap)

# Create an igraph object
stay_at_home_orders_igraph <- graph_from_data_frame(stay_at_home_orders_final, directed = FALSE)

# Convert igraph to visNetwork format
stay_at_home_orders_visNetwork <- toVisNetworkData(stay_at_home_orders_igraph)

# Wrangle edges data
stay_at_home_orders_edges <- stay_at_home_orders_visNetwork$edges %>%
  mutate(value = as.numeric(stay_at_home_orders_overLap),
         width = value/100) %>%
  select(-value) 

stay_at_home_orders_edges <- stay_at_home_orders_edges %>%
  mutate(title = paste0(stay_at_home_orders_edges$stay_at_home_orders_overLap, " days shared in 2020"))

# Wrangle nodes data
stay_at_home_orders_nodes <- stay_at_home_orders_visNetwork$nodes %>%
  inner_join(annual_cases, by = c("id" = "state")) %>%
  mutate(color = eigScalePal(num_colors)[cut(covid_prop, breaks = num_colors)]) %>%
  select(-c(covid_prop))

# Convert data into csv file
write.csv(stay_at_home_orders_edges, 
          file = "shiny_state-restrictions-network/stay_at_home_orders_edges_data.csv")
write.csv(stay_at_home_orders_nodes, 
          file = "shiny_state-restrictions-network/stay_at_home_orders_nodes_data.csv")


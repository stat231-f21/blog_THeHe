# Load packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(mclust)
library(ggrepel)
library(plotly)
library(gganimate)
library(gifski)

# Load Pulse Survey datasets
week22 <- read.csv("data/pulse/pulse2021_puf_22.csv")
week23 <- read.csv("data/pulse/pulse2021_puf_23.csv")
week24 <- read.csv("data/pulse/pulse2021_puf_24.csv")
week25 <- read.csv("data/pulse/pulse2021_puf_25.csv")
week26 <- read.csv("data/pulse/pulse2021_puf_26.csv")
week27 <- read.csv("data/pulse/pulse2021_puf_27.csv")
week28 <- read.csv("data/pulse/pulse2021_puf_28.csv")
week29 <- read.csv("data/pulse/pulse2021_puf_29.csv")
week30 <- read.csv("data/pulse/pulse2021_puf_30.csv")
week31 <- read.csv("data/pulse/pulse2021_puf_31.csv")
week32 <- read.csv("data/pulse/pulse2021_puf_32.csv")
week33 <- read.csv("data/pulse/pulse2021_puf_33.csv")
week34 <- read.csv("data/pulse/pulse2021_puf_34.csv")
week35 <- read.csv("data/pulse/pulse2021_puf_35.csv")
week36 <- read.csv("data/pulse/pulse2021_puf_36.csv")
week37 <- read.csv("data/pulse/pulse2021_puf_37.csv")
week38 <- read.csv("data/pulse/pulse2021_puf_38.csv")
week39 <- read.csv("data/pulse/pulse2021_puf_39.csv")

# # Load COVID cases and state population data
# 
# covid_cases <- read.csv("data/cases/Cases.csv")
# state_populations <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv")
# 
# # Load COVID state restrictions data
# 
# restaurants <- read.csv("data/restrictions/Restaurants.csv")
# bars <- read.csv("data/restrictions/Bars.csv")
# mask_mandates <- read.csv("data/restrictions/Mask_Mandates.csv")
# gathering_bans <- read.csv("data/restrictions/Gathering_Bans.csv")
# stay_at_home_orders <- read.csv("data/restrictions/Stay-At-Home_Orders.csv")

###################################
# Wrangle pulse survey data sets  #
###################################

# Prior to joining weeks, select variables of interest
for (i in 22:39) {
  assign(paste0("week", i),
         eval(parse(text = paste0("week", i))) %>% select(TBIRTH_YEAR, RHISPANIC, RRACE, ANXIOUS, WORRY, INTEREST, DOWN, PRESCRIPT, MH_SVCS, MH_NOTGET, EST_ST, WEEK, HLTHINS1, HLTHINS2, HLTHINS3, HLTHINS4, HLTHINS5, HLTHINS6, HLTHINS7, HLTHINS8))
}

# Join pulse data sets with dates from weeks 22 to 32
first_phase <- rbind(week22, week23, week24, week25, week26, week27, week28, week29, week30, week31, week32)

# Join pulse data sets with dates from weeks 33 to 39
second_phase <- rbind(week33, week34, week35, week36, week37, week38, week39)

# Mutate data sets to include a variable indicating whether it's from first or second phase
first_phase <- first_phase %>%
  mutate(phase = "first") %>%
  select(TBIRTH_YEAR, RHISPANIC, RRACE, ANXIOUS, WORRY, INTEREST, DOWN, PRESCRIPT, MH_SVCS, MH_NOTGET, EST_ST, WEEK, phase,
         HLTHINS1, HLTHINS2, HLTHINS3, HLTHINS4, HLTHINS5, HLTHINS6, HLTHINS7, HLTHINS8)

second_phase <- second_phase %>%
  mutate(phase = "second") %>%
  select(TBIRTH_YEAR, RHISPANIC, RRACE, ANXIOUS, WORRY, INTEREST, DOWN, PRESCRIPT, MH_SVCS, MH_NOTGET, EST_ST, WEEK, phase,
         HLTHINS1, HLTHINS2, HLTHINS3, HLTHINS4, HLTHINS5, HLTHINS6, HLTHINS7, HLTHINS8)

# Join pulse survey data sets with dates from weeks 22 to 39
pulse_data <- rbind(first_phase, second_phase)

# Further wrangling 

pulse_college_data <- pulse_data %>%
  # Filter out people outside of age range 18-25
  filter(TBIRTH_YEAR %in% (1998:2003)) %>%
  # Create new column for race/ethnicity
  mutate(race_ethnicity = case_when(RHISPANIC == 2 ~ "Hispanic or Latino", 
                                    (RHISPANIC == 1 & RRACE == 1) ~ "Non-Hispanic White",
                                    (RHISPANIC == 1 & RRACE == 2) ~ "Non-Hispanic Black",
                                    (RHISPANIC == 1 & RRACE == 3) ~ "Non-Hispanic Asian",
                                    (RHISPANIC == 1 & RRACE == 4) ~ "Non-Hispanic Other/Multiple Races")) %>%
  # Filter out cases that did not respond to our variables of interest
  filter(ANXIOUS != -99, ANXIOUS != -88, DOWN != -99, DOWN != -88, PRESCRIPT != -99, PRESCRIPT != -88, MH_SVCS != -99, MH_SVCS != -88, MH_NOTGET != -99, MH_NOTGET != -88,
         HLTHINS1 != -99, HLTHINS1 != -88, HLTHINS2 != -99, HLTHINS2 != -88, HLTHINS3 != -99, HLTHINS3 != -88, HLTHINS4 != -99, HLTHINS4 != -88, HLTHINS5 != -99, HLTHINS5 != -88, HLTHINS6 != -99, HLTHINS6 != -88, HLTHINS7 != -99, HLTHINS7 != -88, HLTHINS8 != -99, HLTHINS8 != -88) %>% 
  # # Recode anxiety
  # mutate(anxiety = case_when(ANXIOUS == 1 ~ "No",
  #                            (ANXIOUS == 2 |  ANXIOUS == 3 |  ANXIOUS == 4) ~ "Yes")) %>%
  # # Recode depression
  # mutate(depression = case_when(DOWN == 1 ~ "No",
  #                               (DOWN == 2 |  DOWN == 3 |  DOWN == 4) ~ "Yes")) %>%
  # Recode states to two letter state codes
  mutate(state = case_when(EST_ST == "1" ~ "AL",
                           EST_ST == "2" ~ "AK",
                           EST_ST == "4" ~ "AZ",
                           EST_ST == "5" ~ "AR",
                           EST_ST == '6' ~ "CA",
                           EST_ST == '8' ~ "CO",
                           EST_ST == '9' ~ "CT",
                           EST_ST == '10' ~ "DE",
                           EST_ST == '12' ~ "FL",
                           EST_ST == '13' ~ "GA",
                           EST_ST == '15' ~ "HI",
                           EST_ST == '16' ~ "ID",
                           EST_ST == '17' ~ "IL",
                           EST_ST == '18' ~ "IN",
                           EST_ST == '19' ~ "IA",
                           EST_ST == '20' ~ "KS",
                           EST_ST == '21' ~ "KY",
                           EST_ST == '22' ~ "LA",
                           EST_ST == '23' ~ "ME",
                           EST_ST == '24' ~ "MD",
                           EST_ST == '25' ~ "MA",
                           EST_ST == '26' ~ "MI",
                           EST_ST == '27' ~ "MN",
                           EST_ST == '28' ~ "MS",
                           EST_ST == '29' ~ "MO",
                           EST_ST == '30' ~ "MT",
                           EST_ST == '31' ~ "NE",
                           EST_ST == '32' ~ "NV",
                           EST_ST == '33' ~ "NH",
                           EST_ST == '34' ~ "NJ",
                           EST_ST == '35' ~ "NM",
                           EST_ST == '36' ~ "NY",
                           EST_ST == '37' ~ "NC",
                           EST_ST == '38' ~ "ND",
                           EST_ST == '39' ~ "OH",
                           EST_ST == '40' ~ "OK",
                           EST_ST == '41' ~ "OR",
                           EST_ST == '42' ~ "PA",
                           EST_ST == '44' ~ "RI",
                           EST_ST == '45' ~ "SC",
                           EST_ST == '46' ~ "SD",
                           EST_ST == '47' ~ "TN",
                           EST_ST == '48' ~ "TX",
                           EST_ST == '49' ~ "UT",
                           EST_ST == '50' ~ "VT",
                           EST_ST == '51' ~ "VA",
                           EST_ST == '53' ~ "WA",
                           EST_ST == '54' ~ "WV",
                           EST_ST == '55' ~ "WI",
                           EST_ST == '56' ~ "WY")) %>%
  # Recode weeks to starting date
  mutate(week_start_date = case_when(WEEK == 22 ~ as.Date("2021-01-06"),
                                     WEEK == 23 ~ as.Date("2021-01-20"),
                                     WEEK == 24 ~ as.Date("2021-02-03"),
                                     WEEK == 25 ~ as.Date("2021-02-17"),
                                     WEEK == 26 ~ as.Date("2021-03-03"),
                                     WEEK == 27 ~ as.Date("2021-03-17"),
                                     WEEK == 28 ~ as.Date("2021-04-14"),
                                     WEEK == 29 ~ as.Date("2021-04-28"),
                                     WEEK == 30 ~ as.Date("2021-05-12"),
                                     WEEK == 31 ~ as.Date("2021-05-26"),
                                     WEEK == 32 ~ as.Date("2021-06-09"),
                                     WEEK == 33 ~ as.Date("2021-06-23"),
                                     WEEK == 34 ~ as.Date("2021-07-21"),
                                     WEEK == 35 ~ as.Date("2021-08-04"),
                                     WEEK == 36 ~ as.Date("2021-08-18"),
                                     WEEK == 37 ~ as.Date("2021-09-01"),
                                     WEEK == 38 ~ as.Date("2021-09-15"),
                                     WEEK == 39 ~ as.Date("2021-09-29"))) %>% 
  # Rename other columns
  rename(week = WEEK, 
         birth_year = TBIRTH_YEAR,
         prescription = PRESCRIPT,
         mental_health_services = MH_SVCS,
         no_access  = MH_NOTGET) %>% 
  mutate(healthcare = case_when((HLTHINS1 == 1 | HLTHINS2 == 1 | HLTHINS3 == 1 | HLTHINS4 == 1 | HLTHINS5 == 1 | HLTHINS6 == 1 | HLTHINS7 == 1 | HLTHINS8 == 1) ~ 1,
                                (HLTHINS1 == 2 & HLTHINS2 == 2 & HLTHINS3 == 2 & HLTHINS4 == 2 & HLTHINS5 == 2 & HLTHINS6 == 2 & HLTHINS7 == 2 & HLTHINS8 == 2) ~ 2))

#####################
# Kmeans clustering #
#####################

# compare distribution to distribution of population for each group 
# xcould also literally try plotting these like density plots... except that the values are discrete 1-4

# 11/9
# check percentages of pulse respondents in each group we care about

racial_ethnic_totals <- pulse_college_data %>% 
  group_by(race_ethnicity) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n),
         prop_respondents = n/total)

# state_totals <- pulse_college_data %>% 
#   group_by(state) %>% 
#   count() %>% 
#   ungroup() %>% 
#   mutate(total = sum(n),
#          prop_respondents = n/total)

# SAMPLE 

# plot sample for funsies
set.seed(1984)
pulse_sample <- pulse_college_data %>% 
  sample_n(500)

# ggplot(data = pulse_sample, aes(x = ANXIOUS, y = DOWN)) +
#   geom_point(position = "jitter")

# cluster

set.seed(1984)
clustering_vars <- c("ANXIOUS", "DOWN", "prescription", "mental_health_services", "no_access")
pulse_clusters <- pulse_sample %>% 
  select(clustering_vars) %>% 
  kmeans(centers = 10, nstart = 20)

pulse_sample <- pulse_sample %>% 
  mutate(clusters = factor(pulse_clusters$cluster))

plot_ly(pulse_sample, x = ~jitter(ANXIOUS), y = ~jitter(DOWN), z = ~jitter(prescription), type="scatter3d", mode="markers", color = ~clusters)

ggplot(data = pulse_sample, aes(x = ANXIOUS, y = DOWN)) +
  geom_point(aes(color = clusters), position = "jitter") +
  geom_text_repel(aes(label = race_ethnicity, color = clusters, size = 3))


# For each cluster, calculate percent of each cluster that is each race/ethnicity 
set.seed(1984)
pulse_sample_grouped <- pulse_sample %>% 
  group_by(clusters) %>% 
  count(race_ethnicity) %>% 
  mutate(total = sum(n),
         percent_type = n/total) %>% 
  pivot_wider(names_from = race_ethnicity,
              values_from = percent_type) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  janitor::clean_names() %>% 
  summarise(hispanic_or_latino = sum(hispanic_or_latino),
            non_hispanic_asian = sum(non_hispanic_asian),
            non_hispanic_white = sum(non_hispanic_white),
            non_hispanic_other_multiple_races = sum(non_hispanic_other_multiple_races),
            non_hispanic_black = sum(non_hispanic_black))


# LOL, elbow plot
elbow_plot <- data.frame(clusters = 1:10,
                         within_ss = rep(NA, 20))

set.seed(1984)
for (i in 1:10){
  pulse_out <- pulse_sample %>% 
    select(clustering_vars) %>% 
    kmeans(centers = i, nstart = 20)
  
  elbow_plot$within_ss[i] <- pulse_out$tot.withinss
}

# Construct elbow plot
ggplot(elbow_plot, aes(x = clusters, y = within_ss)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of clusters (k)", y = expression("Total W"[k]))

################
# FULL DATASET #
################


# cluster

set.seed(1984)
# clustering_vars <- c("ANXIOUS", "DOWN", "prescription", "mental_health_services", "no_access")
clustering_vars <- c("prescription", "mental_health_services", "no_access", "healthcare")
# clustering_vars <- c("prescription", "mental_health_services", "no_access")

pulse_all_clusters <- pulse_college_data %>% 
  select(clustering_vars) %>% 
  kmeans(centers = 5, nstart = 25)

pulse_clustered_data <- pulse_college_data %>% 
  mutate(clusters = factor(pulse_all_clusters$cluster))

plot_ly(pulse_clustered_data, x = ~jitter(prescription), y = ~jitter(mental_health_services), z = ~jitter(no_access), type="scatter3d", mode="markers", color = ~clusters)
# 
# ggplot(data = pulse_sample, aes(x = ANXIOUS, y = DOWN)) +
#   geom_point(aes(color = clusters), position = "jitter") +
#   geom_text_repel(aes(label = race_ethnicity, color = clusters, size = 3))


# For each cluster, calculate percent of each cluster that is each race/ethnicity 
set.seed(1984)
pulse_grouped <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(race_ethnicity) %>% 
  mutate(total = sum(n),
         percent_type = n/total) %>% 
  pivot_wider(names_from = race_ethnicity,
              values_from = percent_type) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  janitor::clean_names() %>% 
  summarise(hispanic_or_latino = sum(hispanic_or_latino),
            non_hispanic_asian = sum(non_hispanic_asian),
            non_hispanic_white = sum(non_hispanic_white),
            non_hispanic_other_multiple_races = sum(non_hispanic_other_multiple_races),
            non_hispanic_black = sum(non_hispanic_black))

# for each cluster, find the percent of individuals in the cluster with healthcare
# percent with each of the other variables
# none, acute, or chronic anxiety or depression

pres_sum <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(prescription) %>% 
  mutate(total = sum(n),
         percent_type = n/total,
         type = "presc") %>% 
  rename(value = prescription) %>% 
  select(-c(n, total))

mhs_sum <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(mental_health_services) %>% 
  mutate(total = sum(n),
         percent_type = n/total,
         type = "mhs") %>% 
  rename(value = mental_health_services) %>% 
  select(-c(n, total))

no_a_sum <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(no_access) %>% 
  mutate(total = sum(n),
         percent_type = n/total,
         type = "no_a") %>% 
  rename(value = no_access) %>% 
  select(-c(n, total)) %>% 
  mutate(value = recode(value, `1` = 2, `2` = 1))

healthcare_sum <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(healthcare) %>% 
  mutate(total = sum(n),
         percent_type = n/total,
         type = "healthcare") %>% 
  rename(value = healthcare) %>% 
  select(-c(n, total))


# https://ugoproto.github.io/ugo_r_doc/pdf/gganimate.pdf

meep <- rbind(pres_sum, mhs_sum, no_a_sum, healthcare_sum)
# meep <- no_a_sum %>% 
#   full_join(pres_sum, by = "clusters")

# individuals

stacked <- ggplot(data = pres_sum) +
  geom_col(mapping = aes(x = clusters,
                         y = percent_type,
                         fill = value)) +
  coord_flip()
stacked

stacked2 <- ggplot(data = no_a_sum) +
  geom_col(mapping = aes(x = clusters,
                         y = percent_type,
                         fill = value)) +
  coord_flip()
stacked2


# attempting the animated one 

# animate_hc <- ggplot(data = meep) +
#   geom_col(mapping = aes(x = clusters,
#                          y = percent_type,
#                          fill = value)) +
#   transition_states(type, transition_length = 3, state_length = 1) +
#   enter_grow() +
#   exit_shrink()

animate_hc <- ggplot(data = meep) +
  geom_col(mapping = aes(x = clusters,
                         y = percent_type,
                         fill = value)) +
  transition_states(type, transition_length = 3, state_length = 1) +
  enter_drift(x_mod = 0, y_mod = meep$percent_type) +
  exit_drift(x_mod = 0, y_mod = meep$percent_type)

# animate_hc <- ggplot(data = meep) +
#   geom_col(data = meep %>% filter(value == 1),
#            mapping = aes(x = clusters,
#                          y = percent_type),
#            fill = "coral2") +
#   geom_col(data = meep %>% filter(value == 2),
#            mapping = aes(x = clusters,
#                          y = percent_type),
#            fill = "deepskyblue1") +
#   transition_states(type, transition_length = 3, state_length = 1) +
#   enter_drift(x_mod = 0, y_mod = 1) +
#   exit_drift(x_mod = 0, y_mod = meep$percent_type) 
# +
  # enter_drift("2", x_mod = 0, y_mod = -meep$percent_type) +
  # exit_drift("2", x_mod = 0, y_mod = -meep$percent_type)

# 

# animate_hc <- ggplot(data = meep) +
#   geom_col(mapping = aes(x = clusters,
#                          y = percent_type,
#                          fill = value)) +
#   transition_states(type, transition_length = 3, state_length = 1) +
#   enter_drift(x_mod = 0, y_mod = meep$percent_type) +
#   exit_drift(x_mod = 0, y_mod = meep$percent_type)

# animate_hc <- ggplot(data = meep, aes(x = clusters, y = percent_type)) +
#   geom_col(mapping = aes(fill = value), position = "identity", width = 0.8) +
#   transition_states(type, transition_length = 3, state_length = 1) +
#   enter_drift(x_mod = 0, y_mod = meep$percent_type) +
#   exit_drift(x_mod = 0, y_mod = meep$percent_type)

animate(animate_hc, renderer=gifski_renderer("test.gif"))




# Check by state
# Same thing happening
# pulse_grouped_states <- pulse_clustered_data %>% 
#   group_by(clusters) %>% 
#   count(state) %>% 
#   mutate(total = sum(n),
#          percent_type = n/total) %>% 
#   pivot_wider(names_from = state,
#               values_from = percent_type) %>% 
#   mutate_all(~replace(., is.na(.), 0)) %>% 
#   janitor::clean_names() %>% 
#   summarise(ak = sum(ak), 
#             al = sum(al),
#             ar = sum(ar),
#             az = sum(az)) # etc.
# 
# # Check by other mental health variables
# set.seed(1984)
# pulse_grouped_mh <- pulse_clustered_data %>% 
#   mutate(no_access = as.character(no_access)) %>% 
#   group_by(clusters) %>% 
#   count(no_access) %>% 
#   mutate(total = sum(n),
#          prop_type = n/total) %>% 
#   pivot_wider(names_from = no_access,
#               values_from = prop_type) %>% 
#   mutate_all(~replace(., is.na(.), 0)) %>% 
#   janitor::clean_names() %>% 
#   summarise(x1 = sum(x1), 
#             x2 = sum(x2)) # etc.



elbow_plot <- data.frame(clusters = 1:10,
                         within_ss = rep(NA, 10))

set.seed(1984)
for (i in 1:10){
  pulse_out <- pulse_clustered_data %>% 
    select(clustering_vars) %>% 
    kmeans(centers = i, nstart = 25)
  
  elbow_plot$within_ss[i] <- pulse_out$tot.withinss
}

# Construct elbow plot
ggplot(elbow_plot, aes(x = clusters, y = within_ss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of clusters (k)", y = expression("Total W"[k]))


# Quantify changes in proportion



###########
# Dim Red #
###########
pulse_college_data <- pulse_college_data %>% 
  tibble::rowid_to_column("index")
pulse_svd <- pulse_college_data %>% 
  select(-c(birth_year, RHISPANIC, RRACE, EST_ST, week, phase, state, week_start_date, race_ethnicity)) %>% 
  svd()

num_clusters <- 5
library(broom)
pulse_svd_tidy <- pulse_svd %>% 
  tidy(matrix = "u") %>% 
  filter(PC < num_clusters) %>% 
  mutate(PC = paste0("pc_", PC)) %>% 
  pivot_wider(names_from = PC, values_from = value) %>%
  select(-row)

clusts <- pulse_svd_tidy %>% 
  kmeans(centers = num_clusters)

tidy(clusts)


pulse_svd_clust <- pulse_college_data %>% 
  mutate(clusters = factor(clusts$cluster))
mosaic::tally(race_ethnicity ~ clusters, data = pulse_svd_clust)

pulses <- clusts %>%
  augment(pulse_svd_tidy)

ggplot(data = pulses, aes(x = pc_1, y = pc_2)) +
  geom_point(aes(x = 0, y = 0), color = "red", shape = 1, size = 7) + 
  geom_point(size = 5, alpha = 0.6, aes(color = .cluster)) +
  xlab("Best Vector from SVD") + 
  ylab("Second Best Vector from SVD") + 
  scale_color_brewer(palette = "Set2")





# ESSENTIALLY IGNORE THIS

###############################
# Try the yes/no distinction #
##############################
# Changes nothing!

pulse_college_data <- pulse_data %>%
  # Filter out people outside of age range 18-25
  filter(TBIRTH_YEAR %in% (1998:2003)) %>%
  # Create new column for race/ethnicity
  mutate(race_ethnicity = case_when(RHISPANIC == 2 ~ "Hispanic or Latino", 
                                    (RHISPANIC == 1 & RRACE == 1) ~ "Non-Hispanic White",
                                    (RHISPANIC == 1 & RRACE == 2) ~ "Non-Hispanic Black",
                                    (RHISPANIC == 1 & RRACE == 3) ~ "Non-Hispanic Asian",
                                    (RHISPANIC == 1 & RRACE == 4) ~ "Non-Hispanic Other/Multiple Races")) %>%
  # Filter out cases that did not respond to our variables of interest
  filter(ANXIOUS != -99, ANXIOUS != -88, DOWN != -99, DOWN != -88, PRESCRIPT != -99, PRESCRIPT != -88, MH_SVCS != -99, MH_SVCS != -88, MH_NOTGET != -99, MH_NOTGET != -88) %>% 
  # Recode anxiety
  mutate(anxiety = case_when(ANXIOUS == 1 ~ 1,
                             (ANXIOUS == 2 |  ANXIOUS == 3 |  ANXIOUS == 4) ~ 2)) %>%
  # Recode depression
  mutate(depression = case_when(DOWN == 1 ~ 1,
                                (DOWN == 2 |  DOWN == 3 |  DOWN == 4) ~ 2)) %>%
  # Recode states to two letter state codes
  mutate(state = case_when(EST_ST == "1" ~ "AL",
                           EST_ST == "2" ~ "AK",
                           EST_ST == "4" ~ "AZ",
                           EST_ST == "5" ~ "AR",
                           EST_ST == '6' ~ "CA",
                           EST_ST == '8' ~ "CO",
                           EST_ST == '9' ~ "CT",
                           EST_ST == '10' ~ "DE",
                           EST_ST == '12' ~ "FL",
                           EST_ST == '13' ~ "GA",
                           EST_ST == '15' ~ "HI",
                           EST_ST == '16' ~ "ID",
                           EST_ST == '17' ~ "IL",
                           EST_ST == '18' ~ "IN",
                           EST_ST == '19' ~ "IA",
                           EST_ST == '20' ~ "KS",
                           EST_ST == '21' ~ "KY",
                           EST_ST == '22' ~ "LA",
                           EST_ST == '23' ~ "ME",
                           EST_ST == '24' ~ "MD",
                           EST_ST == '25' ~ "MA",
                           EST_ST == '26' ~ "MI",
                           EST_ST == '27' ~ "MN",
                           EST_ST == '28' ~ "MS",
                           EST_ST == '29' ~ "MO",
                           EST_ST == '30' ~ "MT",
                           EST_ST == '31' ~ "NE",
                           EST_ST == '32' ~ "NV",
                           EST_ST == '33' ~ "NH",
                           EST_ST == '34' ~ "NJ",
                           EST_ST == '35' ~ "NM",
                           EST_ST == '36' ~ "NY",
                           EST_ST == '37' ~ "NC",
                           EST_ST == '38' ~ "ND",
                           EST_ST == '39' ~ "OH",
                           EST_ST == '40' ~ "OK",
                           EST_ST == '41' ~ "OR",
                           EST_ST == '42' ~ "PA",
                           EST_ST == '44' ~ "RI",
                           EST_ST == '45' ~ "SC",
                           EST_ST == '46' ~ "SD",
                           EST_ST == '47' ~ "TN",
                           EST_ST == '48' ~ "TX",
                           EST_ST == '49' ~ "UT",
                           EST_ST == '50' ~ "VT",
                           EST_ST == '51' ~ "VA",
                           EST_ST == '53' ~ "WA",
                           EST_ST == '54' ~ "WV",
                           EST_ST == '55' ~ "WI",
                           EST_ST == '56' ~ "WY")) %>%
  # Recode weeks to starting date
  mutate(week_start_date = case_when(WEEK == 22 ~ as.Date("2021-01-06"),
                                     WEEK == 23 ~ as.Date("2021-01-20"),
                                     WEEK == 24 ~ as.Date("2021-02-03"),
                                     WEEK == 25 ~ as.Date("2021-02-17"),
                                     WEEK == 26 ~ as.Date("2021-03-03"),
                                     WEEK == 27 ~ as.Date("2021-03-17"),
                                     WEEK == 28 ~ as.Date("2021-04-14"),
                                     WEEK == 29 ~ as.Date("2021-04-28"),
                                     WEEK == 30 ~ as.Date("2021-05-12"),
                                     WEEK == 31 ~ as.Date("2021-05-26"),
                                     WEEK == 32 ~ as.Date("2021-06-09"),
                                     WEEK == 33 ~ as.Date("2021-06-23"),
                                     WEEK == 34 ~ as.Date("2021-07-21"),
                                     WEEK == 35 ~ as.Date("2021-08-04"),
                                     WEEK == 36 ~ as.Date("2021-08-18"),
                                     WEEK == 37 ~ as.Date("2021-09-01"),
                                     WEEK == 38 ~ as.Date("2021-09-15"),
                                     WEEK == 39 ~ as.Date("2021-09-29"))) %>% 
  # Rename other columns
  rename(week = WEEK, 
         birth_year = TBIRTH_YEAR,
         prescription = PRESCRIPT,
         mental_health_services = MH_SVCS,
         no_access  = MH_NOTGET)

racial_ethnic_totals <- pulse_college_data %>% 
  group_by(race_ethnicity) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n),
         prop_respondents = n/total)

state_totals <- pulse_college_data %>% 
  group_by(state) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n),
         prop_respondents = n/total)


################
# FULL DATASET #
################


# cluster

set.seed(1984)
clustering_vars <- c("anxiety", "depression", "prescription", "mental_health_services", "no_access")

pulse_all_clusters <- pulse_college_data %>% 
  select(clustering_vars) %>% 
  kmeans(centers = 5, nstart = 25)

pulse_clustered_data <- pulse_college_data %>% 
  mutate(clusters = factor(pulse_all_clusters$cluster))

# plot_ly(pulse_sample, x = ~jitter(ANXIOUS), y = ~jitter(DOWN), z = ~jitter(prescription), type="scatter3d", mode="markers", color = ~clusters)
# 
# ggplot(data = pulse_sample, aes(x = ANXIOUS, y = DOWN)) +
#   geom_point(aes(color = clusters), position = "jitter") +
#   geom_text_repel(aes(label = race_ethnicity, color = clusters, size = 3))


# For each cluster, calculate percent of each cluster that is each race/ethnicity 
set.seed(1984)
pulse_grouped <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(race_ethnicity) %>% 
  mutate(total = sum(n),
         percent_type = n/total) %>% 
  pivot_wider(names_from = race_ethnicity,
              values_from = percent_type) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  janitor::clean_names() %>% 
  summarise(hispanic_or_latino = sum(hispanic_or_latino),
            non_hispanic_asian = sum(non_hispanic_asian),
            non_hispanic_white = sum(non_hispanic_white),
            non_hispanic_other_multiple_races = sum(non_hispanic_other_multiple_races),
            non_hispanic_black = sum(non_hispanic_black))



elbow_plot <- data.frame(clusters = 1:5,
                         within_ss = rep(NA, 5))

set.seed(1984)
for (i in 1:5){
  pulse_out <- pulse_clustered_data %>% 
    select(clustering_vars) %>% 
    kmeans(centers = i, nstart = 25)
  
  elbow_plot$within_ss[i] <- pulse_out$tot.withinss
}

# Construct elbow plot
ggplot(elbow_plot, aes(x = clusters, y = within_ss)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:5) +
  labs(x = "Number of clusters (k)", y = expression("Total W"[k]))

# Load packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(mclust)
library(ggrepel)
library(plotly)
library(gganimate)
library(gifski)
library(igraph)
library(visNetwork)
library(ggnetwork)
library(grDevices)

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



###################################
# Wrangle pulse survey data sets  #
# Using same wrangling as before  #
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
  # Recode healthcare: any type of healthcare counts as having healthcare; answering no to all types of healthcare means the individual has no healthcare coverage
  mutate(healthcare = case_when((HLTHINS1 == 1 | HLTHINS2 == 1 | HLTHINS3 == 1 | HLTHINS4 == 1 | HLTHINS5 == 1 | HLTHINS6 == 1 | HLTHINS7 == 1 | HLTHINS8 == 1) ~ 1,
                                (HLTHINS1 == 2 & HLTHINS2 == 2 & HLTHINS3 == 2 & HLTHINS4 == 2 & HLTHINS5 == 2 & HLTHINS6 == 2 & HLTHINS7 == 2 & HLTHINS8 == 2) ~ 2)) %>% 
  # Rename other columns
  rename(week = WEEK, 
         birth_year = TBIRTH_YEAR,
         prescription = PRESCRIPT,
         mental_health_services = MH_SVCS,
         no_access  = MH_NOTGET)

#####################
# Kmeans clustering #
#####################

# Set seed for reproducibility and cluster with healthcare access variables
set.seed(1984)
clustering_vars <- c("prescription", "mental_health_services", "no_access", "healthcare")
pulse_all_clusters <- pulse_college_data %>% 
  select(clustering_vars) %>% 
  # Try 5 clusters (there are 5 racial/ethnic groups)
  kmeans(centers = 5, nstart = 25)

# Add cluster assignments to data frame
pulse_clustered_data <- pulse_college_data %>% 
  mutate(clusters = factor(pulse_all_clusters$cluster))

# Write to .csv for Shiny app
write.csv(pulse_clustered_data, file = "shiny_3D-clusters/pulse_clustered_data.csv")

# Visualize one 3D plot of clusters
# Since all respondents' answers are 1 or 2, use jitter for better visualization
# See "shiny_3D-clusters/app.R" for Shiny app adaptation of this plot
plot_ly(pulse_clustered_data, x = ~jitter(prescription), y = ~jitter(mental_health_services), z = ~jitter(healthcare), type="scatter3d", mode="markers", color = ~clusters)

# Check usefulness of 5 clusters with elbow plot
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
clusters_elbow <- ggplot(elbow_plot, aes(x = clusters, y = within_ss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of clusters (k)", y = expression("Total Withins"[k]))
clusters_elbow
# Save as .png
png(filename="images_and_plots/clusters_elbow.png")
plot(clusters_elbow)
dev.off()
# Indeed, 5 or 6 clusters seems roughly optimal

# Investigate information in the clusters
# COMPARE distribution of racial/ethnic groups in each cluster to that of all respondents
# Get distribution in clusters
pulse_grouped <- pulse_clustered_data %>% 
  # For each cluster, count number of respondents in each racial/ethnic group
  group_by(clusters) %>% 
  count(race_ethnicity) %>% 
  # Get proportion of each racial/ethnic group in each cluster
  mutate(total = sum(n),
         prop = n/total) %>% 
  # Drop count and total columns
  select(-c(n, total)) %>% 
  # Update names in "clusters" column for use in plots
  mutate(clusters = case_when(clusters == 1 ~ "Cluster 1",
                              clusters == 2 ~ "Cluster 2",
                              clusters == 3 ~ "Cluster 3",
                              clusters == 4 ~ "Cluster 4",
                              clusters == 5 ~ "Cluster 5"))

# Get distribution of racial/ethnic groups for all respondents
racial_ethnic_totals <- pulse_college_data %>% 
  group_by(race_ethnicity) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n),
         prop = n/total,
         clusters = "All respondents") %>% 
  select(clusters, race_ethnicity, prop) %>% 
  # Combine with distribution within clusters
  rbind(pulse_grouped)

# Create stacked bar charts to compare breakdown of racial/ethnic groups across clusters
clusters_breakdown_r_e <- ggplot(data = racial_ethnic_totals, 
                             mapping = aes(x = clusters,
                                           y = prop,
                                           fill = race_ethnicity)) +
  geom_col() +
  coord_flip() +
  labs(x = "Group",
       y = "Proportion of group",
       title = "Distribution of Racial/Ethnic Groups for All 
       Respondents and Kmeans Clusters ") +
  scale_fill_manual(name = "Racial/Ethnic Group", 
                     values = c("Hispanic or Latino" = "#66C2A5", 
                                "Non-Hispanic Asian" = "#FC8D62", 
                                "Non-Hispanic Black" = "#8DA0CB", 
                                "Non-Hispanic White" = "#E78AC3",
                                "Non-Hispanic Other/Multiple Races" = "#A6D854"))
# Save as .png
png(filename="images_and_plots/clusters_breakdown_r_e.png", width = 600, height = 500)
plot(clusters_breakdown_r_e)
dev.off()

# COMPARE access of healthcare across clusters with animated bar chart
# Find percent of individuals in each cluster taking prescription
pres_sum <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(prescription) %>% 
  # Use same column names so dfs can be stacked into one df
  # Create column to track the variable "type" once stacked into one df
  mutate(total = sum(n),
         prop_type = n/total,
         type = "presc") %>% 
  rename(value = prescription) %>% 
  select(-c(n, total))

# Receiving counseling or similar 
mhs_sum <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(mental_health_services) %>% 
  mutate(total = sum(n),
         prop_type = n/total,
         type = "mhs") %>% 
  rename(value = mental_health_services) %>% 
  select(-c(n, total))

# Needing access but not receiving
no_a_sum <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(no_access) %>% 
  mutate(total = sum(n),
         prop_type = n/total,
         type = "no_a") %>% 
  rename(value = no_access) %>% 
  select(-c(n, total)) %>% 
  # Align with the other variables, where `1` indicates access to healthcare of some sort
  # Flip `1` and `2` so `1` indicates needing and receiving / not needing, and `2` indicates needing but not receiving access
  mutate(value = recode(value, `1` = 2, `2` = 1))

# Having some type of healthcare coverage
healthcare_sum <- pulse_clustered_data %>% 
  group_by(clusters) %>% 
  count(healthcare) %>% 
  mutate(total = sum(n),
         prop_type = n/total,
         type = "healthcare") %>% 
  rename(value = healthcare) %>% 
  select(-c(n, total))

# Combine above dataframes
clusters_characteristics <- rbind(pres_sum, mhs_sum, no_a_sum, healthcare_sum) %>%
  # Rename values in `type` column so they can easily be called and displayed with `{closest_state}` during animation
  mutate(type = case_when(type == "presc" ~ "Takes Prescription Medication",
                          type =="mhs" ~ "Receives counseling or similar mental health services",
                          type == "no_a" ~ "Able to access to mental health care when needed",
                          type == "healthcare" ~ "Has some form of healthcare coverage"))

#`gganimate` makes plotting stacked bar chart as a single `geom_col()` difficult
# The following dataframe allows us to plot the stacked bar chart as two layers
# The "bottom layer" is always 1, i.e., fills up the whole y-axis
# The top layer then moves across this, so that the proportions of y/n are maintained but show more smoothly in the animation
clusters_bottom_layer <- clusters_characteristics %>% 
  group_by(clusters, type) %>% 
  # Get column of ones just by adding the proportions of "yes" or "no"
  mutate(total = sum(prop_type)) %>% 
  select(clusters, type, total) %>% 
  distinct()

# Create animated bar charts to smoothly move between the 4 variables
# Smoothest way to do so is build up two layers
animate_hc <- ggplot() +
  # First layer always fills entire length of the bar chart (0-1)
  # Since all proportions add to 1 in total, this layer represents "no" responses once overlaid by the second layer
  geom_col(data = clusters_bottom_layer,
           mapping = aes(x = clusters,
                         y = total, 
                         fill = "No")) +
  # Filtering for `value == 1` gives "yes" answers
  geom_col(data = clusters_characteristics %>% filter(value == 1),
           mapping = aes(x = clusters,
                         y = prop_type,
                         fill = "Yes")) +
  # Create legend and colors
  scale_fill_manual(name = "Respondent's answer",
                    labels = c("Yes", "No"),
                    values = c("Yes" = "thistle", "No" = "palegreen3")) +
  # Transition between the four variables with `type`, spend more time on each state than during transition
  transition_states(type, transition_length = 1, state_length = 6) +
  # Have new points drift in and travel the full distance they represent
  enter_drift(x_mod = 0, y_mod = clusters_characteristics$prop_type) +
  # Have old points shrink out of the animation
  exit_shrink() +
  # Flip horizontally
  coord_flip() +
  # Get title to change as states change
  labs(title = "{closest_state}",
       x = "Cluster",
       y = "Proportion of respondents")
# Still unfortunately has problem filling "Yes" for Cluster 5 "receiving counseling" variable

# Save animation
animate(animate_hc, renderer=gifski_renderer("images_and_plots/clusters_breakdown.gif"))

################################
# Racial/Ethnic Groups Network #
################################

# Select mental health care, healthcare access, and race-ethnicity variables 
r_e_network <- pulse_college_data %>% 
  select(race_ethnicity, prescription, mental_health_services, no_access, healthcare, ANXIOUS, DOWN) 

# Use mental health / healthcare access variables to show similarities between racial/ethnic groups
# Quantify similarities to use as edges in network

# Anxiety
anxiety_net <- r_e_network %>% 
  # For each racial/ethnic group, count individuals in each level of the variable
  group_by(race_ethnicity, ANXIOUS) %>% 
  count(ANXIOUS) %>% 
  ungroup() %>% 
  group_by(race_ethnicity) %>% 
  # Calculate percent of individuals experiencing no, some, etc. anxiety
  mutate(total = sum(n),
         prop_a = n/total,
         type = "anx") %>% 
  # Keep only individuals experiencing chronic anxiety
  filter(ANXIOUS == c(3, 4)) %>% 
  mutate(prop = sum(prop_a)) %>% 
  # Keep needed columns 
  select(race_ethnicity, prop, type) %>% 
  # Remove repeated rows
  distinct()

# Depression
depression_net <- r_e_network %>% 
  group_by(race_ethnicity, DOWN) %>% 
  count(DOWN) %>% 
  ungroup() %>% 
  group_by(race_ethnicity) %>% 
  mutate(total = sum(n),
         prop_d = n/total,
         type = "dep") %>% 
  filter(DOWN == c(3, 4)) %>% 
  mutate(prop = sum(prop_d)) %>% 
  select(race_ethnicity, prop, type) %>% 
  distinct()

# Prescription medication
presc_net <- r_e_network %>% 
  group_by(race_ethnicity, prescription) %>% 
  count(prescription) %>% 
  ungroup() %>% 
  group_by(race_ethnicity) %>% 
  mutate(total = sum(n),
         prop = n/total,
         type = "presc") %>% 
  # Keep only individuals taking prescription medications
  filter(prescription == 1) %>% 
  select(race_ethnicity, prop, type) %>% 
  distinct()

# Mental health counseling or similar service
mhs_net <- r_e_network %>% 
  group_by(race_ethnicity, mental_health_services) %>% 
  count(mental_health_services) %>% 
  ungroup() %>% 
  group_by(race_ethnicity) %>% 
  mutate(total = sum(n),
         prop = n/total,
         type = "mhs") %>% 
  filter(mental_health_services == 1) %>% 
  select(race_ethnicity, prop, type) %>% 
  distinct()

# Needed access to mental health services but did not receive them
no_a_net <- r_e_network %>% 
  group_by(race_ethnicity, no_access) %>% 
  count(no_access) %>% 
  ungroup() %>% 
  group_by(race_ethnicity) %>% 
  mutate(total = sum(n),
         prop = n/total,
         type = "no_a") %>% 
  filter(no_access == 1) %>% 
  select(race_ethnicity, prop, type) %>% 
  distinct()

# Healthcare coverage
hc_net <- r_e_network %>% 
  group_by(race_ethnicity, healthcare) %>% 
  count(healthcare) %>% 
  ungroup() %>% 
  group_by(race_ethnicity) %>% 
  mutate(total = sum(n),
         prop = n/total,
         type = "hc") %>% 
  filter(healthcare == 1) %>% 
  select(race_ethnicity, prop, type) %>% 
  distinct()

# Combine above data-frames for network 
r_e_net <- rbind(anxiety_net, depression_net, presc_net, mhs_net, no_a_net, hc_net) %>% 
  # Pivot wider to get column for each racial/ethnic group
  pivot_wider(names_from = race_ethnicity,
              values_from = prop)

# Create a matrix with all racial/ethnic combinations
race_ethnicity_Mat <- t(combn(names(r_e_net[,-1]), 2))

# Pre-allocate space for a count vector
race_ethnicity_close <- integer(nrow(race_ethnicity_Mat))

# Loop through race_ethnicity combos 
for(i in 1:nrow(race_ethnicity_Mat)) {
  r_e_1 <- r_e_net[, race_ethnicity_Mat[i,]]
  # Get difference in percent of individuals for each combination of racial/ethnic groups, for each variable, and put result in count vector
  race_ethnicity_close[i] <- abs(r_e_1[, 1] - r_e_1[, 2])
}

# Convert list to dataframe
r_e_network_final <- rbind.data.frame(race_ethnicity_close) 
# Transpose so group combinations are the rows
r_e_network_final <- t(r_e_network_final)
# Get rid of row names (artifact of `rbind.data.frame()`)
rownames(r_e_network_final) <- NULL
# Reassign to data frame 
r_e_network_final <- as.data.frame(r_e_network_final) %>% 
  # Add group combinations 
  mutate("race_ethnicity_one" = race_ethnicity_Mat[,1], 
         "race_ethnicity_two" = race_ethnicity_Mat[,2]) %>% 
  # Manually assign corresponding column names
  rename("anx" = V1,
         "dep" = V2,
         "presc" = V3,
         "mhs" = V4,
         "no_a" = V5,
         "hc" = V6) %>%
  # Subtract all values from 1, so the network will give more edge weight to smaller differences 
  mutate(anx = 1-anx,
         dep = 1-dep,
         presc = 1-presc,
         mhs = 1-mhs,
         no_a = 1-no_a,
         hc = 1-hc)

# Save as .csv
write.csv(r_e_network_final, file = "shiny_racial-ethnic-network/r-e-network_final.csv")

# Create legends for network
# Set color palette
eigScalePal <- colorRampPalette(c("blue", "red"), bias = 5)
num_colors <- 5

# Create horizontal plots for color legends for each variable
# Anxiety
png(filename = "shiny_racial-ethnic-network/legends/legend_anx.png", width = 400, height = 400)
anx_image <- as.raster(matrix(eigScalePal(5), nrow = 1))
plot(c(0,1),c(0,4),type = 'n', axes = F,xlab = '', ylab = '')
# Manually set xlab to move it closer to color scale
title(xlab = "Proportion of respondents", line=0, cex.lab=1.2)
# Set the limits on the color scale to the min and max proportion
text(y=1.5, x = seq(0,1,l=5), labels = seq(round(min(anxiety_net$prop), digits = 2), round(max(anxiety_net$prop), digits = 2),l=5))
anx_image <- rasterImage(anx_image, 0, 0, 1, 1)
dev.off()

# Repeat for other variables
# Depression
png(filename = "shiny_racial-ethnic-network/legends/legend_dep.png", width = 400, height = 400)
dep_image <- as.raster(matrix(eigScalePal(5), nrow = 1))
plot(c(0,1),c(0,4),type = 'n', axes = F,xlab = '', ylab = '')
title(xlab = "Proportion of respondents", line=0, cex.lab=1.2)
text(y = 1.5, x = seq(0,1,l=5), labels = seq(round(min(depression_net$prop), digits = 2), round(max(depression_net$prop), digits = 2), l=5))
dep_image <- rasterImage(dep_image, 0, 0, 1, 1)
dev.off()

# Prescription
png(filename = "shiny_racial-ethnic-network/legends/legend_presc.png", width = 400, height = 400)
presc_image <- as.raster(matrix(eigScalePal(5), nrow = 1))
plot(c(0,1),c(0,4),type = 'n', axes = F,xlab = '', ylab = '')
title(xlab = "Proportion of respondents", line=0, cex.lab=1.2)
text(y = 1.5, x = seq(0,1,l=5), labels = seq(round(min(presc_net$prop), digits = 2), round(max(presc_net$prop), digits = 2),l=5))
presc_image <- rasterImage(presc_image, 0, 0, 1, 1)
dev.off()

# Mental health services
png(filename = "shiny_racial-ethnic-network/legends/legend_mhs.png", width = 400, height = 400)
mhs_image <- as.raster(matrix(eigScalePal(5), nrow = 1))
plot(c(0,1),c(0,4),type = 'n', axes = F,xlab = '', ylab = '')
title(xlab = "Proportion of respondents", line=0, cex.lab=1.2)
text(y = 1.5, x = seq(0,1,l=5), labels = seq(round(min(mhs_net$prop), digits = 2), round(max(mhs_net$prop), digits = 2),l=5))
mhs_image <- rasterImage(mhs_image, 0, 0, 1, 1)
dev.off()

# No access
png(filename = "shiny_racial-ethnic-network/legends/legend_no_a.png", width = 400, height = 400)
no_a_image <- as.raster(matrix(eigScalePal(5), nrow = 1))
plot(c(0,1),c(0,4),type = 'n', axes = F,xlab = '', ylab = '')
title(xlab = "Proportion of respondents", line=0, cex.lab=1.2)
text(y = 1.5, x = seq(0,1,l=5), labels = seq(round(min(no_a_net$prop), digits = 2), round(max(no_a_net$prop), digits = 2),l=5))
no_a_image <- rasterImage(no_a_image, 0, 0, 1, 1)
dev.off()

# Healthcare
png(filename = "shiny_racial-ethnic-network/legends/legend_hc.png", width = 400, height = 400)
hc_image <- as.raster(matrix(eigScalePal(5), nrow = 1))
plot(c(0,1),c(0,4),type = 'n', axes = F,xlab = '', ylab = '')
title(xlab = "Proportion of respondents", line=0, cex.lab=1.2)
text(y = 1.5, x = seq(0,1,l=5), labels = seq(round(min(hc_net$prop), digits = 2), round(max(hc_net$prop), digits = 2),l=5))
hc_image <- rasterImage(hc_image, 0, 0, 1, 1)
dev.off()

# Get nodes and edges for each health care variable
# Select anxiety
anx_visNetwork <- r_e_network_final %>% 
  select(race_ethnicity_one, race_ethnicity_two, anx) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges
anx_nodes <- anx_visNetwork$nodes %>% 
  # Join data frames to get proportions and assign colors for the legend
  inner_join(anxiety_net, by = c("id" = "race_ethnicity")) %>%
  mutate(color = eigScalePal(num_colors)[cut(prop, breaks = num_colors)]) %>%
  select(-c(prop))
anx_edges <- anx_visNetwork$edges %>% 
  mutate(value = anx) 

# Save as .csv
write.csv(anx_nodes, file = "shiny_racial-ethnic-network/anxiety_nodes.csv")
write.csv(anx_edges, file = "shiny_racial-ethnic-network/anxiety_edges.csv")

# Select depression
dep_visNetwork <- r_e_network_final %>% 
  select(race_ethnicity_one, race_ethnicity_two, dep) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges 
dep_nodes <- dep_visNetwork$nodes %>% 
  inner_join(depression_net, by = c("id" = "race_ethnicity")) %>%
  mutate(color = eigScalePal(num_colors)[cut(prop, breaks = num_colors)]) %>%
  select(-c(prop))
dep_edges <- dep_visNetwork$edges %>% 
  mutate(value  = dep)

# Save as .csv
write.csv(dep_nodes, file = "shiny_racial-ethnic-network/depression_nodes.csv")
write.csv(dep_edges, file = "shiny_racial-ethnic-network/depression_edges.csv")

# Select prescription
presc_visNetwork <- r_e_network_final %>% 
  select(race_ethnicity_one, race_ethnicity_two, presc) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges 
presc_nodes <- presc_visNetwork$nodes %>% 
  inner_join(presc_net, by = c("id" = "race_ethnicity")) %>%
  mutate(color = eigScalePal(num_colors)[cut(prop, breaks = num_colors)]) %>%
  select(-c(prop))
presc_edges <- presc_visNetwork$edges %>% 
  mutate(value = presc)

# Save as .csv
write.csv(presc_nodes, file = "shiny_racial-ethnic-network/prescription_nodes.csv")
write.csv(presc_edges, file = "shiny_racial-ethnic-network/prescription_edges.csv")

# Select mental health services
mhs_visNetwork <- r_e_network_final %>% 
  select(race_ethnicity_one, race_ethnicity_two, mhs) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges
mhs_nodes <- mhs_visNetwork$nodes %>% 
  inner_join(mhs_net, by = c("id" = "race_ethnicity")) %>%
  mutate(color = eigScalePal(num_colors)[cut(prop, breaks = num_colors)]) %>%
  select(-c(prop))
mhs_edges <- mhs_visNetwork$edges %>% 
  mutate(value = mhs)

# Save as .csv
write.csv(mhs_nodes, file = "shiny_racial-ethnic-network/mental_health_services_nodes.csv")
write.csv(mhs_edges, file = "shiny_racial-ethnic-network/mental_health_services_edges.csv")

# Select needed access but did not receive
no_a_visNetwork <- r_e_network_final %>% 
  select(race_ethnicity_one, race_ethnicity_two, no_a) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges 
no_a_nodes <- no_a_visNetwork$nodes %>% 
  inner_join(no_a_net, by = c("id" = "race_ethnicity")) %>%
  mutate(color = eigScalePal(num_colors)[cut(prop, breaks = num_colors)]) %>%
  select(-c(prop))
no_a_edges <- no_a_visNetwork$edges %>% 
  mutate(value = no_a)

# Save as .csv
write.csv(no_a_nodes, file = "shiny_racial-ethnic-network/no_access_nodes.csv")
write.csv(no_a_edges, file = "shiny_racial-ethnic-network/no_access_edges.csv")

# Select healthcare
hc_visNetwork <- r_e_network_final %>% 
  select(race_ethnicity_one, race_ethnicity_two, hc) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges
hc_nodes <- hc_visNetwork$nodes %>% 
  inner_join(hc_net, by = c("id" = "race_ethnicity")) %>%
  mutate(color = eigScalePal(num_colors)[cut(prop, breaks = num_colors)]) %>%
  select(-c(prop))
hc_edges <- hc_visNetwork$edges %>% 
  mutate(value = hc)

# Save as .csv
write.csv(hc_nodes, file = "shiny_racial-ethnic-network/healthcare_nodes.csv")
write.csv(hc_edges, file = "shiny_racial-ethnic-network/healthcare_edges.csv")

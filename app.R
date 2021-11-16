# Load packages
library(tidyverse)
library(dplyr)
library(igraph)
library(visNetwork)
library(grDevices)
library(shiny)

# Load network datasets
restaurants_final <- read.csv("wrangled_csv_data/restaurants_network_data.csv") %>% select(-X)
bars_final <- read.csv("wrangled_csv_data/bars_network_data.csv") %>% select(-X)
mask_mandates_final <- read.csv("wrangled_csv_data/mask_mandates_network_data.csv") %>% select(-X)
gathering_bans_final <- read.csv("wrangled_csv_data/gathering_bans_network_data.csv") %>% select(-X)
stay_at_home_orders_final <- read.csv("wrangled_csv_data/stay_at_home_orders_network_data.csv") %>% select(-X)

# Load proportion of annual COVID cases per state population data
annual_cases <- read.csv("wrangled_csv_data/proportion_annual_cases_data.csv")

# Set color palette
eigScalePal <- colorRampPalette(c("blue", "red"), bias = 5)
num_colors <- 10

# Create plot for color legend
png(file = "covid_legend.png")
legend_image <- as.raster(matrix(eigScalePal(num_colors), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Proportion of Annual COVID Cases')
text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
legend_image <- rasterImage(legend_image, 0, 0, 1,1)
dev.off()

# Restaurant Restrictions
restaurants_igraph <- graph_from_data_frame(restaurants_final, directed = FALSE)

restaurants_visNetwork <- toVisNetworkData(restaurants_igraph)

restaurants_edges <- restaurants_visNetwork$edges %>%
  mutate(value = as.numeric(restaurants_overLap),
         width = value/100) %>%
  select(-value)

restaurants_nodes <- restaurants_visNetwork$nodes %>%
  inner_join(annual_cases, by = c("id" = "state")) %>%
  mutate(color = eigScalePal(num_colors)[cut(covid_prop, breaks = num_colors)]) %>%
  select(-c(X, covid_prop))

# Bar Restrictions
bars_igraph <- graph_from_data_frame(bars_final, directed = FALSE)

bars_visNetwork <- toVisNetworkData(bars_igraph)

bars_edges <- bars_visNetwork$edges %>%
  mutate(value = as.numeric(bars_overLap),
         width = value/100) %>%
  select(-value)

bars_nodes <- bars_visNetwork$nodes %>%
  inner_join(annual_cases, by = c("id" = "state")) %>%
  mutate(color = eigScalePal(num_colors)[cut(covid_prop, breaks = num_colors)]) %>%
  select(-c(X, covid_prop))

# Mask Mandates
mask_mandates_igraph <- graph_from_data_frame(mask_mandates_final, directed = FALSE)

mask_mandates_visNetwork <- toVisNetworkData(mask_mandates_igraph)

mask_mandates_edges <- mask_mandates_visNetwork$edges %>%
  mutate(value = as.numeric(mask_mandates_overLap),
         width = value/100) %>%
  select(-value)

mask_mandates_nodes <- mask_mandates_visNetwork$nodes %>%
  inner_join(annual_cases, by = c("id" = "state")) %>%
  mutate(color = eigScalePal(5)[cut(covid_prop, breaks = 5)]) %>%
  select(-c(X, covid_prop))

# create igraph object
# states <- c("AK", "CT", "SD", "AR", "LA")
# 
# gathering_bans_final <- gathering_bans_final %>%
#   filter(state_one %in% states,
#          state_two %in% states) 

# Gathering Bans
gathering_bans_igraph <- graph_from_data_frame(gathering_bans_final, directed = FALSE)

gathering_bans_visNetwork <- toVisNetworkData(gathering_bans_igraph)

gathering_ban_edges <- gathering_bans_visNetwork$edges %>%
  mutate(value = as.numeric(gathering_bans_overLap),
         width = value/100) %>%
  select(-value)

gathering_ban_nodes <- gathering_bans_visNetwork$nodes %>%
  inner_join(annual_cases, by = c("id" = "state")) %>%
  mutate(color = eigScalePal(num_colors)[cut(covid_prop, breaks = num_colors)]) %>%
  select(-c(X, covid_prop))

# Stay at home orders 
stay_at_home_orders_igraph <- graph_from_data_frame(stay_at_home_orders_final, directed = FALSE)

stay_at_home_orders_visNetwork <- toVisNetworkData(stay_at_home_orders_igraph)

stay_at_home_orders_edges <- stay_at_home_orders_visNetwork$edges %>%
  mutate(value = as.numeric(stay_at_home_orders_overLap),
         width = value/100) %>%
  select(-value)

stay_at_home_orders_nodes <- stay_at_home_orders_visNetwork$nodes %>%
  inner_join(annual_cases, by = c("id" = "state")) %>%
  mutate(color = eigScalePal(num_colors)[cut(covid_prop, breaks = num_colors)]) %>%
  select(-c(X, covid_prop))

############
#    ui    #
############
ui <- fluidPage(title = "State COVID Restrictions Network",
                fillPage(sidebarLayout(
                  sidebarPanel(
                    selectizeInput(
                      inputId = "filterNodes",
                      label = "Select states:",
                      choices = gathering_ban_nodes$id,
                      selected = "AK",
                      multiple = TRUE
                    ),
                    selectInput(
                      inputId = "restriction",
                      label = "Select a type of COVID restriction",
                      choices =  list(
                        "Resturant Restrictions" = "rest",
                        "Bar Restrictions" = "bar",
                        "Mask Mandates" = "mask",
                        "Gathering Bans" = "gatban",
                        "Stay at Home Orders" = "stayhome"),
                      selected = "gatban",
                      multiple = FALSE
                    ),
                    img(src="covid_legend.png", align = "left"),
                    width = 3,
                  ),
                  mainPanel(
                    visNetworkOutput("network_proxy_update", width = "100%", height = "90vh"),
                    width = 9
                  )
                )))

############
# server   #
############

server <- function(input, output) {
  active_nodes <- reactive({
    switch(input$restriction,
           "rest" = subset(restaurants_nodes, restaurants_nodes$id %in% input$filterNodes), 
           "bar" = subset(bars_nodes, bars_nodes$id %in% input$filterNodes),
           "mask" = subset(mask_mandates_nodes, mask_mandates_nodes$id %in% input$filterNodes),
           "gatban" = subset(gathering_ban_nodes, gathering_ban_nodes$id %in% input$filterNodes),
           "stayhome" = subset(stay_at_home_orders_nodes, stay_at_home_orders_nodes$id %in% input$filterNodes)
    )
  })
  
  active_edges <- reactive({
    switch(input$restriction,
           "rest" = subset(restaurants_edges, restaurants_edges$from %in% input$filterNodes),
           "bar" = subset(bars_edges, bars_edges$from %in% input$filterNodes),
           "mask" = subset(mask_mandates_edges, mask_mandates_edges$from %in% input$filterNodes),
           "gatban" = subset(gathering_ban_edges, gathering_ban_edges$from %in% input$filterNodes),
           "stayhome" = subset(stay_at_home_orders_edges, stay_at_home_orders_edges$from %in% input$filterNodes)
    )
  })
  
  output$network_proxy_update <- renderVisNetwork({
    visNetwork(active_nodes(), active_edges(), height = "700px", width = "100%", 
               main = list(text = "Network for States Sharing the Same COVID Restrictions in 2020", 
                           style = "font-family:Arial;font-size:20px"
               ),
               submain = list(text = 
                                "*Nodes are colored by proportion of annual COVID cases per state population<br>
                                 *Edges are weighted by number of days in 2020 that states shared 
                                 the same restrictions",
                              style = "font-family:Arial;font-size:13px")) %>%
      visNodes(size = 10) %>%
      visOptions(selectedBy = "group", 
                 highlightNearest = list(enabled = TRUE, hover = TRUE), 
                 nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(color = list(color = "black", highlight = "red")) 
  })
  
  myVisNetworkProxy <- visNetworkProxy("network_proxy_update")
  
  observe ({
    filteredNodes <- active_nodes()[gathering_ban_nodes$id %in% input$filterNodes, ,drop = FALSE]
    hiddenNodes <- anti_join(active_nodes(), filteredNodes)
    visRemoveNodes(myVisNetworkProxy, id = hiddenNodes$id)
    visUpdateNodes(myVisNetworkProxy, nodes = filteredNodes)
  })
}

#####################
# call to shiny App #
#####################
shinyApp(ui = ui, server = server)
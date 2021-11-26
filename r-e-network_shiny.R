# Load packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
library(plotly)
library(igraph)
library(visNetwork)
library(ggnetwork)
library(grDevices)
library(shiny)

# Read in data
r_e_network <- read.csv("wrangled_csv_data/r-e-network.csv")

# Get nodes and edges for each health care variable
# Select anxiety
anx_visNetwork <- r_e_network %>% 
  select(race_ethnicity_one, race_ethnicity_two, anx) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges 
anx_nodes <- anx_visNetwork$nodes 
anx_edges <- anx_visNetwork$edges %>% 
  mutate(value = anx) 

# Select depression
dep_visNetwork <- r_e_network %>% 
  select(race_ethnicity_one, race_ethnicity_two, dep) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges 
dep_nodes <- dep_visNetwork$nodes 
dep_edges <- dep_visNetwork$edges %>% 
  mutate(value  = dep)

# Select prescription
presc_visNetwork <- r_e_network %>% 
  select(race_ethnicity_one, race_ethnicity_two, presc) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges 
presc_nodes <- presc_visNetwork$nodes 
presc_edges <- presc_visNetwork$edges %>% 
  mutate(value = presc)

# Select mental health services
mhs_visNetwork <- r_e_network %>% 
  select(race_ethnicity_one, race_ethnicity_two, mhs) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges 
mhs_nodes <- mhs_visNetwork$nodes 
mhs_edges <- mhs_visNetwork$edges %>% 
  mutate(value = mhs)

# Select needed access but did not receive
no_a_visNetwork <- r_e_network %>% 
  select(race_ethnicity_one, race_ethnicity_two, no_a) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges 
no_a_nodes <- no_a_visNetwork$nodes 
no_a_edges <- no_a_visNetwork$edges %>% 
  mutate(value = no_a)

# Select healthcare
hc_visNetwork <- r_e_network %>% 
  select(race_ethnicity_one, race_ethnicity_two, hc) %>% 
  # Create igraph object
  graph_from_data_frame(directed = FALSE) %>% 
  # Create visNetwork object
  toVisNetworkData()

# Get nodes and weighted edges 
hc_nodes <- hc_visNetwork$nodes 
hc_edges <- hc_visNetwork$edges %>% 
  mutate(value = hc)

############
#    ui    #
############
ui <- fluidPage(title = "Racial/Ethnic Groups Network",
                fillPage(sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId = "hc_variable",
                      label = "Select a type of hc variable",
                      choices =  list(
                        "Anxiety" = "anx",
                        "Depression" = "dep",
                        "Prescription Medication" = "presc",
                        "Counseling or similar services" = "mhs",
                        "Needed but did not receive care" = "no_a",
                        "Healthcare coverage" = "hc"),
                      selected = "anx",
                      multiple = FALSE
                    ),
                    # img(src="covid_legend.png", align = "left"),
                    # width = 3,
                  ),
                  mainPanel(
                    visNetworkOutput("network_proxy_update_re", width = "100%", height = "90vh"),
                    width = 9
                  )
                )))

############
# server   #
############
server <- function(input, output) {
  active_nodes <- reactive({
    switch(input$hc_variable,
           "anx" = anx_nodes,
           "dep" = dep_nodes,
           "presc" = presc_nodes,
           "mhs" = mhs_nodes,
           "no_a" = no_a_nodes,
           "hc" = hc_nodes)
  })
  
  active_edges <- reactive({
    switch(input$hc_variable,
           "anx" = anx_edges,
           "dep" = dep_edges,
           "presc" = presc_edges,
           "mhs" = mhs_edges,
           "no_a" = no_a_edges,
           "hc" = hc_edges
    )
  })
  output$network_proxy_update_re <- renderVisNetwork({
    visNetwork(active_nodes(), active_edges(), height = "700px", width = "100%",
               scaling = list(min = min(input$hc_variable), max = max(input$hc_variable)),
               main = list(text = "Network for Racial/Ethnic Groups", 
                           style = "font-family:Arial;font-size:20px"
               ),
               submain = list(text = 
                                "*Edges are weighted by how close groups are",
                              style = "font-family:Arial;font-size:13px")) %>%
      visNodes(size = 10) %>%
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), 
                 nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(color = list(color = "black", highlight = "red")) 
  })
  
  myreVisNetworkProxy <- visNetworkProxy("network_proxy_update_re")

}

#####################
# call to shiny App #
#####################
shinyApp(ui = ui, server = server)
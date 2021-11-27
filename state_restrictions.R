# Load packages
library(tidyverse)
library(dplyr)
library(igraph)
library(visNetwork)
library(grDevices)
library(shiny)
library(shinythemes)

#######################
#    Load Datasets    #
#######################
# Load network datasets
restaurants_edges <- read.csv("wrangled_csv_data/restaurants_edges_data.csv") %>% select(-X)
restaurants_nodes <- read.csv("wrangled_csv_data/restaurants_nodes_data.csv") %>% select(-X)

bars_edges <- read.csv("wrangled_csv_data/bars_edges_data.csv") %>% select(-X)
bars_nodes <- read.csv("wrangled_csv_data/bars_nodes_data.csv") %>% select(-X)

mask_mandates_edges <- read.csv("wrangled_csv_data/mask_mandates_edges_data.csv") %>% select(-X)
mask_mandates_nodes <- read.csv("wrangled_csv_data/mask_mandates_nodes_data.csv") %>% select(-X)

gathering_ban_edges <- read.csv("wrangled_csv_data/gathering_ban_edges_data.csv") %>% select(-X)
gathering_ban_nodes <- read.csv("wrangled_csv_data/gathering_ban_nodes_data.csv") %>% select(-X)

stay_at_home_orders_edges <- read.csv("wrangled_csv_data/stay_at_home_orders_edges_data.csv") %>% select(-X)
stay_at_home_orders_nodes <- read.csv("wrangled_csv_data/stay_at_home_orders_nodes_data.csv") %>% select(-X)

# Load proportion of annual COVID cases per state population data
annual_cases <- read.csv("wrangled_csv_data/proportion_annual_covid_cases_data.csv")

##############################################
# Define choice values and names for widgets #
##############################################
# For nodes
node_values <- state.abb
node_names <- state.name
names(node_values) <- node_names

# For datasets
dataset_values <- c("rest", "bar", "mask", "gatban", "stayhome")
dataset_names <- c("Restaurant Restrictions", "Bar Restrictions", "Mask Mandates", "Gathering Bans", "Stay at Home Orders")
names(dataset_values) <- dataset_names

############
#    ui    #
############
ui <- fluidPage(
  theme = shinytheme("lumen"),
  title = "State COVID Restrictions Network",
                fillPage(sidebarLayout(
                  sidebarPanel(
                    selectizeInput(
                      inputId = "filterNodes",
                      label = HTML("Select states <br />
                      (To remove a state, select it and hit the delete button on your keyboard)"),
                      choices = node_values,
                      selected = "AK",
                      multiple = TRUE
                    ),
                    selectInput(
                      inputId = "restriction",
                      label = "Select a type of COVID restriction",
                      choices =  dataset_values,
                      selected = "gatban",
                      multiple = FALSE
                    ),
                    br(),
                    p("Proportion of Annual COVID Cases per State Population"),
                    img(src="covid_legend.png", width = "500px", height = "500px"),
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
    # Switch nodes dataset depending on user input
    switch(input$restriction,
           "rest" = subset(restaurants_nodes, restaurants_nodes$id %in% input$filterNodes), 
           "bar" = subset(bars_nodes, bars_nodes$id %in% input$filterNodes),
           "mask" = subset(mask_mandates_nodes, mask_mandates_nodes$id %in% input$filterNodes),
           "gatban" = subset(gathering_ban_nodes, gathering_ban_nodes$id %in% input$filterNodes),
           "stayhome" = subset(stay_at_home_orders_nodes, stay_at_home_orders_nodes$id %in% input$filterNodes)
    )
  })
  
  active_edges <- reactive({
    # Switch edges dataset depending on user input
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
               main = list(text = paste("Network for States Sharing the Same COVID", dataset_names[dataset_values == input$restriction], "in 2020"), 
                           style = "font-family:Arial;font-size:20px"),
               submain = list(text = "*Nodes are colored by proportion of annual COVID cases per state population<br>
                                 *Edges are weighted by number of days in 2020 that states shared 
                                 the same restriction order<br>
                                 Hover over edges to see the exact number of days",
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
    # Create a data frame to contain the nodes the user selects
    filteredNodes <- active_nodes()[gathering_ban_nodes$id %in% input$filterNodes, ,drop = FALSE]
    # Create a data frame to contain all the nodes the user did not select
    hiddenNodes <- anti_join(active_nodes(), filteredNodes)
    # 
    visRemoveNodes(myVisNetworkProxy, id = hiddenNodes$id)
    visUpdateNodes(myVisNetworkProxy, nodes = filteredNodes)
  })
}

#####################
# call to shiny App #
#####################
shinyApp(ui = ui, server = server)

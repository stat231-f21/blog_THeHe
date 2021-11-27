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
library(shinythemes)

#######################
#    Load Datasets    #
#######################
# Load nodes datasets
anx_nodes <- read.csv("wrangled_csv_data/anxiety_nodes.csv") %>% select(-X)
dep_nodes <- read.csv("wrangled_csv_data/depression_nodes.csv") %>% select(-X)
presc_nodes <- read.csv("wrangled_csv_data/prescription_nodes.csv") %>% select(-X)
mhs_nodes <- read.csv("wrangled_csv_data/mental_health_services_nodes.csv") %>% select(-X)
no_a_nodes <- read.csv("wrangled_csv_data/no_access_nodes.csv") %>% select(-X)
hc_nodes <- read.csv("wrangled_csv_data/healthcare_nodes.csv") %>% select(-X)

# MOVE ALL THIS IF I USE IT 
eigScalePal <- colorRampPalette(c("blue", "red"), bias = 5)
num_colors <- 5

# Create plot for color legend
png(file = "legends/anx_legend.png")
anx_image <- as.raster(matrix(eigScalePal(5), ncol=1))
plot(c(0,4),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
text(x=1.25, y = seq(0,1,l=5), labels = seq(min(anxiety_net$prop), max(anxiety_net$prop),l=5))
anx_image <- rasterImage(anx_image, 0, 0, 1,1)
dev.off()

anx_nodes <- anx_nodes %>% 
  inner_join(anxiety_net, by = c("id" = "race_ethnicity")) %>%
  mutate(color = eigScalePal(num_colors)[cut(prop, breaks = num_colors)]) %>%
  select(-c(prop))

# Load edges datasets
anx_edges <- read.csv("wrangled_csv_data/anxiety_edges.csv") %>% select(-X)
dep_edges <- read.csv("wrangled_csv_data/depression_edges.csv") %>% select(-X)
presc_edges <- read.csv("wrangled_csv_data/prescription_edges.csv") %>% select(-X)
mhs_edges <- read.csv("wrangled_csv_data/mental_health_services_edges.csv") %>% select(-X)
no_a_edges <- read.csv("wrangled_csv_data/no_access_edges.csv") %>% select(-X)
hc_edges <- read.csv("wrangled_csv_data/healthcare_edges.csv") %>% select(-X)

############
#    ui    #
############
ui <- fluidPage(
  theme = shinytheme("lumen"),
  title = "Racial/Ethnic Groups Network",
  
                fillPage(sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId = "hc_variable",
                      label = "Select a type of health care variable",
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
                    # TAKE THIS OUT IF I DON'T USE IT
                    # br(),
                    # p("Proportion"),
                    # img(src="anx_legend.png", width = "200px", height = "200px"),
                    # width = 3,
                  ),
                  mainPanel(c(
                    visNetworkOutput("network_proxy_update_re", width = "100%", height = "90vh"),
                    imageOutput("legend"))
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
               main = list(text = "Network for Mental Health Symptoms and Access to Resources for Racial/Ethnic Groups", 
                           style = "font-family:Arial;font-size:20px"
               ),
               submain = list(text = 
                                "*Edges are weighted by how similar groups are in terms of the proportion of people<br>
                                  experiencing mental health symptoms or having access to mental health resources",
                              style = "font-family:Arial;font-size:13px")) %>%
      visNodes(size = 10) %>%
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE), 
                 nodesIdSelection = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visEdges(color = list(color = "black", highlight = "red")) 
  })
  
  myreVisNetworkProxy <- visNetworkProxy("network_proxy_update_re")
  
  output$legend <- renderImage({
    if (input$hc_variable = "anx") {
      img(src = "legends/anx_legend.png") 
    }
  })

}

#####################
# call to shiny App #
#####################
shinyApp(ui = ui, server = server)
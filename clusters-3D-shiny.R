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

# plot_ly(pulse_clustered_data, x = ~jitter(prescription), y = ~jitter(mental_health_services), z = ~jitter(healthcare), type="scatter3d", mode="markers", color = ~clusters)

choice_values <- c("prescription", "mental_health_services", "no_access", "healthcare")
choice_names <- c("Prescription medication", "Mental health counseling or other services", "Needed access to mental health care but did not receive", "Some form of healthcare")
names(choice_values) <- choice_names

############
#    ui    #
############
ui <- fluidPage(title = "Clusters Plot",
                fillPage(sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId = "var1",
                      label = "Select the first variable to plot",
                      choices = choice_values,
                      selected = "prescription",
                      multiple = FALSE
                    ),
                    selectInput(
                      inputId = "var2",
                      label = "Select the second variable to plot",
                      choices = choice_values,
                      selected = "mental_health_services",
                      multiple = FALSE
                    ),
                    selectInput(
                      inputId = "var3",
                      label = "Select the second variable to plot",
                      choices = choice_values,
                      selected = "no_access",
                      multiple = FALSE
                    ),
                    submitButton(text = "Make plot", icon = NULL, width = NULL),
                    # img(src="covid_legend.png", align = "left"),
                    # width = 3,
                  ),
                  mainPanel(plotlyOutput("clusters")
                  )
                )))


############
# server   #
############
server <- function(input, output){
  
  # # Tab 1: interactive lineplot
  # data_lineplot <- reactive({
  #   pulse_clustered_data %>% 
  #     filter(race_ethnicity %in% input$r_e)
  # })

  output$clusters <- renderPlotly({
    plot_ly(pulse_clustered_data, x = ~jitter(get(input$var1)), 
            y = ~jitter(get(input$var2)), 
            z = ~jitter(get(input$var3)), 
            type="scatter3d", mode="markers", color = ~clusters) %>% 
      layout(title = "Title",
             scene = list(xaxis = list(title = paste(choice_names[choice_values == input$var1])), 
                          yaxis = list(title = paste(choice_names[choice_values == input$var2])),
                          zaxis = list(title = paste(choice_names[choice_values == input$var3])))
             )
  })

  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

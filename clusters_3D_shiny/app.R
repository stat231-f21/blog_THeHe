# Load packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
library(plotly)
library(grDevices)
library(shiny)

# Read in data
pulse_clustered_data <- read.csv("../wrangled_csv_data/pulse_clustered_data.csv")
# Convert clusters to factor for proper legend
pulse_clustered_data$clusters <- as.factor(pulse_clustered_data$clusters)


# Set up choices for input variables
# Choose 3 of the 4 vairables used to do the Kmeans Clustering
choice_values <- c("prescription", "mental_health_services", "no_access", "healthcare")
# Names for reactive axes
choice_axes <- c("Prescription", "Counseling or similar", "Needed but did not get", "Some healthcare")
names(choice_values) <- choice_axes
# Overwrite previous names to use for the drop-down
choice_names <- c("Prescription medication", "Mental health counseling or similar service", "Could not access mental health care when needed", "Some form of healthcare")
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
                      label = "Select the third variable to plot",
                      choices = choice_values,
                      selected = "no_access",
                      multiple = FALSE
                    ),
                    submitButton(text = "Make plot", icon = NULL, width = NULL),
                    h5("The values on this plot are jittered to visualize the density of each cluster. Values near 2 indicate an individual responded `no` to the survey question, while values near 1 indicate a response of `yes`."),
                  ),
                  mainPanel(plotlyOutput("clusters")
                  )
                )))


############
# server   #
############
server <- function(input, output){

  output$clusters <- renderPlotly({
    plot_ly(pulse_clustered_data, x = ~jitter(get(input$var1)), 
            y = ~jitter(get(input$var2)), 
            z = ~jitter(get(input$var3)), 
            type="scatter3d", mode="markers", color = ~clusters) %>% 
      layout(title = list(text = "Mental Health and Healthcare Resources Kmeans Clustering"),
             scene = list(xaxis = list(title = paste(choice_axes[choice_values == input$var1])), 
                          yaxis = list(title = paste(choice_axes[choice_values == input$var2])),
                          zaxis = list(title = paste(choice_axes[choice_values == input$var3]))),
             legend = list(title=list(text='<b> Cluster </b>'))
             )
  })

  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

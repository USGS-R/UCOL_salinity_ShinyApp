###Shiny app to visualize iterations###
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(tmap)
library(haven)
library(ggpubr)
library(sas7bdat)

# this loads the information to the SERVER app, NOT the LOCAL ENVIRONMENT

#load("~/DOI/Schwarz, Gregory E - Sparrow_network_analysis/SA_applications/PUMP_network_analysis/Experiment_Results/UCOL_salinity_exp1.Rdata")
load("UCOL_salinity_exp1.Rdata")

ui <- fluidPage(
  
  # Application title
  titlePanel("AE optimization viewer UCOL Salinity Model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout( fluid=TRUE,
    sidebarPanel(width = 2,
      sliderInput("v_value",
                  "iteration to view",
                  min = 1,
                  max = 10,
                  value = 1),
      selectInput(inputId = "select", h3("Select box"), 
                  choices = modelparms, selected = "sprink_mi2")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 6,
      plotOutput("aeOptSiteMap", height = 800),
      plotOutput("aeOptGradientBox", height = 500),
      plotOutput("aeOptBasinCharMap", height = 800),
      plotOutput("aeOptBasinCharBox", height = 500)
    )
  )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  output$aeOptSiteMap <- renderPlot({
    p <- filter(net_mid, iter==input$v_value)
    p1 <- filter(p, IFSELECT==1)
    
    ggplot() +
      geom_sf(data = NetworkSparrowdata, color = "lightblue", lwd=.2) +
      geom_sf(data=p, color = "black", size=.5) +
      geom_sf(data = p1, color="red", size=1.5) +
      ggtitle(paste("SPARROW OptNet", length(unique(p1$WATERID)), "new sites (red) specified",
                    length(unique(p$WATERID)), "preditermined sites (black)"))
  } )
  output$aeOptBasinCharMap <- renderPlot({
    p <- filter(net_mid, iter==input$v_value)
    p1 <- filter(p, IFSELECT==1)

      ggplot() +
      geom_sf(data = NetworkSparrowdata, aes_string(color = input$select)) +
        scale_color_gradientn(colours = c("white", "grey", "yellow", "green", "blue","purple")) +
      geom_sf(data=p, color = "black", size=.5) +
      geom_sf(data = p1, color="red", size=1.5) +
        ggtitle(paste("Site locations and Standardized Watershed Characteristics", length(unique(p1$WATERID)), "new sites (red)   Char=", input$select))
  } )
  
  output$aeOptGradientBox <- renderPlot({
    iterGrad <- filter(datGradient, iter==input$v_value)
    iterGradSel <- filter(iterGrad,IFSELECT==1)

    ggplot() +
      geom_boxplot(data=iterGrad, aes(x=as.factor(name), y=value),
                   fill="grey") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      geom_point(data=iterGradSel, aes(x=as.factor(name), y=value, color=as.factor(WATERID))) +
      ggtitle(paste("Experiment S1", length(unique(iterGradSel$WATERID)), "new sites specified"))
  } )
  output$aeOptBasinCharBox <- renderPlot({
    sparrowDataStdLongSel<- sparrowDataStdLong %>% 
      filter(iter==input$v_value) %>%
      filter(IFSELECT==1)
    ggplot() +
      geom_boxplot(data=sparrowDataStdLong, aes(x=as.factor(name), y=value),
                   fill="grey")  +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      geom_point(data=sparrowDataStdLongSel, aes(x=as.factor(name), y=value, color=as.factor(WATERID))) +
      ggtitle(paste("Standardized Watershed Characteristics with", length(unique(sparrowDataStdLongSel$WATERID)), "new sites (red)"))
  } )
}

# Run the application 
shinyApp(ui = ui, server = server)

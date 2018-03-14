#Git Project

#CROSS SURVEY WATER CHEM QA TOOL
#

library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(fBasics)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(plotly)
library(Cairo);options(shiny.usecairo=TRUE)

#### Datasets

## NCCA Datasets

## NWCA Datasets
nwcaChem <- read.csv("nwca2011_waterchem.csv")
nwcaSiteInfo <- read.csv("nwca2011_siteinfo.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage("NARS Cross Survey QA Tool Prototype",
                           tabPanel("About",
                                    column(10,
                                           h3("About"))),
                           tabPanel("NCCA",
                                    column(12, tabsetPanel(
                                      tabPanel("Conductivity"),
                                      tabPanel("pH"),
                                      tabPanel("Total Nitrogen"),
                                      tabPanel("Total Phosphorus"),
                                      tabPanel("Chlorophyll")
                                    ))),
                           tabPanel("NWCA",
                                    column(12, tabsetPanel(
                                      tabPanel("Conductivity",
                                               column(12, tabsetPanel(
                                                 tabPanel("Box Plots",
                                                          column(12,
                                                                 # Current Issues: 
                                                                 ## Cannot select individual points
                                                                 ## Cannot view quartiles in hover
                                                                 ## Cannot customize hover information
                                                                 ## Should be fixed in next plotly update
                                                                 h3("National Conductivity:"),
                                                                 plotlyOutput("nwcaCondBox"),
                                                                 h3("Conductivity by Ecoregion:"),
                                                                 plotlyOutput("nwcaCondBoxRegion"),
                                                                 h3("Conductivity by Ecoregion and State"),
                                                                 plotlyOutput("nwcaCondBoxGroup"))),
                                                 tabPanel("Bar Plots",
                                                          column(12,
                                                                 h3("Mean Conductivity by Region"),
                                                                 plotlyOutput("nwcaCondBarRegion"),
                                                                 br(),
                                                                 h3("Mean Conductivity by State"),
                                                                 plotlyOutput("nwcaCondBarState"))),
                                                 tabPanel("Histograms",
                                                          
                                                          column(12,
                                                                 h3("Average Conductivity")))
                                               ))),
                                      tabPanel("pH"),
                                      tabPanel("Total Nitrogen"),
                                      tabPanel("Total Phosphorus"),
                                      tabPanel("Chlorophyll"),
                                      tabPanel("Map",
                                               column(4, wellPanel(
                                                 h4("Analyte Selection"),
                                                 h5("Select the Analyte you would like to view"),
                                                 textInput("analyte", "Select Analyte to Map:", "TN"))),
                                               column(8, plotlyOutput("nwcaMap")))
                                    )))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$nwcaCondBox <- renderPlotly({
    p <- plot_ly(nwcaChem, y = ~COND, type = "box") 
    #p <- plot_ly(data=nwcaChem, y = ~COND, type = "box", boxpoints = "all", jitter = 0.3,
    #pointpos = -1.8) 
  })
  
  output$nwcaCondBoxRegion <- renderPlotly({
    p <- plot_ly(nwcaChem, y = ~COND, color = ~REGION, type = "box")
  })
  
  output$nwcaCondBoxGroup <- renderPlotly({
    p <- plot_ly(nwcaChem, x = ~REGION, y = ~COND, color = ~STATE, type = "box") %>%
      layout(boxmode = "group")
  })
  
  output$nwcaCondBarRegion <- renderPlotly({
    meanCondRegion <- aggregate(nwcaChem$COND, list(region = nwcaChem$REGION), mean, na.rm=TRUE)
    p <- plot_ly(x = meanCondRegion$region, y = meanCondRegion$x, type = "bar")
  })
  
  output$nwcaCondBarState <- renderPlotly({
    meanCondState <- aggregate(nwcaChem$COND, list(state = nwcaChem$STATE), mean, na.rm=TRUE)
    p <- plot_ly(x = meanCondState$state, y = meanCondState$x, type = "bar")
  })
  
  
  output$nwcaMap <- renderPlotly({
    nwcaChem$q <- with(df, cut(input$analyte, quantile(input$analyte)))
    levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
    df$q <- as.ordered(df$q)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )
    
    p <- plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250)) %>%
      add_markers(
        x = ~lon, y = ~lat, size = ~input$analyte, color = ~q, hoverinfo = "text",
        text = ~paste(df$name, "<br />", df$pop/1e6, "")
      ) %>%
      layout(title = 'NWCA Analyte Results<br>(Click legend to toggle)', geo = g)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


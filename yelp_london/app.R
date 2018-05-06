#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(datasets)
library(ggmap)
load("data1 copy.RData")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "London Restaurants"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Yelp vs. Tripadvisor", icon = icon("th"), tabName = "vs")),
      menuItem("Yelp Top 50", tabName = "yelp", icon = icon("th"))
  ),
  dashboardBody(
    tabItems(
      #yelp top 50
      tabItem(tabName = "yelp",
              fluidRow(
                h3("Top 50 London Restaurants on Yelp"),
                   box(width = 5,
                       sliderInput("distance", "Distance from city center: (100 meters)",
                                   1,97,c(5,50)),
                       sliderInput("nreview", "Range of review count:",1,1173,step=50,
                                  c(50,500)),
                       checkboxGroupInput("price","Price Level:",
                                   c("$"=1,"$$"=2,"$$$"=3,"$$$$"=4),selected=2),
                       actionButton("update", "Update View")
                       ),
                   box(width = 7,
                       h4("Wordcloud: Rating x Review Count"),
                       plotOutput(outputId = "main_plot"),
                       #verbatimTextOutput("main_plot"),
                       hr(),
                       helpText("Size of restaurant names based on rating x review_count."),
                       helpText("Spaces in restaurant names are removed."))),
              fluidRow(
                   box(width = 6,
                       h5("Map of Top 50"),
                       plotOutput(outputId = "map1"),
                       hr(),
                       helpText("If map display is unsuccessful, please refresh and try again.")
                       ),
                  box(width = 6,
                      h5("View the Dataset"),
                      style = 'overflow-x: scroll',
                      DT::dataTableOutput("mytable1"))
              )
      ),
      
      #comparison
      tabItem(tabName = "vs",
              fluidRow(
                box(width= 7,
                    h4("Summary"),
                    verbatimTextOutput("summary"),
                    # Include clarifying text ----
                    helpText("Note: while the data view will show only the specified",
                             "number of observations, the summary will still be based",
                             "on the full dataset of annual mean temperature.")),
                infoBoxOutput(width= 5, "sourcebox")
              ),
              fluidRow(
                box(
                  h4("Distribution"),
                  width = 8,
                  plotOutput("plot1")),
                box(width = 4,
                    h5("Control"),
                    sliderInput("distance2", "Distance from city center: (100 meters)",
                               0,277,step=10,c(1,451)),
                    sliderInput("nreview2", "Range of yelp review count:",1,1173,step=50,
                               c(50,600))
                    
                ))
              
              
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ##################comparison################
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    summary(yelp_tripadv)
  })
  
  output$plot1 <- renderPlot({
    dataset <- yelp_tripadv
    dataset <- dataset %>% filter(distance>=input$distance2[1] & distance<=input$distance2[2])
    dataset <- dataset %>% filter(review_count>=input$nreview2[1] & review_count<=input$nreview2[2])
    
    # rownames(dataset) <- dataset[,1]
    a <- ggplot(dataset,
           aes(x=score,fill = factor(yortrip)))
    a + geom_histogram() +
      facet_wrap(~yortrip, nrow = 1)

  })
  
  output$sourcebox <- renderInfoBox({
    value <- 
      infoBox(
        "Data Source", "Collected by Yelp Fusion API on 04/28/2018",
        icon = icon("list"),
        color = "blue", fill = FALSE
      )
  })
  
  
  ##########################yelp top 50######################
  library(wordcloud)
  wordcloud_rep <- repeatable(wordcloud)
  library(stringr)
  output$main_plot <- renderPlot({
    dataset2 <- datasetInput()
    wordcloud_rep(str_replace_all(dataset2$name, " ", ""), dataset2$rating*dataset2$review_count*10, scale=c(4,0.2),
                  min.freq = 1,max.words=200,rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  datasetInput <- eventReactive(input$update, {
    dataset2 <- payload
    dataset2$distance <- dataset2$distance/100
    dataset2 %>%
      filter(distance>=input$distance[1] & distance<=input$distance[2]) %>% 
      filter(review_count >= input$nreview[1] & review_count<=input$nreview[2]) %>%
      filter(price %in% input$price)
  }, ignoreNULL = FALSE)
  
  output$mytable1 <- DT::renderDataTable(
    DT::datatable(payload[c("name","review_count","price","address1","distance")],
                  options = list(scrollX = TRUE,searching = FALSE,pageLength = 5))
  )
  
  output$map1 <- renderPlot({
    #map
    london <- geocode("london, england")
    map <- get_map(london, zoom=12)    ##modify the map location to adjust the view
    circle_scale_amt <- 0.4
    ggmap(map) +
      geom_point(
        aes(x = longitude, y = latitude, colour = rating),
        data = payload,
        alpha=0.4, size=(payload$rating^2)*circle_scale_amt)+ 
      scale_size_continuous(range=range(payload$rating)) +
      scale_color_distiller(type = "seq", palette = "Purples",direction=1)
  })
  
}

shinyApp(ui, server)
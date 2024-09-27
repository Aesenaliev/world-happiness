#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(colourpicker)
library(tidyverse)
happy <- read_csv('happy.csv')
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("united"),
  h1(strong("Happiness Report"), style = "text-align: center; "),
  br(),
  tags$img(src = "world.jpg", width = "600", height = "400", style = "margin: 0 auto; display: block;"),

  br(),
  br(),
  h4("This shiny app can visualize graphs based on the World Happiness Index from 2015. You can select from a list of variables
     and see the appropriate graphs. You can also change the color and display statistics. ", style = "text-align: center; "),
  br(),
  br(),
    # Application title
    titlePanel(""),

    sidebarLayout(
        #Pick var
        sidebarPanel(
          
          selectInput("selectvar", label = h3("Choose a variable"), 
                      choices=list("Region"=1, "Score"=2, "GDP"=3, "Family"=4, "Life Expectancy"=5), 
                      selected = 1),
          p(em("Here you can select from a dropdown a variable you would like to see a graph of")),
          
        hr(),
        #Pick color
        colourInput("col", label = h3("Please select your color"), "lightblue"),
        p(em("Here you can choose a color for the graph, just click what color you like")),
        
        hr(),
        
        #show statistic

        sliderInput("slider1", label = h3("Score Range"), min = 0, 
                    max = 8, value = c(0, 8)),
        p(em("Here you can limit the Happiness score for the variable graph, on a slider")),
        hr(),
        
        checkboxInput("checkbox1", label="Display statistics", value=FALSE),
        p(em("Check the box if you want to see the statistics")),
        

        checkboxInput("checkbox2", label="Label Counts", value=FALSE),
        p(em("Check the box if you want to see the counts")),

        ),
        


        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           p(strong(em("Above is the graph based on your selected variable")), style = "text-align: center; "),
           br(),
           p(strong(em("Below is the statistics for the variables if you check the checkbox on the left labeled Display statistics")), style = "text-align: center; "),
           p("Table:"),
           fluidRow(column(5, verbatimTextOutput("table"))),
           p("Mean:"),
           fluidRow(column(5, verbatimTextOutput("mean"))),
           p("SD:"),
           fluidRow(column(5, verbatimTextOutput("sd"))),
           p("Median:"),
           fluidRow(column(5, verbatimTextOutput("median"))),
           p("Five Number Summary:"),
           fluidRow(column(5, verbatimTextOutput("fivenum"))),
           

           ),
        

    ),
  br(),
  br(),
  h3(strong("Citation:")),
  p(strong("Picture")),
  p("“World Happiness Report Finds That Crises Make Us Kinder.” Greater Good, greatergood.berkeley.edu/article/item/world_happiness_report_finds_that_crises_make_us_kinder. Accessed 19 Nov. 2023. "),
  p(strong("Kaggle Dataset")),
  p("Network, Sustainable Development Solutions. “World Happiness Report.” Kaggle, 27 Nov. 2019, www.kaggle.com/datasets/unsdsn/world-happiness/."),
  p(strong("World Happiness Report Website")),
  p("Helliwell, John F., et al. “World Happiness Report 2015.” The World Happiness Report, 23 Apr. 2015, worldhappiness.report/ed/2015/."),


  
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      if(input$selectvar == 1 & input$checkbox2 == TRUE){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        bar <- barplot(table(filter1$region), main = paste('Distribution of Regions') ,xlab='Regions',col = input$col, border = 'black', ylim = c(0, 55))
        text(bar, table(filter1$region),  labels = table(filter1$region), pos = 3)
        bar
      }
      else if (input$selectvar == 1){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        barplot(table(filter1$region), main = paste('Distribution of Regions') ,xlab='Regions',col = input$col, border = 'black', ylim = c(0, 55))
      }
        
      if(input$selectvar == 2 & input$checkbox2 == TRUE){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        hist1 <- hist(filter1$score, main='Distribution of Happiness Scores',xlab='Happiness Score',col = input$col, xlim=c(input$slider1[1], input$slider1[2]), labels = TRUE, ylim = c(0, 35))
        hist1
      }
      else if (input$selectvar == 2){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        hist1 <- hist(filter1$score, main='Distribution of Happiness Scores',xlab='Happiness Score',col = input$col, xlim=c(input$slider1[1], input$slider1[2]), ylim = c(0, 35))
        hist1
      }
      if(input$selectvar == 3 & input$checkbox2 == TRUE){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        hist2 <- hist(filter1$GDP, main='GDP influenced Happiness Score Histogram',xlab='Extent to which GDP influenced happiness score',col = input$col, border = 'black', labels = TRUE, ylim = c(0,35))
        hist2
      }
      else if (input$selectvar == 3){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        hist2 <- hist(filter1$GDP, main='GDP influenced Happiness Score Histogram',xlab='Extent to which GDP influenced happiness score',col = input$col, border = 'black', ylim = c(0,35))
        hist2
      }
      if(input$selectvar == 4 & input$checkbox2 == TRUE){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        hist3 <- hist(filter1$family, main='Family influenced Happiness Score Histogram',xlab='Extent to which family influenced happiness score',col = input$col, border = 'black', labels = TRUE, ylim = c(0,50))
        hist3
      }
      else if (input$selectvar == 4){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        hist3 <- hist(filter1$family, main='Family influenced Happiness Score Histogram',xlab='Extent to which family influenced happiness score',col = input$col, border = 'black', ylim = c(0,50))
        hist3
      }
      if(input$selectvar == 5 & input$checkbox2 == TRUE){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        hist4 <- hist(filter1$life_expectancy, main='Life Expectancy influenced Happiness Score Histogram',xlab='Extent to which Life Expectancy influenced happiness score',col = input$col, border = 'black', labels = TRUE, ylim = c(0,40))
        hist4
      }
      else if (input$selectvar == 5){
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        hist4 <- hist(filter1$life_expectancy, main='Life Expectancy influenced Happiness Score Histogram',xlab='Extent to which Life Expectancy influenced happiness score',col = input$col, border = 'black', ylim = c(0, 40))
        hist4
      }
    })
    
    output$table <- renderPrint({
      if(input$checkbox1 == TRUE & input$selectvar == 1) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        table(filter1$region)
      }
    })
    
    output$mean <- renderPrint({ 
      if(input$checkbox1 == TRUE & input$selectvar == 2) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(mean(filter1$score, na.rm=TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 3) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(mean(filter1$GDP, na.rm = TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 4) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(mean(filter1$family, na.rm = TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 5) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(mean(filter1$life_expectancy, na.rm = TRUE),2)}
    })
    
    output$sd <- renderPrint({ 
      if(input$checkbox1 == TRUE & input$selectvar == 2) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(sd(filter1$score, na.rm=TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 3) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(sd(filter1$GDP, na.rm = TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 4) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(sd(filter1$family, na.rm = TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 5) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(sd(filter1$life_expectancy, na.rm = TRUE),2)}
    })
    
    output$median <- renderPrint({ 
      if(input$checkbox1 == TRUE & input$selectvar == 2) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(median(filter1$score, na.rm=TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 3) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(median(filter1$GDP, na.rm = TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 4) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(median(filter1$family, na.rm = TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 5) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(median(filter1$life_expectancy, na.rm = TRUE),2)}
    })
    
    output$fivenum <- renderPrint({ 
      if(input$checkbox1 == TRUE & input$selectvar == 2) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(fivenum(filter1$score, na.rm=TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 3) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(fivenum(filter1$GDP, na.rm = TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 4) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(fivenum(filter1$family, na.rm = TRUE),2)}
      else if(input$checkbox1 == TRUE & input$selectvar == 5) {
        filter1 <- filter(happy, score >= input$slider1[1] & score <= input$slider1[2])
        round(fivenum(filter1$life_expectancy, na.rm = TRUE),2)}
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)

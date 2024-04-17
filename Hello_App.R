library(shiny)
movies <- read.csv("BollywoodData.csv")

ui <- fluidPage(
  titlePanel("Bollywood Movie Data"),
  div(
  tags$img(src = "https://images.moneycontrol.com/static-mcnews/2022/08/Bollywood-Flops-770x433.jpg?impolicy=website&width=770&height=431", width = 500, height = 250),
  tags$p("Image from Money Control", style = "font-style: italic;")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectvar", "Choose a variable", 
                  choices = list("Number of Screens"=1, "Revenue"=2, "Budget"=3, 
                                 "Verdicts"=4, "Genre" = 5), 
                  selected = 1),
      uiOutput("additional_questions"),
      selectInput("colorchoice", "Please select a color:",
                  choices = c("Red", "Blue", "Purple"),
                  selected = "Red"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      actionButton("calcbutton", "Calculate"),
    
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      verbatimTextOutput("result_text")
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    color <- switch(input$colorchoice,
                    "Red" = "red3",
                    "Blue"="cadetblue2",
                    "Purple"="mediumpurple1")
    if (input$selectvar == 1) {
      hist(movies$Number_of_Screens, breaks = input$bins, main='Distribution of Screens',xlab='Number of Screens',col = color, border = 'darkgrey')
    } else if (input$selectvar == 2) {
      hist(movies$RevenueUSD, breaks = input$bins, main='Distribution of Movie Revenue',xlab='Revenue (millions of $)',col = color, border = 'darkgrey')
    } else if (input$selectvar == 3) {
      hist(movies$BudgetUSD, breaks = input$bins, main='Distribution of Movie Budgets',xlab='Budget (millions of $)',col = color, border = 'darkgrey')
    } else if (input$selectvar == 4) {
      barplot(table(movies$Verdict), main='Distribution of Verdicts',xlab="Verdicts",col = color, border = 'darkgrey', ylim= c(0,200), las=2)
    } else if (input$selectvar == 5) {
      barplot(table(movies$Genre), main='Distribution of Genres',xlab='Genres',col = color, border = 'darkgrey')
    }
  })
  
  output$additional_questions <- renderUI({
    choice <- input$selectvar
    if (choice == 1){
      selectInput("stat1", "Display a Statistic",
                  choices = c("Mean", "Standard Deviation"))
    } else if (choice == 2) {
      selectInput("stat1", "Display a Statistic",
                  choices = c("Mean", "Standard Deviation"))
    } else if (choice == 3) {
      selectInput("stat1", "Display a Statistic",
                  choices = c("Mean", "Standard Deviation"))
    } else if (choice == 4) {
      selectInput("stat2", "Display a Statistic",
                  choices = c("Least Frequent", "Most Frequent"))
    } else if (choice == 5) {
      selectInput("stat2", "Display a Statistic",
                  choices = c("Least Frequent", "Most Frequent"))
    } 
  })
  
  observeEvent(input$calcbutton, {
    choice <- input$selectvar
    
    result <- switch(
      choice,
      "1" = {
        calculation <- input$stat1
        switch(calculation, 
               "Mean" = round(mean(movies$Number_of_Screens), 2),
               "Standard Deviation" = round(sd(movies$Number_of_Screens), 2))
      },
      "2" = {
        calculation <- input$stat1
        switch(calculation, 
               "Mean" = round(mean(movies$RevenueUSD), 2),
               "Standard Deviation" = round(sd(movies$RevenueUSD), 2))
      },
      "3" = {
        calculation <- input$stat1
        switch(calculation, 
               "Mean" = round(mean(movies$BudgetUSD), 2),
               "Standard Deviation" = round(sd(movies$BudgetUSD), 2))
      },
      "4" = {
        calculation <- input$stat2
        switch(calculation,
               "Least Frequent" = names(which.min(table(movies$Verdict))),
               "Most Frequent" = names(which.max(table(movies$Verdict))))
      },
      "5" = {
        calculation <- input$stat2
        switch(calculation,
               "Least Frequent" = names(which.min(table(movies$Genre))),
               "Most Frequent" = names(which.max(table(movies$Genre))))
      }
    )
    
    output$result_text <- renderPrint({
      paste("Statistic Result:", result)
    })
  })
}

shinyApp(ui = ui, server = server)

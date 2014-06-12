library(shiny)
# ui.R

shinyUI(fluidPage(
  titlePanel("Milk Vendor Version 1.2.2"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Input data on daily milk sales and
               expiration dates in order to
               see how much milk you should order. Randomize
               the data to see how the app works."),
      selectInput("select", label = h3("Randomize the data"), 
                  choices = list("Manual" = 1, "Randomize" = 2), 
                  selected = 1),
      br(),
      numericInput("cost", label = h5("Cost per unit"), value = 1),
      numericInput("sell", label = h5("Sell Price per unit"), value = 2),
      numericInput("exp", label = h5("Days until expiration"), value = 7),
      h4("Sales data"),
      numericInput("num1", label = h5("Day 1"), value = 1),
      numericInput("num2", label = h5("Day 2"), value = 1),
      numericInput("num3", label = h5("Day 3"), value = 1),
      numericInput("num4", label = h5("Day 4"), value = 1),
      numericInput("num5", label = h5("Day 5"), value = 1),
      numericInput("num6", label = h5("Day 6"), value = 1),
      numericInput("num7", label = h5("Day 7"), value = 1),
      numericInput("num8", label = h5("Day 8"), value = 1),
      numericInput("num9", label = h5("Day 9"), value = 1),
      numericInput("num10", label = h5("Day 10"), value = 1),
      numericInput("num11", label = h5("Day 11"), value = 1),
      numericInput("num12", label = h5("Day 12"), value = 1),
      numericInput("num13", label = h5("Day 13"), value = 1),
      numericInput("num14", label = h5("Day 14"), value = 1),
      numericInput("num15", label = h5("Day 15"), value = 1),
      numericInput("num16", label = h5("Day 16"), value = 1),
      numericInput("num17", label = h5("Day 17"), value = 1),
      numericInput("num18", label = h5("Day 18"), value = 1),
      numericInput("num19", label = h5("Day 19"), value = 1),
      numericInput("num20", label = h5("Day 20"), value = 1),
      numericInput("num21", label = h5("Day 21"), value = 1)
      ),
    
    mainPanel(
      tabsetPanel(type="tabs", 
                  tabPanel("Summary",
                           tableOutput("summary")),
                  tabPanel("Plot Sales per Day",
                           plotOutput("hist"))
      )
    )
  )))
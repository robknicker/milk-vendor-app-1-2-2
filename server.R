# server.R
library(shiny)
install.packages("data.table")
library(data.table)

shinyServer(
  function(input, output, clientData, session) {
    
    observe({
      sel=input$select
      rmean=runif(1,4,25)
      rsd=rmean/runif(1,2.5,5)
      c_nums = c(round(rnorm(21,rmean,rsd)))
      rcost=round(runif(1,.5,5), digits=2)
      rsell1=rcost*runif(1,1,3)
      rsell=round(rsell1, digits=2)
      rexpiration=round(runif(1,5,15))
      
      if (sel==2){
        updateNumericInput(session, "num1", value=c_nums[1])
        updateNumericInput(session, "num2", value=c_nums[2])
        updateNumericInput(session, "num3", value=c_nums[3])
        updateNumericInput(session, "num4", value=c_nums[4])
        updateNumericInput(session, "num5", value=c_nums[5])
        updateNumericInput(session, "num6", value=c_nums[6])
        updateNumericInput(session, "num7", value=c_nums[7])
        updateNumericInput(session, "num8", value=c_nums[8])
        updateNumericInput(session, "num9", value=c_nums[9])
        updateNumericInput(session, "num10", value=c_nums[10])
        updateNumericInput(session, "num11", value=c_nums[11])
        updateNumericInput(session, "num12", value=c_nums[12])
        updateNumericInput(session, "num13", value=c_nums[13])
        updateNumericInput(session, "num14", value=c_nums[14])
        updateNumericInput(session, "num15", value=c_nums[15])
        updateNumericInput(session, "num16", value=c_nums[16])
        updateNumericInput(session, "num17", value=c_nums[17])
        updateNumericInput(session, "num18", value=c_nums[18])
        updateNumericInput(session, "num19", value=c_nums[19])
        updateNumericInput(session, "num20", value=c_nums[20])
        updateNumericInput(session, "num21", value=c_nums[21])
        updateNumericInput(session, "exp", value=rexpiration)
        updateNumericInput(session, "cost", value=rcost)
        updateNumericInput(session, "sell", value=rsell)
      }
      
    })
    
    milk <- reactive({
      c(as.numeric(input$num1),
        as.numeric(input$num2),
        as.numeric(input$num3),
        as.numeric(input$num4),
        as.numeric(input$num5),
        as.numeric(input$num6),
        as.numeric(input$num7),
        as.numeric(input$num8),
        as.numeric(input$num9),
        as.numeric(input$num10),
        as.numeric(input$num11),
        as.numeric(input$num12),
        as.numeric(input$num13),
        as.numeric(input$num14),
        as.numeric(input$num15),
        as.numeric(input$num16),
        as.numeric(input$num17),
        as.numeric(input$num18),
        as.numeric(input$num19),
        as.numeric(input$num20),
        as.numeric(input$num21))  
    })
    
    mean <- reactive({
      sum(milk())/length(milk())
    })
    var <- reactive({
      sum((milk()-sum(milk())/length(milk()))^2)/(length(milk())-1) 
    })
    expi <- reactive({
      as.numeric(input$exp)
    })
    newmean <- reactive({
      expi()*mean()
    })
    newvar <- reactive({
      expi()^2*var()
    })
    cost <- reactive({
      as.numeric(input$cost)
    })
    sell <- reactive({
      as.numeric(input$sell)
    })
    overcost<- reactive({
      cost()
    })
    undercost<- reactive({
      sell() - cost()
    })
    q <- reactive({
      undercost()/(overcost()+undercost())
    })
    opt <- reactive({
      qnorm(q(), newmean(), sqrt(newvar()))
    })
    
    summary <- reactive({
      data.table(Summary=c("Mean per day",
                           "Standard deviation per day",
                           "Mean per expiration date",
                           "Standard deviation per expieration date",
                           "Optimal amount to order"),
                 Statistic=c(mean(),
                             sqrt(var()),
                             newmean(),
                             sqrt(newvar()),
                             opt()))
    })
    
    xest <- reactive({
      seq(newmean()-4*sqrt(newvar()), newmean()+4*sqrt(newvar()), length=1000)
    })
    
    yest <- reactive({
      dnorm(xest(), newmean(), sqrt(newvar()))
    })
    
    output$opt <- renderText({
      opt()
    })
    output$mean <- renderText({
      mean()
    })
    output$sd <- renderText({
      sqrt(var())
    })
    output$newmean <- renderText({
      newmean()
    })
    output$newsd <- renderText({
      sqrt(newvar())
    })
    output$hist <- renderPlot({
      hist(milk())
    })
    output$summary <- renderTable({
      summary()
    })
    output$estimate <- renderPlot({
      plot(xest(),yest())
    })
  }
)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(reshape2)

CONFIG_GOLD = 50


# Define UI for application that draws a histogram
ui <- page_sidebar(
    title = "吸收态随机游走演示",
    sidebar = sidebar(
            sliderInput("p",
                        "单次胜率（甲）",
                        min = 0,
                        max = 1,
                        value = 0.4),
            sliderInput("gold",
                        "初始资金（甲）,共10",
                        min = 0,
                        max = CONFIG_GOLD,
                        value = 6),
            selectInput(
              "select",
              "你认为的胜者是？",
              list("甲" = "A", "乙" = "B")
            ),
            actionButton("start", "开始模拟"),
            actionButton("random", "重选参数")#,
            #actionButton("trueP","真实概率展示")
            #,
            # value_box(
            #   title = "本次模拟的胜者",
            #   value = textOutput("result"),
            #   showcase = bs_icon("flag"),
            #   theme = "purple"
            # )
        ),
    card(
      card_header("理论讲解"),
      withMathJax(p("对赌的问题是在概率论研究中的经典模型，数学期望的概念就是在研究两人对赌中断时候分钱的方案中产生，接下来让我们看一个经典案例。")),
      p("如果甲乙两人对赌，资金共计为 \\(m\\)，直到一个人输光结束。这是一个马尔可夫过程（带两个吸收壁的随机游走），状态集 \\(S=\\{0,1,2, \\ldots, m\\}\\) ，当 \\(i \\neq 0, m\\)  时，转移函数满足 \\(p_{i, i-1}=q,p_{i, i+1}=p\\) 并且 \\(p+q=1\\)，而 \\(0, m\\) 作为吸收态满足 \\(p_{00}=p_{mm}=1\\)"),
      p("不妨设定到 \\(m\\) 为甲胜利，吸收到 \\(0\\) 为甲失败，以下讨论初始状态为 \\(i\\) 时，最终的成功概率 \\(a_i\\) "),
      p("对于成功的概率，显然有边界条件 \\(a_0=0, a_m=1\\) 以及递推关系 \\(a_i=(1-p) a_{i-1}+p a_{i+1}\\) ，整理得 \\((1-p)\\left(a_i-a_{i-1}\\right)=p\\left(a_{i+1}-a_i\\right)\\) "),
      p("令 \\(\\delta_i=a_{i+1}-a_i, \\rho=\\frac{1-p}{p}\\) ，得到 \\(\\delta_i=\\rho^i \\delta_0\\) 。利用 \\(\\sum_{i=0}^m \\delta_i=1\\) 推出 \\(\\delta_0=\\frac{1}{1+\\rho+\\cdots+\\rho^{m-1}}\\), 从而化简得到 \\(a_i=\\frac{1+\\rho+\\cdots+\\rho^{i-1}}{1+\\rho+\\cdots+\\rho^{m-1}}\\) ，即:"),
      div("$$a_i=\\left\\{\\begin{array}{ll}\\frac{1-\\rho^i}{1-\\rho^m} & \\rho \\neq 1 \\\\ \\frac{i}{m} & \\rho=1\\end{array}\\right.$$")
    ),
    layout_column_wrap(
      width = 1/3, height = 300,
      plotlyOutput("Plot"),
      plotlyOutput("Plot2"),
      layout_column_wrap(
        width = 1,heights_equal = "row",
        value_box(
        title = "甲的获胜期望是",
        value = textOutput("prob"),
        showcase = bs_icon("bar-chart"),
        theme = "purple"
        ),
        value_box(
            title = "随机模拟1000次的胜率",
            value = textOutput("probsim"),
            showcase = bs_icon("activity"),
            theme = "teal"
          )
      )
  )
)

runOnce <- function(p,i,t=0){
  new.i <- i
  ilist <- c(i)
  while(0<new.i&& new.i<CONFIG_GOLD){
    tmp <- new.i
    if(runif(1)<p){
      new.i <- tmp + 1
    }else{
      new.i <- tmp - 1
    }
    ilist=c(ilist,new.i)
  }
  if(t==0){
    return(ilist)
  }else{
    return(ilist[length(ilist)]/CONFIG_GOLD)
  }
}

runIter <- function(p,i){
  ilist <- runOnce(p,i)
  foo <- function(t){
    runOnce(p,i,t)
  }
  tmp <- lapply(1:1000, foo)
  probline <- cumsum(tmp)/1:1000
  return(list(ilist,probline))
}

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  resultText <- reactiveValues(Win=NULL,
                               Choice=NULL,
                               Right=NULL,
                               probsim=NULL)
  #数据清除
  observeEvent(input$random,{
    key1 <- runif(1)
    key2 <- runif(1)
    v1 <- 0.2+0.6*key1
    v2 <- 3+rbinom(1, CONFIG_GOLD-6, key2)
    if(v1==0.5)v1 <- v1 + randu(1) - randu(1)
    if(v1>0.5){
      v2 <- min(v2,CONFIG_GOLD-v2)
    }else{
      v2 <- max(v2,CONFIG_GOLD-v2)
    }
    updateSliderInput(session = session, "p",value =v1)
    updateSliderInput(session = session, "gold",value = v2)
    output$Plot <- renderPlotly({NULL})
    output$Plot2 <- renderPlotly({NULL})
    resultText$Win <- NULL
    resultText$Choice <- NULL
    resultText$Right <- NULL
    resultText$probsim <- NULL
  })

  observeEvent(input$start,{
    pp <- isolate(input$p)
    ii <- isolate(input$gold)
    rr <- runIter(pp,ii)

    golds <- rr[[1]]
    if(golds[length(golds)]==0){
      resultText$Win = "乙"
    }else{
      resultText$Win = "甲"
    }

    tmpy <- rr[[2]]
    #print(tmpy[length(tmpy)])
    resultText$probsim = tmpy[length(tmpy)]
    #print(resultText)

    output$Plot <- renderPlotly({
        tmp <- CONFIG_GOLD-golds
        tt <- 1:length(golds)
        mydata <- data.frame(cbind(tt,golds,tmp))

        fig <- plot_ly(mydata, x = ~tt, y = ~tmp, type = 'bar', name = '乙')
        fig <- fig %>% add_trace(y = ~golds, name = '甲')
        fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')%>%
          layout(                        # all of layout's properties: /r/reference/#layout
            title = "一次模拟结果", # layout's title: /r/reference/#layout-title
            xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
              title = "轮数",      # xaxis's title: /r/reference/#layout-xaxis-title
              showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
            yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
              title = "资金")     # yaxis's title: /r/reference/#layout-yaxis-title
          )

        fig
        #print(data)
        #plot_ly(data,x=data$Var2,y=data$value,type="bar",color=data$Var1)
      })

    output$Plot2 <- renderPlotly({
      x <- c(1:1000)
      y <- rr[[2]]
      data <- data.frame(x, y)
      plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'lines')
    })



#
#     observeEvent(input$trueP,{
      pp <- isolate(input$p)
      idx <- isolate(input$gold)
      if(pp==0.5){
        resultText$Right = idx/CONFIG_GOLD
      }else{
        rho <- (1-pp)/pp
        m <- CONFIG_GOLD#总资金
        resultText$Right = (1-rho^idx)/(1-rho^m)
      }
    # })

  })

  output$result <- renderText(resultText$Win)
  # output$result <- renderUI({
  #     s1 <- paste("胜利的是：",resultText$Win)
  #     s2 <- paste("您的选择策略是：",resultText$Choice)
  #     HTML(paste(s1, s2,  sep = '<br/>'))
  #     })

  output$prob <- renderText({
    resultText$Right
  })
  output$probsim <- renderText({
    resultText$probsim
  })


}

# Run the application
shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)
library(showtext)
library(shinythemes)

ui=fluidPage(
  theme=shinytheme("sandstone"),
  titlePanel("分化指标行情看板(2000-2019)",windowTitle = "Panel"),
  fluidRow(
    column(4,
           selectInput("select", label = h3("基础指标："), 
                       choices = list("换手率" = 1, "成交量" = 2, "股价" = 3, "市盈率"=4,"市净率"=5,"流通市值"=6,"流动比率"=7,"资产负债率"=8,"股息率"=9,"ROE"=10), 
                       selected = 1)),
    column(4,
           sliderInput("range", label = h3("时间段："), min = min(date), 
                       max = max(date), value = date))
  ),
  
  fluidRow(
    plotOutput("distPlot")
  )
)

server=function(input, output) {
  output$distPlot <- renderPlot({
    start=input$range[1]
    end=input$range[2]
    s=e=0
    for (i in 1:n) {
      if(input$range[1]>=date[i]){s=s+1}
      if(input$range[2]>=date[i]){e=e+1}
    }
    hchg=lchg=list()
    hchg[[1]]=hchg1
    hchg[[2]]=hchg2
    hchg[[3]]=hchg3
    hchg[[4]]=hchg4
    hchg[[5]]=hchg5
    hchg[[6]]=hchg6
    hchg[[7]]=hchg7
    hchg[[8]]=hchg8
    hchg[[9]]=hchg9
    hchg[[10]]=hchg10
    lchg[[1]]=lchg1
    lchg[[2]]=lchg2
    lchg[[3]]=lchg3
    lchg[[4]]=lchg4
    lchg[[5]]=lchg5
    lchg[[6]]=lchg6
    lchg[[7]]=lchg7
    lchg[[8]]=lchg8
    lchg[[9]]=lchg9
    lchg[[10]]=lchg10
    h=l=seq(1:(e-s+1))
    h[1]=l[1]=100
    for (i in 2:(e-s+1)) {
      h[i]=h[i-1]*(1+hchg[[as.numeric(input$select)]][i-1]/100)
      l[i]=l[i-1]*(1+lchg[[as.numeric(input$select)]][i-1]/100)
    }
    name=c("换手率","成交量","股价","市盈率","市净率","流通市值","流动比率","资产负债率","股息率","ROE")
    h_name=rep(paste("高",name[as.numeric(input$select)],sep=""),length(h))
    l_name=rep(paste("低",name[as.numeric(input$select)],sep=""),length(l))
    ggplotdata_h=data.frame(date[s:e],h,h_name)
    colnames(ggplotdata_h)[1:3]=c("时间","指数","指标名称")
    ggplotdata_l=data.frame(date[s:e],l,l_name)
    colnames(ggplotdata_l)[1:3]=c("时间","指数","指标名称")
    ggplotdata=rbind(ggplotdata_h,ggplotdata_l)
    ggplot(ggplotdata,aes(x=时间,y=指数,group=指标名称,colour=指标名称))+
      geom_line(size=1.1)+
      scale_color_manual(values=c('#8B0000','#363636'))+
      geom_smooth(size=0.8)+
      theme(legend.position="bottom")
  })
}

shinyApp(ui,server)

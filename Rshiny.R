#library
#loadfonts(dev="win")
#install.packages('extrafont')
#library(extrafont)
#extrafont::font_import()
#windowsFonts(Times=windowsFont('Times New Roman'))

#install.packages('ggmap')
#install.packages('raster')
#install.packages('rgeos')
#install.packages('maptools')
#install.packages('rgdal')
#install.packages('plotly')
#install.packages('fpc') 
#install.packages('NbClust')
#install.packages('shiny')
#install.packages('shape')

library(ggmap)

register_google(key='AIzaSyCkGsjzK5QAZQwFZCSpqqc5oKa-FpsGf2k')
library(ggplot2)

library(raster)
library(rgeos)
library(maptools)
library(sp)
library(rgdal)
library(plotly)

library(fpc)
library(NbClust) 

library(shiny)
library(dplyr)
library(scales)
library(shape)
theme_set(theme_bw(base_family='NanumGothic'))
#---------------------------------------------#
#1.
ui1 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x",
                  label = "지역구",
                  choices = c("서울시", levels(seoul$SGNG_NM)),
                  selected = "서울시"),
      plotlyOutput("map", width=400, height=700)
    ),
    mainPanel(
      splitLayout(
        plotOutput(outputId = "pie", width=400, height=400),
        plotOutput(outputId = "sex", width=400, height=400)
      ),
      splitLayout(
        plotOutput(outputId = "day", width=400, height=400),
        plotOutput(outputId = "time", dblclick = "plot1_dblclick",
                   brush = brushOpts(id="plot1_brush", resetOnNew = T), width=400, height=400)
      )
    )
  )
)
server1 <- function(input, output, session) {
  output$map <- renderPlotly({
    if(input$x=="서울시"){
      s <- seoul
      lon1 <- 127.001
      lat1 <- 37.564
      num <- 11
      num1 <- 45
      add <- scale_colour_gradient(low="lightblue", high="red")
    }else{
      s <- seoul[seoul$SGNG_NM==input$x,]
      lon1 <- median(s$lon)
      lat1 <- median(s$lat)
      num <- 13
      num1 <- 2
      add <- NULL
    }
    map_seoul <- get_map(location=c(lon=lon1, lat=lat1), zoom=num, maptype='roadmap')
    p <- ggmap(map_seoul) +
      geom_point(data=s, aes(x=lon, y=lat, col=s[,num1], size=sum, key=key)) +
      labs(x="", y="") +
      theme(plot.title=element_text(size=18, hjust=0.5),
            legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) +
      add
    ggplotly(p) %>% layout(dragmode = "select")
  })
  
  output$sex <- renderPlot({
    d <- event_data("plotly_click")
    if(is.null(d$curveNumber)){
      d$curveNumber <- 0
    }
    if(input$x=="서울시"){
      s <- seoul
      if(d$curveNumber > 1){
        s <- seoul[seoul$key==d$key,]
      }
    }else{
      s <- seoul[seoul$SGNG_NM==input$x,]
      if(d$curveNumber > 1){
        s <- seoul[seoul$key==d$key,]
      }
    }
    total <- data.frame("sex" = c("Male", "Female"),
                        "sum" = c(sum(s$MAN_SALE_SUM), sum(s$WMAN_SALE_SUM)))
    total$per <- total$sum/sum(total$sum)
    total$sex <- as.factor(total$sex)
    ggplot(total, aes(x="", y=sum, col=sex, fill=sex)) +
      geom_bar(stat="identity", width = 1) +
      coord_polar("y", start=0) +
      labs(title="성별", x="", y="") +
      #theme_bw(base_family='Times') + 
      geom_text(aes(label=paste0(round(per,2)*100,"%")), position=position_stack(vjust=0.5), size=5, col="white") + 
      theme(plot.title=element_text(size=18, hjust=0.5),
            legend.key = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_blank())
  })
  
  output$pie <- renderPlot({
    d <- event_data("plotly_click")
    if(is.null(d$curveNumber)){
      d$curveNumber <- 0
    }
    if(input$x=="서울시"){
      s <- seoul    
      if(d$curveNumber > 1){
        s <- seoul[seoul$key==d$key,]
      }
    }else{
      s <- seoul[seoul$SGNG_NM==input$x,]
      if(d$curveNumber > 1){
        s <- seoul[seoul$key==d$key,]
      }
    }
    total <- data.frame("age" = c(10, 20, 30, 40, 50, 60),
                        "sum" = c(sum(s$`10G_SALE_SUM`), sum(s$`20G_SALE_SUM`), sum(s$`30G_SALE_SUM`), 
                                  sum(s$`40G_SALE_SUM`), sum(s$`50G_SALE_SUM`), sum(s$`60G_SALE_SUM`)))
    total$per <- total$sum/sum(total$sum)
    total$age <- as.factor(total$age)
    ggplot(total, aes(x="", y=sum, col=age, fill=age)) +
      geom_bar(stat="identity", width = 1) +
      coord_polar("y", start=0) +
      labs(title="연령별", x="", y="") +
      #theme_bw(base_family='Times') + 
      geom_text(aes(label=paste0(round(per,2)*100,"%")), position=position_stack(vjust=0.5), size=5, col="white") + 
      theme(plot.title=element_text(size=18, hjust=0.5),
            legend.key = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_blank())
  })
  
  output$day <- renderPlot({
    d <- event_data("plotly_click")
    if(is.null(d$curveNumber)){
      d$curveNumber <- 0
    }
    if(input$x=="서울시"){
      num <- 1
      s <- a
      if(d$curveNumber > 1){
        s <- seoul[seoul$key==d$key,]
        total <- data.frame("geo" = s$ADONG_NM,
                            "day" = c("Mon", "Tus", "wed", "Thu", "Fri", "Sat", "Sun"), 
                            "sum" = apply(s[,c(35:41)], 2, sum))
        total$day <- factor(total$day, c("Mon", "Tus", "wed", "Thu", "Fri", "Sat", "Sun"))
        s <- total
      }
    }else{
      s <- b[b$region==input$x,]
      num <- 2
      if(d$curveNumber > 1){
        s <- seoul[seoul$key==d$key,]
        total <- data.frame("geo" = s$ADONG_NM,
                            "day" = c("Mon", "Tus", "wed", "Thu", "Fri", "Sat", "Sun"), 
                            "sum" = apply(s[,c(35:41)], 2, sum))
        total$day <- factor(total$day, c("Mon", "Tus", "wed", "Thu", "Fri", "Sat", "Sun"))
        s <- total
        num <- 1
      }
    }
    ggplot(s) +
      geom_bar(aes(x=day, y=sum), stat = "identity") +
      #theme_bw(base_family='Times') + 
      labs(title="요일별", x="", y="") +
      theme(plot.title=element_text(size=18, hjust=0.5))
  })
  
  ranges <- reactiveValues(x=NULL, y=NULL)
  output$time <- renderPlot({
    d <- event_data("plotly_click")
    if(is.null(d$curveNumber)){
      d$curveNumber <- 0
    }
    if(input$x=="서울시"){
      num <- 1
      s <- c
      if(d$curveNumber > 1){
        s <- seoul[seoul$key==d$key,]
        total <- data.frame("geo" = s$ADONG_NM,
                            "time" = c(0:23), 
                            "sum" = apply(s[,c(11:34)], 2, sum))
        total$time <- factor(total$time, c(0:23))
        s <- total
      }
    }else{
      s <- e[e$region==input$x,]
      num <- 2
      if(d$curveNumber > 1){
        s <- seoul[seoul$key==d$key,]
        total <- data.frame("geo" = s$ADONG_NM,
                            "time" = c(0:23), 
                            "sum" = apply(s[,c(11:34)], 2, sum))
        total$time <- factor(total$time, c(0:23))
        s <- total
        num <- 1
      }
    }
    ggplot(s) +
      geom_line(aes(x=time, y=sum, group=s[,num], col=s[,num])) +
      coord_cartesian(xlim=ranges$x, ylim=ranges$y, expand = F) +
      labs(title="시간대별", x="", y="") +
      #theme_bw(base_family='Times') + 
      theme(plot.title=element_text(size=18, hjust=0.5),
            legend.key = element_blank(),
            legend.title = element_blank())
  })
  observeEvent(input$plot1_dblclick,{
    brush <- input$plot1_brush
    if(!is.null(brush)){
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }else{
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}

shinyApp(ui1, server1)
#---------------------------------------------#
#2. 
ui2 <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x",
                  label = "지역구",
                  choices = c("서울시", levels(sido_new$SGNG_NM)),
                  selected = "서울시"),
      plotOutput(outputId = "position", dblclick = "plot_dblclick",
                 brush = brushOpts(id="plot_brush", resetOnNew = T),
                 , width=400, height = 300),
      DT::dataTableOutput(outputId = "SGNG")
    ),
    mainPanel(
      plotOutput(outputId = "cluster", width=900, height = 500),
      plotOutput(outputId = "map1",  width=900, height = 500)
    )
  )
)
server2 <- function(input, output) {
  ranges <- reactiveValues(x=NULL, y=NULL)
  output$position <- renderPlot({
    if(input$x=="서울시"){
      s <- sido_new
    }else{
      s <- sido_new[sido_new$SGNG_NM==input$x,]    
    }
    m <- median(s$main)
    si <- median(s$side)
    s$main <- s$main - m
    s$side <- s$side - si
    ggplot(data=s, aes(x=main, y=side, col=ADONG_NM))+
      geom_jitter() +
      coord_cartesian(xlim=ranges$x, ylim=ranges$y, expand = F) +
      labs(x="주소비층(30,40,50대)_금액", y="비소비층(10,20,60대)_금액") +
      geom_vline(aes(xintercept=m)) +
      geom_hline(aes(yintercept=si)) +
      #theme_bw(base_family='Times') + 
      theme(plot.title=element_text(size=18, hjust=0.5),
            legend.position = "none")
  })
  observeEvent(input$plot_dblclick,{
    brush <- input$plot_brush
    if(!is.null(brush)){
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }else{
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  output$SGNG <- DT::renderDataTable({
    if(input$x=="서울시"){
      s <- sido_new
    }else{
      s <- sido_new[sido_new$SGNG_NM==input$x,]    
    }
    brushedPoints(s, req(input$plot_brush)) %>%
      select(SGNG_NM, ADONG_NM, main, side)
  })
  output$map1 <- renderPlot({
    if(input$x=="서울시"){
      s <- sido_new
      num <- 11
      lon1 <- 127.001
      lat1 <- 37.564
    }else{
      s <- sido_new[sido_new$SGNG_NM==input$x,]    
      num <- 14
      lon1 <- median(s$lon)
      lat1 <- median(s$lat)
    }
    map_seoul <- get_map(location=c(lon=lon1, lat=lat1), zoom=num, maptype='roadmap')
    ggmap(map_seoul) +
      geom_point(data=s, aes(x=lon, y=lat, fill=cluster), shape=21, alpha=0.7) +
      facet_grid(. ~ cluster) + 
      labs(x="", y="") +
      #theme_bw(base_family='Times') + 
      theme(plot.title=element_text(size=18, hjust=0.5),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
  })
  output$cluster <- renderPlot({
    if(input$x=="서울시"){
      s <- sido_new
    }else{
      s <- sido_new[sido_new$SGNG_NM==input$x,]    
    }
    ggplot() +
      geom_point(data=s, aes(x=man_prop, y=main_prop, col=cluster)) +
      theme(plot.title=element_text(size=18, hjust=0.5)) +
      labs(x="남자 비율", y="주소비층_비율") +
      #theme_bw(base_family='Times') + 
      geom_vline(aes(xintercept=0.5)) +
      geom_hline(aes(yintercept=0.5))
  })
}

shinyApp(ui = ui2, server = server2)

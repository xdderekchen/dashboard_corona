
library(shiny)
library(DT)
library(reshape2)
library(dygraphs)
library(shinydashboard)
library(htmltools)
library(shinyBS)
library(highcharter)
source("./data.R")

global_data <- get_init_data()

#last_date = global_data[["lastdate"]]

#global_data_daily = get_daily_data()

ui <- dashboardPage(
  
  dashboardHeader(title = "COVID-19 Trend Analysis",  titleWidth = 300, disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  body=  dashboardBody(
    tags$head(includeHTML("google-analytics.html")),
   
    fluidRow(
        shinydashboard::box( width=12,   title = "Country by country: how COVID-19 case trajectories compare", status = "primary", solidHeader = TRUE,
                             highchartOutput("hcontainer", height='400px')
        
      )
    )
    ,
    verbatimTextOutput("urlText")
    # ,
    # fluidRow(
    #   shinydashboard::box( width=12,   title = "Country by country: how COVID-19 case trajectories compare", status = "primary", solidHeader = TRUE,
    #                        dygraphOutput("dygraph", height='500px')
    #                        
    #   )
    # )
  )
)



server <- function(input, output, session) {
  
  output$urlText <- renderText({
    paste(sep = "",
          "protocol: ", session$clientData$url_protocol, "\n",
          "hostname: ", session$clientData$url_hostname, "\n",
          "pathname: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n"
    )
  })
  myData <- reactive({
      data = get_data_sinceXcases(X= 100,top=25, inputdata = global_data)
      print(data)
      data
  })
  output$dygraph <-   renderDygraph({
    mainData <- myData()[["data"]]
    X = mainData[["tsX"]]
    cnt = ncol(mainData)
    z = data.frame(mainData[, 2:cnt])
    z = ts(z)
    #rownames(z) <- X
    
    
    dygraph(z) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.5,
                  hideOnMouseOut = TRUE) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "count", valueRange=c(100, NULL)) %>%
      dyOptions(includeZero = F, logscale=T) %>%  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) 
  })
  
  output$hcontainer <-   renderHighchart({
    mainData <- myData()[["data"]]
    predictedX = 30
    mainData["predict33"] <-  ifelse(mainData$tsX <= predictedX, (100 * (1.33^ (mainData$tsX-1))), NA)
    mainData["predict40"] <-  ifelse(mainData$tsX <= predictedX, (100 * (1.4^ (mainData$tsX-1))), NA)
    

    X = mainData["tsX"]
    print(mainData)
   
    xJapan <- mainData %>% filter(!is.na(Japan)) %>%  summarise(id=max(tsX)) %>% select(id) %>% first() %>% unlist()
    xUS    <- mainData %>% filter(!is.na(US))    %>%  summarise(id=max(tsX)) %>% select(id) %>% first() %>% unlist()
    xItaly    <- mainData %>% filter(!is.na(Italy))    %>%  summarise(id=max(tsX)) %>% select(id) %>% first() %>% unlist()
    xIran   <- mainData %>% filter(!is.na(Iran))    %>%  summarise(id=max(tsX)) %>% select(id) %>% first() %>% unlist()
    xFrance    <- mainData %>% filter(!is.na(France))    %>%  summarise(id=max(tsX)) %>% select(id) %>% first() %>% unlist()
    xKorea    <- mainData %>% filter(!is.na(`Korea, South`))    %>%  summarise(id=max(tsX)) %>% select(id) %>% first() %>% unlist()
    
    ys = colnames(mainData)
    #colors <- c( "#07E4EF","#92FB8F", "#FB1108", "#2980b9","#ABD6E6","#9AD2E1","#FD150B","#FA7806","#FBE426","#FCFB8F",
    #             "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")
    
    #hc_tooltip(table = TRUE, sort = TRUE) %>% 
    hc <- highchart() %>%
      hc_title(
        text = "By the number of days since <span style=\"color:#e5b13a\">100th case</span>",
        useHTML = TRUE) %>%
      hc_xAxis(categories = X, title=list(text = "Number of days since 100th case -->")) %>%
      hc_yAxis(title = list(text = "Confirmed cases - by logarithmic"), opposite = FALSE, type= 'logarithmic', min=100) %>%
      hc_exporting(enabled = TRUE)  %>%
      hc_plotOptions(line = list(
                                 marker = list(
                                   lineWidth = 1,
                                   enabled = F
                                 ))) %>% hc_add_theme(hc_theme_ffx())
      
    
    
    if (0 < length(ys)) {
      for (i in 2:length(ys)) {
        if ( ys[i] %in% c("China", "US", "predict33", "predict40")) {
          lineWdith = 2
          if (ys[i] %in% c("predict33", "predict40")) {
            hc <- hc %>% hc_add_series(name = ys[i], data = mainData[, ys[i]],type = "line", showInLegend = F, lineWidth = 1, dashStyle = 'Dot', color = "#000000")
          }
          else
          {
            hc <- hc %>% hc_add_series(name = ys[i], data = mainData[, ys[i]],type = "line", showInLegend = F, lineWidth = lineWdith)
          }
        }
        else {
          lineWdith = 1
          hc <- hc %>% hc_add_series(name = ys[i], data = mainData[, ys[i]],type = "line", showInLegend = F, lineWidth = lineWdith)
        }
        
      }
    }
    
   ### hc %>% hc_add_annotations ( list(xValue = 10, yValue = mainData[10, ys[2]], text = 'Annotated chart!')) -> hc
  
    hc %>% 
      hc_annotations(
        list(
          labels = list(
            list(point = list(x = 50,     y = mainData[50, "China"], xAxis = 0, yAxis = 0), text = "China"),
            list(point = list(x = as.integer(xJapan)-1, y = mainData[xJapan, "Japan"], xAxis = 0, yAxis = 0), text = "Japan"),
            list(point = list(x = as.integer(xUS)-1,    y = mainData[xUS, "US"],       xAxis = 0, yAxis = 0), text = "US"),
            list(point = list(x = predictedX-1,         y = mainData[predictedX, "predict33"],       xAxis = 0, yAxis = 0), text = "Predicted(33%)"),
            list(point = list(x = predictedX-1,         y = mainData[predictedX, "predict40"],       xAxis = 0, yAxis = 0), text = "Predicted(40%)"),
            list(point = list(x = as.integer(xItaly)-1,    y = mainData[xItaly, "Italy"],       xAxis = 0, yAxis = 0), text = "Italy"),
            list(point = list(x = as.integer(xIran)-1,    y = mainData[xIran, "Iran"],       xAxis = 0, yAxis = 0), text = "Iran"),
            list(point = list(x = as.integer(xFrance)-1,    y = mainData[xFrance, "France"],       xAxis = 0, yAxis = 0), text = "France"),
            list(point = list(x = as.integer(xKorea)-1,    y = mainData[xKorea, "Korea, South"],       xAxis = 0, yAxis = 0), text = "Korea")
          
           
            
          )
        )
      ) -> hc
    
    hc
    
  })
  
  
}


# Run the application 
#shinyApp(ui = ui, server = server) 

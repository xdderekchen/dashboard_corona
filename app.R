#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(sparkline)
library(dplyr)
library(dygraphs)
library(reshape2)
library(shinydashboard)
library(leaflet)
library(htmltools)
source("./data.R")
global_data <- get_init_data()
last_date = global_data[["lastdate"]]



ui <- dashboardPage(
    dashboardHeader(title = "Dashboard for Coronavirus",  titleWidth = 300),
    dashboardSidebar(disable = TRUE),
    
    body=  dashboardBody(
        tags$div(
            tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                   "Live Data from Johns Hopkins - Whiting School of Engineering: https://raw.githubusercontent.com/CSSEGISandData")
        ),
        fluidRow(
            column(width=6,
                tags$div(tags$h2(paste("last updated:", as.character(last_date))))),
            column(width=6,
                selectInput("selcountry", "Top 30 Countries:",
                        c("World" = "World")))
            
        ),
        fluidRow(
            valueBoxOutput("vconfirmed"),
            valueBoxOutput("vdeath"),
            valueBoxOutput("vrecovered")
        ),
        fluidRow(
            box(
                width = 12, status = "info", solidHeader = TRUE,
                title = "Time Series",
                
                box(width = 4,dygraphOutput("dygraphC", height='150px')),
                box(width = 4,dygraphOutput("dygraphD", height='150px')),
                box(width = 4,dygraphOutput("dygraphR", height='150px'))
               
            )
            
            ),
        fluidRow(
            box(
                width = 12, status = "info",
               
                leafletOutput("mymap", width = "100%", height = 300)
            )
        )
   )
)


            
server <- function(input, output, session) {
    
    topCountries = get_top_country(global_data$confirm)
    updateSelectInput(session, "selcountry",
                      
                      choices = c("World", topCountries)
    )
    
    myCountry <- reactive({
        
        myselCountry     <- input$selcountry
        v = strsplit(myselCountry, split=" - ")[[1]][1]
        return (v)
    })
    
    output$vconfirmed <- renderValueBox({
        country = myCountry()
      
        value = get_last_value(global_data$confirm, country, last_date)
        valueBox(
            value = format(value, big.mark=","),  #formatC(value, digits = 1, format = "f"),
            subtitle = "Confirmed",
            icon = icon("users"), color="blue"
        )
    })

    output$vdeath <- renderValueBox({
        country = myCountry()
        value = get_last_value(global_data$death, country, last_date)
        valueBox(
            value = format(value, big.mark=","),
            subtitle = "Total deaths",
            icon = icon("users"), color="yellow"
        )
    })

    output$vrecovered <- renderValueBox({
        country = myCountry()
        value = get_last_value(global_data$recovered,country, last_date)
        valueBox(
            value = format(value, big.mark=","),
            "Total Recovered",
            icon = icon("users"),  color="green"
        )
    })
    
    output$dygraphC <-   renderDygraph({
        country = myCountry()
        data = get_time_series(data0 = global_data$confirm, country= country)
       
        z = data.frame(confirm = data$new_value)
        rownames(z) <- data$date
        dygraph(z, main="Confirmed", group="A") %>%
            dyLegend(show = "onmouseover", hideOnMouseOut = FALSE) %>%
            dyHighlight(highlightCircleSize = 5, 
                        highlightSeriesBackgroundAlpha = 0.5,
                        hideOnMouseOut = TRUE) %>%
            dyAxis("x", drawGrid = FALSE) %>%
            dyAxis("y", label = "Count")  
        
        
    })
    
    output$dygraphD <-   renderDygraph({
        country = myCountry()
        data = get_time_series(data0 = global_data$death, country= country)
        z = data.frame(death = data$new_value)
        rownames(z) <- data$date
        dygraph(z, main="Death", group="A") %>%
            dyLegend(show = "onmouseover", hideOnMouseOut = FALSE) %>%
            dyHighlight(highlightCircleSize = 5, 
                        highlightSeriesBackgroundAlpha = 0.5,
                        hideOnMouseOut = TRUE) %>%
            dyAxis("x", drawGrid = FALSE) %>%
            dyAxis("y", label = "Count")  
        
      
        
        
    })
    output$dygraphR <-   renderDygraph({
        country = myCountry()
        data = get_time_series(data0 = global_data$recovered, country= country)
        z = data.frame(recovered = data$new_value)
        rownames(z) <- data$date
        dygraph(z, main="Recovered", group="A") %>%
            dyLegend(show = "onmouseover", hideOnMouseOut = FALSE) %>%
            dyHighlight(highlightCircleSize = 5, 
                        highlightSeriesBackgroundAlpha = 0.5,
                        hideOnMouseOut = TRUE) %>%
            dyAxis("x", drawGrid = FALSE) %>%
            dyAxis("y", label = "Count")  
        
        
        
        
    })
   
    output$mymap <- renderLeaflet({
        country = myCountry()
        data <- get_map_data(country=country, data0=global_data$confirm, date0 = last_date)
       
        leaflet(data) %>% addCircles(lng = ~Long, lat = ~Lat) %>% 
            addTiles() %>% addCircleMarkers(data = data, lng = ~Long, lat = ~Lat, 
                                            radius = 3, popup = ~as.character(content), 
                                          
                                            stroke = FALSE, fillOpacity = 0.8)
    })
}


# Run the application 
shinyApp(ui = ui, server = server) 

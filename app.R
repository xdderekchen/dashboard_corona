
library(shiny)
library(DT)
library(reshape2)
library(shinydashboard)
library(htmltools)
source("./data.R")

#global_data <- get_init_data()
#last_date = global_data[["lastdate"]]

global_data_daily = get_daily_data()

ui <- dashboardPage(
  
  dashboardHeader(title = "DC-MD-VA Coronavirus",  titleWidth = 300, disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  body=  dashboardBody(
    tags$head(includeHTML("google-analytics.html")),
    #tags$div(
    #tags$h3("Most updated information pulled from State Govement's CDC sites, you may click the icon to go the CDC websites")),
    fluidRow(
      box(
        width = 12, status = "info", title="Live Data from CDC sites", solidHeader = TRUE,
        
        fluidRow(
             valueBoxOutput("vDC"),
             valueBoxOutput("vMD"),
             valueBoxOutput("vVA")
        ),
        div(DTOutput("chinaCDC"), style = "font-size: 65%; width: 65%")
       
      )
    ),
    
    fluidRow(
      box(
        width = 12, status = "info", title="Data from John Hopkins, daily updated", solidHeader = TRUE,
        div(DTOutput("world"), style = "font-size: 70%; width: 70%"),
        tags$style(HTML("hr {border-top: 1px solid #000000;}")),
        div(DTOutput("USA"), style = "font-size: 70%; width: 70%")
      )
    )
   
  )
)



server <- function(input, output, session) {
  
 
  URLs = get_State_CDC_URL()
  dc_data <- get_DC(URLs[URLs$REGION=="DC", "URL"])
  md_data <- get_MD(URLs[URLs$REGION=="MD", "URL"])
  va_data <- get_VA(URLs[URLs$REGION=="VA", "URL"])
 
  china_data <- get_China()
  
  china_data <- spread(china_data, category, value)
  rownames(china_data) <- c("CHINA CDC")
  china_data[, c("Date", "Confirmed cases", "Deaths", "Recoveries", "Suspected cases")] -> china_data
  
  output$chinaCDC <- renderDT(
    datatable(
     data= china_data, class = "compact",
     options = list(
       dom = 't',
       ordering = FALSE,
       paging = FALSE,
       searching = FALSE)
   )
  )
  
  
  world_data <- get_daily_data_world(global_data_daily[[2]])
  USA_data   <- get_daily_data_US(global_data_daily[[2]])
  
  
 # output$china <- renderTable(china_data)
  
  output$world <- renderDT(datatable(data = world_data,class = "compact",
                          
                           caption = tags$caption("World", style = "color:Blue"),
                           options = list(
                             dom = 't',
                             ordering = FALSE,
                             paging = FALSE,
                             searching = FALSE))
                           )
  
  output$USA <- renderDT(datatable(data = USA_data,class = "compact",
                        
                         caption = tags$caption("USA", style = "color:Blue"),
                         options = list(
                           dom = 't',
                           ordering = FALSE,
                           paging = FALSE,
                           searching = FALSE))
  )
  
  #output$DC <- renderTable(dc_data[[2]])
  #output$MD <- renderTable(md_data[[2]])
  #output$VA <- renderTable(va_data[[2]])

 
  
  output$vDC <- renderValueBox({
    if (is.na(dc_data)) {
      value = "NA"
    } else
    {
      value = format(dc_data[[1]], big.mark=",")
    }
    valueBox(
      value = value,
      "Washington DC",
      icon = icon("users"),  color="blue",
      href = URLs[URLs$REGION=="DC", "URL"]
    )
  })
  
  output$vMD <- renderValueBox({
    if (is.na(md_data)) {
      value = "NA"
    } else
    {
      value = format(md_data[[1]], big.mark=",")
    }
    valueBox(
      value = value,
      "Maryland",
      icon = icon("users"),  color="blue",
      href = URLs[URLs$REGION=="MD", "URL"]
    )
  })
  
  output$vVA <- renderValueBox({
    if (is.na(va_data)) {
      value = "NA"
    } else
    {
      value = format(va_data[[1]], big.mark=",")
    }
    valueBox(
      value = value,
      "Virginia",
      icon = icon("users"),  color="blue",
      href =  URLs[URLs$REGION=="VA", "URL"]
    )
  })
  
  
    
 
}


# Run the application 
shinyApp(ui = ui, server = server) 


library(shiny)
library(DT)
library(reshape2)
library(shinydashboard)
library(htmltools)
library(shinyBS)
source("./data.R")

global_data_daily = get_daily_data()

ui <- dashboardPage(
  
  dashboardHeader(title = "DC-MD-VA COVID-19",  titleWidth = 300, disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  body=  dashboardBody(
    tags$head(includeHTML("google-analytics.html")),
    
    fluidRow(
      box(
        width = 12, status = "info", title="Live Data from CDC sites", solidHeader = TRUE,
        
        # fluidRow(
        #      infoBoxOutput("vDC"),
        #      infoBoxOutput("vMD"),
        #      valueBoxOutput("vVA")
        #      #valueBoxOutput("vVA")
        # ),
        div(style = "padding: padding: -2px 0px; margin:0em",
        fluidRow(
          box(
            width = 4, status = "primary", title="District of Columbia", solidHeader = T, 
            DT::dataTableOutput("DC_VALUE")
          ),
          box(
            width = 4, status = "primary", title="Maryland", solidHeader = T,
            DT::dataTableOutput("MD_VALUE")
          ),
          box(
            width = 4, status = "primary", title="Virginia", solidHeader = T,
            DT::dataTableOutput("VA_VALUE")
          )
        )),
        div(style = "padding: padding: -2px 0px; margin:0em",
        fluidRow(
        box(
          width = 12, status = "primary", title="China", solidHeader = T,  
          div(DTOutput("chinaCDC0"), style = "font-size: 65%; width: 65%")
        )))#,
        
        #div(DTOutput("chinaCDC"), style = "font-size: 65%; width: 65%")
          
      )
    ),
    
    fluidRow(
      box(
        width = 12, status = "info", title="Data from John Hopkins, daily updated", solidHeader = TRUE,
        div(DTOutput("world"), style = "font-size: 73%; width: 73%"),
        tags$style(HTML("hr {border-top: 1px solid #000000;}")),
        div(DTOutput("USA"), style = "font-size: 70%; width: 70%")
      )
    )
  )
)



server <- function(input, output, session) {
  
  URLs = get_State_CDC_URL()
  
  dc_data <- list() #get_DC(URLs[URLs$REGION=="DC", "URL"])
  #md_data <- get_MD(URLs[URLs$REGION=="MD", "URL"])
  #va_data <- get_VA(URLs[URLs$REGION=="VA", "URL"])
  md_data = list() #Because CDC sites uses tabeleu, make it impossible to do web scrabling.
  va_data = list()
  
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
                          
                           #caption = tags$caption("World", style = "color:Blue"),
                           options = list(
                             dom = 't',
                             ordering = FALSE,
                             paging = FALSE,
                             searching = FALSE))
                           )
  
  output$USA <- renderDT(datatable(data = USA_data,class = "compact",
                        
                         #caption = tags$caption("USA", style = "color:Blue"),
                         options = list(
                           dom = 't',
                           ordering = FALSE,
                           paging = FALSE,
                           searching = FALSE))
  )
  
  archiveData <- get_default_value(global_data_daily[[2]])
  testData    <- get_test_data()
  archivePre3days <- get_default_value(global_data_daily[[3]])
  
  informationDialog <- function()
  {
    v = 0
    if (length(dc_data) > 0) {
      v = strtoi(dc_data[[1]])
    }
    pmax(v, archiveData[[1]]["District of Columbia"], testData[[1]]["DC"],  na.rm =TRUE) -> dcvalue
    archivePre3days[[1]]["District of Columbia"] -> dcvalueP3
    dc = paste0("DC -- ", as.integer(((dcvalue / dcvalueP3) -1) * 100), "%")
    
    v = 0
    if (length(md_data) > 0) {
      v = strtoi(md_data[[1]])
    }
    pmax(v, archiveData[[1]]["Maryland"], testData[[1]]["MD"],  na.rm =TRUE) -> mdvalue
    archivePre3days[[1]]["Maryland"] -> mdvalueP3
    md = paste0("MD -- ", as.integer(((mdvalue / mdvalueP3) -1) * 100), "%")
    ### mddoubleRate = 3 * log(2) * log(mdvalue / mdvalueP3)
    #md = paste0(md, ", the doubling rate is ", round(mddoubleRate,1), " days")
    
    v = 0
    if (length(va_data) > 0) {
      v = strtoi(va_data[[1]])
    }
    pmax(v, archiveData[[1]]["Virginia"], testData[[1]]["VA"],  na.rm =TRUE)-> vavalue
    archivePre3days[[1]]["Virginia"] -> vavalueP3

    va = paste0("VA --  ", as.integer(( (vavalue / vavalueP3) -1) * 100), "%")
    
    
    totalconfirmed = dcvalue + mdvalue+ vavalue
    totaldeath = pmax( archiveData[[2]]["District of Columbia"], testData[[2]]["DC"],  na.rm =TRUE)+ 
      pmax( archiveData[[2]]["Maryland"], testData[[2]]["MD"],  na.rm =TRUE) +
      pmax(archiveData[[2]]["Virginia"], testData[[2]]["VA"],  na.rm =TRUE)
    
    cmsg = paste0("In the area of DC, MD and VA, the total number of confirmed cases is  <bold><FONT COLOR='blue'> ", totalconfirmed ,
                   "</FONT></bold>", ", with <FONT COLOR='red'>", totaldeath, " </FONT> deaths.")
    msg  = "<h5>In the past 3 days, the number of confirmed cases increased by</h5>"
    msg2 = "<h4 style='color:red;'>Please stay at home to reduce the spread of covid-19 Virus.</h4>"
    content =  paste(cmsg, msg, "<ul><li>", dc, "</li><li> ", md, "</li><li> ", va, "</li></ul>", "Note: 100% means the number is doubled", msg2)
    return(content)
    
  }
  
  
  info_modal <- modalDialog(
     title = "Important message",
     
   
     HTML(informationDialog()),
     easyClose = T,
   )
  # 
  # # Show the model on start up ...
   showModal(info_modal)
  
  # pmax(NA, 1, na.rm =TRUE)
  # 
  # dc_data_display = ifelse(length(dc_data)==0, ifelse(is.na(testData["DC"]), 
  #                                                 paste0(archiveData["District of Columbia"], "*"),
  #                                                 paste0(testData["DC"], "")),
  #                          format(dc_data[[1]], big.mark=","))
  # 
  # md_data_display = ifelse(length(md_data)==0, ifelse(is.na(testData["MD"]), 
  #                                                 paste0(archiveData["Maryland"], "*"),
  #                                                 paste0(testData["MD"], "")),
  #                          format(md_data[[1]], big.mark=","))
  # print(md_data_display)
  # 
  # va_data_display = ifelse(length(va_data)==0, ifelse(is.na(testData["VA"]), 
  #                                                 paste0(archiveData["Virginia"], "*"),
  #                                                 paste0(testData["VA"], "")),
  #                          format(va_data[[1]], big.mark=","))
  # print(va_data_display)
  
  dc_data_display <- reactive({
    v = 0
    if (length(dc_data) > 0) {
      v = strtoi(dc_data[[1]])
    }
  
    pmax(v, archiveData[[1]]["District of Columbia"], testData[[1]]["DC"],  na.rm =TRUE) -> R
    
    
    return (R)
  })
  
  md_data_display <- reactive({
    v = 0
    if (length(md_data) > 0) {
      v = strtoi(md_data[[1]])
    }
    pmax(v, archiveData[[1]]["Maryland"], testData[[1]]["MD"],  na.rm =TRUE)-> R
    return (R)
  
  })
  
  va_data_display <- reactive({
    v = 0
    if (length(va_data) > 0) {
      v = strtoi(va_data[[1]])
    }
    pmax(v, archiveData[[1]]["Virginia"], testData[[1]]["VA"],  na.rm =TRUE) -> R
    return (R)
  })
  
  dc_data_display_d <- reactive({
    
    pmax( archiveData[[2]]["District of Columbia"], testData[[2]]["DC"],  na.rm =TRUE) -> R
    
    return (R)
  })
  
  md_data_display_d <- reactive({
   
    pmax( archiveData[[2]]["Maryland"], testData[[2]]["MD"],  na.rm =TRUE)-> R
    return (R)
    
  })
  
  va_data_display_d <- reactive({
   
    pmax(archiveData[[2]]["Virginia"], testData[[2]]["VA"],  na.rm =TRUE) -> R
    return (R)
  })
  
  output$vDC <- renderValueBox({
    
    value = dc_data_display()
    value = ifelse (is.na(value), "NA", value)
    dvalue = dc_data_display_d()
    print(paste("DC :" ,value))
    if (dvalue > 0)
    {
      value = paste(value, " (", dvalue, "death)")
    }
    valueBox(
      subtitle = "Washington DC",
      value = value,
      #subtitle="Washington DC",
      icon = icon("users"),  color="blue",
      href = URLs[URLs$REGION=="DC", "URL"], fill=T
    )
  })
  
  output$vMD <- renderValueBox({
    value = md_data_display()
    value = ifelse (is.na(value), "NA", value)
    print(paste("MD :" ,value))
    dvalue = md_data_display_d()
    if (dvalue > 0)
    {
      value = paste(value, " (", dvalue, "death)")
    }
    valueBox(
      value = value,
      subtitle = "Maryland",
      icon = icon("users"),  color="blue",
      href = URLs[URLs$REGION=="MD", "URL"], fill=T
    )
  })
  
  output$vVA <- renderValueBox({
    value = va_data_display()
    value = ifelse (is.na(value), "NA", value)
    print(paste("VA :" ,value))
    dvalue = va_data_display_d()
    if (dvalue > 0)
    {
      value = paste(value, " (", dvalue, "death)")
    }
    valueBox(
      value = value,
      subtitle = "Virginia",
      icon = icon("users"),  color="blue",
      href =  URLs[URLs$REGION=="VA", "URL"]
    )
  })
  createLink <- function(label, url) {
    sprintf('<a href="%s" target="_blank" >%s</a>',label, url)
    #class="btn btn-primary"
  }
  
  get_va_table_v=reactive({
    value = va_data_display()
    value = ifelse (is.na(value), "NA", as.character(value))
    
    d = data.frame("Confirmed" =value, Deaths=as.character(va_data_display_d()),
                   link=createLink(URLs[URLs$REGION=="VA", "URL"], "CDC"))
    d
  })
  
  output$VA_VALUE <- DT::renderDataTable( get_va_table_v(),class = "compact",rownames= FALSE,
                                   options = list( 
                                     dom = 't',
                                     ordering = FALSE,
                                     paging = FALSE,
                                     searching = FALSE),  escape = FALSE
  )
  #############################################
  get_md_table_v=reactive({
    value = md_data_display()
    value = ifelse (is.na(value), "NA", as.character(value))
    
    d = data.frame("Confirmed" =value, Deaths=as.character(md_data_display_d()),
                   link=createLink(URLs[URLs$REGION=="MD", "URL"], "CDC"))
    d
  })
  
  output$MD_VALUE <- DT::renderDataTable( get_md_table_v(),class = "compact",rownames= FALSE,
                                          options = list( 
                                            dom = 't',
                                            ordering = FALSE,
                                            paging = FALSE,
                                            searching = FALSE),  escape = FALSE
  )
  ###############################
  get_dc_table_v=reactive({
    value = dc_data_display()
    value = ifelse (is.na(value), "NA", as.character(value))
    
    d = data.frame("Confirmed" =value,  Deaths=as.character(dc_data_display_d()),
                   link=createLink(URLs[URLs$REGION=="DC", "URL"], "CDC"))
    d
  })
  
  output$DC_VALUE <- DT::renderDataTable( get_dc_table_v(),class = "compact",rownames= FALSE,
                                          options = list( 
                                            dom = 't',
                                            ordering = FALSE,
                                            paging = FALSE,
                                            searching = FALSE),  escape = FALSE
  )
  
  
  china_data0 = reactive({
    d = china_data
    d$link=createLink(URLs[URLs$REGION=="CHINA", "URL"], "CDC")
    d$Date <- NULL
    d
  })
  
  output$chinaCDC0 <- DT::renderDataTable(

      china_data0(), class = "compact",rownames= FALSE,
      options = list(
        dom = 't',
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE),  escape = FALSE
    )
  
}


# Run the application 
#shinyApp(ui = ui, server = server) 

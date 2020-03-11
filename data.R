library(httr)
library(tidyr)
library(reshape2)
library(dplyr)
   
   
get_daily_data <- function()
{
   today = Sys.Date()
   yesterday = today - 1
   todayStr <- format(today, "%m-%d-%Y")
   yesterdayStr <- format(yesterday, "%m-%d-%Y")
   hitbub_root = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports"
   todayPath <- file.path(hitbub_root, paste0(todayStr, ".csv"))
   yesPath   <- file.path(hitbub_root, paste0(yesterdayStr, ".csv")  )
   thedate = today
   a <- NA
   
   if (Sys.time()> strptime("10:00 PM", "%I:%M %p") ) {   #OK, now today after 10pm, try to get today's update
      a <- tryCatch(
        {
           read.csv(todayPath, header=T, stringsAsFactors = F)
        },
        error=function(cond) {
           return (NA)
        },
        warning=function(cond) {
           return (NA)
        }
     )
   }
   
   if (is.na(a))
   {
      a <- tryCatch(
         {
            read.csv(yesPath, header=T, stringsAsFactors = F)
         },
         error=function(cond) {
            return (NA)
         },
         warning=function(cond) {
            return (NA)
         }
      )
      thedate = yesterday
   }
   return (list(date=thedate, data=a))                      
}



get_daily_data_US <- function(all_daily_data)
{
   all_daily_data %>% filter(Country.Region == "US") -> USData
   
   USDataTotle <- USData %>% summarise(Confirmed = sum(Confirmed), 
                                       Deaths= sum(Deaths), 
                                       Recovered= sum(Recovered), 
                                       Updated = max(Last.Update)) %>% mutate(State = "ALL") %>% 
                               select(State,  Confirmed, Deaths, Recovered, Updated)
   bind_rows(USDataTotle,
             USData %>% select(State=Province.State,Confirmed, Deaths, Recovered, Updated = Last.Update)
   ) -> USData
   rownames(USData) <- USData$State
   USData$State <- NULL
   USData$Updated = sapply(USData$Updated,  function(x) paste(format(as.Date(x), "%m-%d"), strsplit(x, split="T")[[1]][2]))
   return (USData)
}
#  get_daily_data_US(get_daily_data()[[2]]) ->A


get_daily_data_world <- function(all_daily_data)
{
   world <- all_daily_data %>% summarise(Confirmed = sum(Confirmed), 
                                Deaths= sum(Deaths), 
                                Recovered= sum(Recovered), 
                                Updated = max(Last.Update)) %>% 
                  mutate(Region = "World") %>% select(Region,  Confirmed, Deaths, Recovered, Updated)
  
   all_daily_data %>% group_by(Country.Region) %>% summarise(Confirmed = sum(Confirmed), 
                                                          Deaths= sum(Deaths), 
                                                          Recovered= sum(Recovered), 
                                                          Updated = max(Last.Update)) %>% 
                                                select(Region= Country.Region,  Confirmed, Deaths, Recovered, Updated) -> aggregated
   #print(lastdata)
   mt <- aggregated[order(aggregated$Confirmed,decreasing = T ), ]
   china <- mt %>% filter(Region == "Mainland China")
   canada <- mt %>% filter(Region == "Canada")
   top5 <- mt %>% filter(!(Region %in% c("Canada", "Mainland China", "US")))
   top5 <- top5[1:5,]
   world = bind_rows(world, china, canada, top5)
   
   rownames(world) <- world$Region
   world$Region <- NULL
   world$Updated = sapply(world$Updated,  function(x) paste(format(as.Date(x), "%m-%d"), strsplit(x, split="T")[[1]][2]))
   return (world)
   
}

#  get_daily_data_world(get_daily_data()[[2]])


get_init_data <- function()
{
   hitbub_root <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
   github_pathConfirm <- file.path(hitbub_root, "time_series_19-covid-Confirmed.csv")
   github_pathDeaths  <- file.path(hitbub_root, "time_series_19-covid-Deaths.csv")
   github_pathRecovered  <- file.path(hitbub_root, "time_series_19-covid-Recovered.csv")
   
   c <- read.csv(github_pathConfirm, header=T, stringsAsFactors = F)
   d <- read.csv(github_pathDeaths, header=T, stringsAsFactors = F)
   r <- read.csv(github_pathRecovered, header=T, stringsAsFactors = F)
   
   get_convert_wide_to_long <- function(df)
   {
     # Convert data from wide to long format
     #New Format will be 
     #    Province.State, Country.Region, Lat, Long, Date, Value
     data_long <- melt(df, id.vars=c("Province.State", "Country.Region", "Lat", "Long"))
     data_long$date0 <- substr(as.character(data_long$variable), 2, 8)
     data_long$date <- as.Date(data_long$date0, '%m.%d.%y')
     data_long$variable <- NULL
     names(data_long)[names(data_long)=="Province.State"] <- "region"
     names(data_long)[names(data_long)=="Country.Region"] <- "country"
     
     return (data_long[!is.na(data_long$value) , ])
   }
   c = get_convert_wide_to_long(c)
   d = get_convert_wide_to_long(d)
   r = get_convert_wide_to_long(r)
   lastDate = max(c$date)

   
   return (list(lastdate= lastDate, confirm = c,death = d, recovered = r))
   
}
get_init_data()[[2]] -> AAA

get_last_value <- function(data0, country, date0)
{
   lastdata = data0[data0$date==date0, ]
   if (country != "World")
   {
      lastdata = lastdata[lastdata$country==country, ]
   }
   r = lastdata  %>% summarise(total = sum(value))
   
   return ( r[1,1])
   
}

get_map_data <- function(country,data0, date0)
{
   lastdata = data0[data0$date==date0, ]
   if (country != "World")
   {
      lastdata = lastdata[lastdata$country==country, ]
   }
   
   lastdata <- mutate(lastdata, content=paste0('<strong>confirmed: </strong>',value,
                                          '<br><strong>Country:</strong> ', country,
                                          '<br><strong>region:</strong> ',region)) 
   
   return (lastdata)
}
#sep = "<br/>",
get_time_series <- function(data0, country)
{
   #data0 <- CCC
   firstDate = min(data0$date)
   lastDate =  max(data0$date)
   if (country != "World")
   {
      data0 = data0[data0$country==country, ]
   }
   dDDD <- data0 %>% group_by(date) %>% summarise(value = sum(value)) %>%
      mutate(prev_value = lag(value, order_by=date)) %>%
      mutate(new_value = value -  prev_value) %>% filter(!is.na(prev_value))
   return (dDDD)
}

get_top_country <- function(data0)
{
   date0 =  max(data0$date)
   lastdata = data0[data0$date==date0, ]
   lastdata <- lastdata %>% group_by(country) %>% summarise(value = sum(value))
   #print(lastdata)
   mt <- lastdata[order(lastdata$value,decreasing = T ), ]
   return (paste(mt$country[1:30], "-",  mt$value[1:30]))
   
}
library(dplyr)
library(rvest)
library(gdata)

get_State_CDC_URL <- function()
{
   str <- "REGION,URL
DC,https://coronavirus.dc.gov/page/coronavirus-data
MD,https://phpa.health.maryland.gov/Pages/Novel-coronavirus.aspx
VA,http://www.vdh.virginia.gov/surveillance-and-investigation/novel-coronavirus/
CHINA,http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm"
   return (read.csv(text=str, stringsAsFactors = F))
}

get_DC <- function(DC_URL)
{
  
  tryCatch(
  {
   raw_data <- xml2::read_html(DC_URL)
   data <- raw_data %>% html_nodes("p")
   print(xml_text(data[2]))
   updated_DateTime <- xml_text(data)
   
   contents <- raw_data %>% html_nodes("ul") %>%  html_nodes("li") %>% html_text(trim = TRUE)
   line = NA
   for (i in seq_along(contents))
   {
      if (gdata::startsWith(contents[i], "Number of")) {
         if (is.na(line))
         {
            line = contents[i]
         }
         else
         {
            line = paste(line, "\n", contents[i])
         }
         newline = contents[i]
         if (grepl('positive|Positive', newline))
         {
            positiveCount = trim( strsplit(newline, split=":")[[1]][2])
            #print(positiveCount)
         }
      }
   }
   #return (read.csv(text=line, header=F, sep= ":", col.names = c("category", "count"), stringsAsFactors=FALSE))
   return (list(pvalue=positiveCount, 
                rawdata = read.csv(text=line, header=F, sep= ":", 
                                   col.names = c("category", "value"), 
                                   colClasses = c("character",  "integer"), 
                                   stringsAsFactors=FALSE)))
  },
  error=function(cond) {
     return (NA)
  },
  warning=function(cond) {
     return (NA)
  })
}
#   get_DC()

#################

get_MD <- function(MD_URL)
{
   tryCatch(
      {
   #MD_URL = "https://phpa.health.maryland.gov/Pages/Novel-coronavirus.aspx"
   raw_data <- xml2::read_html(MD_URL)
   
   contents <- raw_data %>% html_nodes("div") 

   line = NA
   positiveCount = 0
   for (i in seq_along(contents))
   {
      if (gdata::startsWith(html_text(contents[i]), "Number of")) {
         if (is.na(line))
         {
            line = html_text(contents[i], trim = TRUE)
         }
         else
         {
            line = paste(line, "\n", html_text(contents[i], trim = TRUE))
         }
         newline = html_text(contents[i], trim = TRUE)
         if (grepl('positive|Positive', newline))
         {
            positiveCount = trim( strsplit(newline, split=":")[[1]][2])
            #print(positiveCount)
         }
      }
   }
   return (list(pvalue=positiveCount, 
                rawdata = read.csv(text=line, header=F, sep= ":", 
                                   col.names = c("category", "value"), 
                                   colClasses = c("character",  "integer"), 
                                   stringsAsFactors=FALSE)))
      },
   error=function(cond) {
      return (NA)
   },
   warning=function(cond) {
      return (NA)
   })
  # return (read.csv(text=line, header=F, sep= ":", col.names = c("category", "value"), stringsAsFactors=FALSE))
}
#   get_MD("https://phpa.health.maryland.gov/Pages/Novel-coronavirus.aspx")


get_VA <- function(VA_URL)
{
   tryCatch(
      {
   #VA_URL = "http://www.vdh.virginia.gov/surveillance-and-investigation/novel-coronavirus/"
   #VA_URL="https://vdhdata.vdh.virginia.gov/views/COVID-19inVirginia/External?:embed=y&amp;:isGuestRedirectFromVizportal=y&amp;:display_count=no&amp;:showVizHome=no&amp;:toolbar=yes"
   raw_data <- xml2::read_html(VA_URL)
   contents <- raw_data %>% html_nodes("p") 
   line = NA
   positiveCount = 0
   for (i in seq_along(contents))
   {
      if (gdata::startsWith(html_text(contents[i]), "Number of")) {
         if (is.na(line))
         {
            line = html_text(contents[i], trim = TRUE)
         }
         else
         {
            line = paste(line, "\n", html_text(contents[i], trim = TRUE))
         }
         newline = html_text(contents[i], trim = TRUE)
         if (grepl('positive|Positive', newline))
         {
            positiveCount = trim( strsplit(newline, split=":")[[1]][2])
            #print(positiveCount)
         }

      }
   }
   return (list(pvalue=positiveCount, 
                rawdata = read.csv(text=line, header=F, sep= ":", 
                                   col.names = c("category", "value"), 
                                   colClasses = c("character",  "integer"), 
                                   stringsAsFactors=FALSE)))
},
error=function(cond) {
   return (NA)
},
warning=function(cond) {
   return (NA)
})
}
#VA_URL = "http://www.vdh.virginia.gov/surveillance-and-investigation/novel-coronavirus/"
#get_VA(VA_URL)[[2]] ->a

get_China0 <- function()
{
   China_URL = "http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm"
   raw_data <- xml2::read_html(China_URL)
   
   contents <- raw_data %>% html_nodes("p") 
   found = F
   line = NA
   for (i in seq_along(contents))
   {
      if (gdata::startsWith(html_text(contents[i]), "National Health Commission Update"))
      {
         if (found == F)
         {
            found <- T
            next
         }
         else 
         {
            break
         }
      } 
      if (found == T) {
         if (is.na(line))
         {
            line = paste0("Date :", html_text(contents[i], trim = TRUE))
         }
         else
         {
            line = paste(line, "\n", html_text(contents[i], trim = TRUE))
         }
      }
   }
   line = gsub("\\.", "\n", line)
   
 
   data = read.csv(text=line, header=F, sep= ":",  col.names = c("category", "value"), stringsAsFactors=FALSE, strip.white = T)
   data %>% dplyr::filter(!(category==" ")) %>% dplyr::filter(!(is.na(category))) 
}

#get_China0()

get_China <- function()
{
   china_data_cache = "china.rds"
   if(!file.exists(china_data_cache)){
      data = get_China0()
      saveRDS(data, file = china_data_cache)
      return (data)
   } else
   {
      finf = file.info(china_data_cache)
      if (difftime(Sys.time(), finf[,"mtime"], units = "hours") > 1)
      {
         print("redo")
         data = get_China0()
         saveRDS(data, file = china_data_cache)
         return (data)
      }
      else 
      {
         print("use cache")
         return (readRDS(china_data_cache))
      }
   }
}


#  get_China()

#ccc = get_top_country(data)


#head(dataC$date, 1)

#get_last_value(data$confirm, data$lastdate)

#data <- get_init_data()$confirm
#A <- data$confirm

#get_map_data(country="World", data0=data, date0 = lastDate)
#library(googlesheets4)
#get_google <- function()
#{
#   a =read_sheet("https://docs.google.com/spreadsheets/d/1vAr_WyP6ljZljM2g8puSN23X8spJV2m2Ya5PJ5yGZyk/edit?usp=sharing", )

#   return (a)
#}


#get_VAgoogle <- function()
#{
#   a =read_sheet("https://docs.google.com/spreadsheets/d/1vAr_WyP6ljZljM2g8puSN23X8spJV2m2Ya5PJ5yGZyk/edit?usp=sharing")
#   a %>% filter(region == "VA") %>% select (category, value) -> b
#   return (b)
#}

### get_VA()



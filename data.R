library(httr)
library(tidyr)
library(reshape2)
library(dplyr)


get_init_data <- function()
{
   hitbub_root <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
   github_pathConfirm <- file.path(hitbub_root, "time_series_19-covid-Confirmed.csv")
   github_pathDeaths  <- file.path(hitbub_root, "time_series_19-covid-Deaths.csv")
   github_pathRecovered  <- file.path(hitbub_root, "time_series_19-covid-Recovered.csv")
   
   c <- read.csv(github_path, header=T, stringsAsFactors = F)
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
#ccc = get_top_country(data)


#head(dataC$date, 1)

#get_last_value(data$confirm, data$lastdate)

#data <- get_init_data()$confirm
#A <- data$confirm

#get_map_data(country="World", data0=data, date0 = lastDate)



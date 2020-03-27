# Dashboard for coronavirus update

These applications provide real time status of COVID-19 pandemic around world.
 
  * (Local View) : https://dxdinfo.shinyapps.io/DMV_Corona/ (pulling data from CDC sites of DC, MD and VA)
  * (Global View): https://dxdinfo.shinyapps.io/Coronavirus_stat/     (pulling data from John Hopkins)
  * (Growth Rate after 100 cases): https://dxdinfo.shinyapps.io/COVID_Model/
 
**Data Source** *(Johns Hopkins - Whiting School of Engineering)*: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series 

The data source is updated daily.

In here, we use the R shiny technology to present data for a tutorial purpose.

## Steps:
### 1. Data Import from github

```{r}

 hitbub_root <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
   github_pathConfirm <- file.path(hitbub_root, "time_series_19-covid-Confirmed.csv")
   github_pathDeaths  <- file.path(hitbub_root, "time_series_19-covid-Deaths.csv")
   github_pathRecovered  <- file.path(hitbub_root, "time_series_19-covid-Recovered.csv")
   
   c <- read.csv(github_path, header=T, stringsAsFactors = F)
   d <- read.csv(github_pathDeaths, header=T, stringsAsFactors = F)
   r <- read.csv(github_pathRecovered, header=T, stringsAsFactors = F)
```
### 2. Visualization
#### 2.1 Total confirm cases, Total deaths, Total Recovered, at last reported date (maybe 1 day delay)
#### 2.2 Time series of three matrices
#### 2.3 use embedded Lat/Lon to present data in map.

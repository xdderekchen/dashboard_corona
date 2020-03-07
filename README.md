# Dashboard for coronavirus update

Data Source (Johns Hopkins - Whiting School of Engineering): https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series 
The data source is updated daily.

In here, we use the R shiny technology to present data for a tutorial purpose.

## Steps:
### 1. Data Imports from github

'''{r}
library(httr)

github_path <-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
data <- read.csv(text=GET(github_path), skip=7, header=T)
'''{r}

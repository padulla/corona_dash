

library (RCurl)
library('magrittr')



download <- RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

#dim(data) = 254 73
data <- read.csv(text = download, 
                 stringsAsFactors = FALSE, 
                 colClasses = c(rep('character', 4), rep('numeric', 73-4)))

dates <- colnames(data)[-c(1:4)] %>% 
  strsplit(., 'X') %>% 
  lapply(., '[', 2) %>% 
  unlist() %>% 
  as.Date(format = "%m.%d.%y")


DataColNames <- colnames(data)[1:4]
colnames(data) <- c(DataColNames, format(dates, '%m/%d/%y'))

ncol(data)
da
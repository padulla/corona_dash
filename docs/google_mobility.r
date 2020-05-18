#Google Mobily



library(tidyverse)
library(plotly)
library(xlsx)


RepoGoogle <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=57b4ac4fc40528e2"





RawGoogle <- readr::read_csv(RepoGoogle,
                             col_types = paste0('cccc','D',  strrep('d', 6))
)


BdBr <- filter(RawGoogle, country_region_code=="BR")





BrCodes <-read_csv(file = "C:/Users/Padulla/Documents/GitHub/corona_dash/Br_dic_states.csv"
                    ,col_names = TRUE
                    ,col_types = 'cc')


SelectedBR <- c("Brasil"
                ,"SP"
                ,"RJ"
                ,"CE"
                ,"MG"
                ,"RS"
                ,"DF"
                ,"PR"
                ,"BA"
                ,"AM"
                ,"SC"
                ,"RN"
                ,"PE"
                ,"ES"
                ,"MA"
                ,"GO"
                ,"PA"
                ,"MS"
                ,"MT"
                ,"AC"
                ,"SE"
                ,"AL"
                ,"TO"
                ,"RO"
                ,"AP"
                ,"PB"
                ,"PI"
                ,"RR")









BdBrRetail <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brasil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, retail_and_recreation_percent_change_from_baseline) %>%
  tidyr::spread(date,retail_and_recreation_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))
  


BdBrGrocery <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brasil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, grocery_and_pharmacy_percent_change_from_baseline) %>%
  tidyr::spread(date,grocery_and_pharmacy_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BdBrParks <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brasil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, parks_percent_change_from_baseline) %>%
  tidyr::spread(date,parks_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BdBrTransit <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brasil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, transit_stations_percent_change_from_baseline) %>%
  tidyr::spread(date,transit_stations_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BdBrWork <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brasil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, workplaces_percent_change_from_baseline) %>%
  tidyr::spread(date,workplaces_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BdBrResidential <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brasil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, residential_percent_change_from_baseline) %>%
  tidyr::spread(date,residential_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))



















m <-  as.matrix(BdBrRetail)
p <- plot_ly(x=colnames(m), y=rownames(m), z = m, type = "heatmap") %>%
     layout(margin = list(l=120))
 p





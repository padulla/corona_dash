#Google Mobily



library(tidyverse)
library(plotly)
library(xlsx)


RepoGoogle <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=57b4ac4fc40528e2"





RawGoogle <- readr::read_csv(RepoGoogle,
                             col_types = paste0('cccccc','D',  strrep('d', 6))
)


BdBr <- filter(RawGoogle, country_region_code=="BR")



BrPesosPib <-read.xlsx(file = "C:/Users/leandro/Documents/GitHub/corona_dash/docs/pesos_pib.xlsx"
                   , sheetName = "base"
                   ) 







BrCodes <-read_csv(file = "C:/Users/leandro/Documents/GitHub/corona_dash/Br_dic_states.csv"
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
  replace_na(list(sub_region_1 = "Brazil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, retail_and_recreation_percent_change_from_baseline) %>%
  tidyr::spread(date,retail_and_recreation_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))
  


BdBrGrocery <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brazil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, grocery_and_pharmacy_percent_change_from_baseline) %>%
  tidyr::spread(date,grocery_and_pharmacy_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BdBrParks <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brazil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, parks_percent_change_from_baseline) %>%
  tidyr::spread(date,parks_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BdBrTransit <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brazil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, transit_stations_percent_change_from_baseline) %>%
  tidyr::spread(date,transit_stations_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BdBrWork <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brazil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, workplaces_percent_change_from_baseline) %>%
  tidyr::spread(date,workplaces_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BdBrResidential <- BdBr %>% 
  replace_na(list(sub_region_1 = "Brazil")) %>%
  left_join(BrCodes) %>%
  select(date, Short, residential_percent_change_from_baseline) %>%
  tidyr::spread(date,residential_percent_change_from_baseline, fill = 0) %>%
  tibble::column_to_rownames(var = "Short") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))


lista_gg <- list(BdBrRetail,BdBrGrocery,BdBrParks,BdBrTransit,BdBrWork,BdBrResidential)


df_gg2 <- map(lista_gg,~ {
  y <- .x %>% 
  rownames_to_column(var="States") %>% 
  mutate_at(vars("States"),as.factor) %>% 
  left_join(BrPesosPib,by="States") %>% 
  mutate_at(vars(-c("part.","States")),~ part. * .x) %>% 
  filter(States!="BR") 

soma_linhas <- y %>%
  select(-c("States","part.")) %>% 
  colSums %>%
  setNames(colnames(y)[-c(1,length(y))])


z <- bind_rows(y,soma_linhas) %>% 
  replace_na(list(States="BR")) %>% 
  select(-"part.") %>% 
  column_to_rownames("States")
}
) %>% setNames(c("BdBrRetail_f","BdBrGrocery_f","BdBrParks_f","BdBrTransit_f","BdBrWork_f","BdBrResidential_f"))


list2env(df_gg2, globalenv())



write.xlsx(BdBrRetail_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrRetail_f")
write.xlsx(BdBrGrocery_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrGrocery_f",append=TRUE)
write.xlsx(BdBrParks_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrParks_f",append=TRUE)
write.xlsx(BdBrTransit_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrTransit_f",append=TRUE)
write.xlsx(BdBrWork_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrWork_f",append=TRUE)
write.xlsx(BdBrResidential_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrResidential_f",append=TRUE)
write.xlsx(BdBrRetail, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrRetail",append=TRUE)









m <-  as.matrix(BdBrRetail)
p <- plot_ly(x=colnames(m), y=rownames(m), z = m, type = "heatmap") %>%
     layout(margin = list(l=120))
 p





 
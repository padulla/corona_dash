#Google Mobily



library(tidyverse)
library(plotly)
library(xlsx)


# Endereço do Relatório #

RepoGoogle <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=57b4ac4fc40528e2"




# Importa e transforma em uma tabela #

RawGoogle <- readr::read_csv(RepoGoogle,
                             col_types = paste0('ccccccc','D',  strrep('d', 6))
)

BdBr <- filter(RawGoogle, country_region_code=="BR")

# Cria as bases para os países #

# Brasil #

BdBR <- filter(RawGoogle, country_region_code=="BR") %>% 
  replace_na(list(sub_region_1 = "BR")) %>% 
  filter(sub_region_1=="BR") 
  

# US #

BdUS <-filter(RawGoogle, country_region_code=="US") %>% 
  replace_na(list(sub_region_1 = "US")) %>% 
  filter(sub_region_1=="US")


# Alemanha #

BdDE <-filter(RawGoogle, country_region_code=="DE")%>% 
  replace_na(list(sub_region_1 = "DE")) %>% 
  filter(sub_region_1=="DE")

# França #

BdFR <-filter(RawGoogle, country_region_code=="FR")%>% 
  replace_na(list(sub_region_1 = "FR")) %>% 
  filter(sub_region_1=="FR")

# Espanha #

BdES <-filter(RawGoogle, country_region_code=="ES")%>% 
  replace_na(list(sub_region_1 = "ES")) %>% 
  filter(sub_region_1=="ES")

# Itália #

BdIT <-filter(RawGoogle, country_region_code=="IT")%>% 
  replace_na(list(sub_region_1 = "IT"))%>% 
  filter(sub_region_1=="IT")

# UK #

BdGB <-filter(RawGoogle, country_region_code=="GB")%>% 
  replace_na(list(sub_region_1 = "GB"))%>% 
  filter(sub_region_1=="GB")


# AU #

BdAU <-filter(RawGoogle, country_region_code=="AU")%>% 
  replace_na(list(sub_region_1 = "AU"))%>% 
  filter(sub_region_1=="AU")

# NO #

BdNO <-filter(RawGoogle, country_region_code=="NO")%>% 
  replace_na(list(sub_region_1 = "NO"))%>% 
  filter(sub_region_1=="NO")















# Retail


retail_com <- cbind(BdBR['retail_and_recreation_percent_change_from_baseline'],
                    BdUS['retail_and_recreation_percent_change_from_baseline'],
                    BdDE['retail_and_recreation_percent_change_from_baseline'],
                    BdFR['retail_and_recreation_percent_change_from_baseline'],
                    BdES['retail_and_recreation_percent_change_from_baseline'],
                    BdIT['retail_and_recreation_percent_change_from_baseline'],
                    BdGB['retail_and_recreation_percent_change_from_baseline'],
                    BdAU['retail_and_recreation_percent_change_from_baseline'],
                    BdNO['retail_and_recreation_percent_change_from_baseline'])

names(retail_com)[1] <- "BR"
names(retail_com)[2] <- "US"
names(retail_com)[3] <- "DE"
names(retail_com)[4] <- "FR"
names(retail_com)[5] <- "ES"
names(retail_com)[6] <- "IT"
names(retail_com)[7] <- "GB"
names(retail_com)[8] <- "AU"
names(retail_com)[9] <- "NO"


retail_com_mm7d <- 
  do.call(cbind, lapply(colnames(retail_com)[1:9], function(x) TTR::SMA(retail_com[,x], 7))) %>% 
  as_tibble %>% 
  rename_all(~ colnames(retail_com)[1:9])

retail_com_mm7d <-  cbind(BdBR['date'],retail_com_mm7d) 


# Grocery

grocery_com <- cbind(BdBR['grocery_and_pharmacy_percent_change_from_baseline'],
                    BdUS['grocery_and_pharmacy_percent_change_from_baseline'],
                    BdDE['grocery_and_pharmacy_percent_change_from_baseline'],
                    BdFR['grocery_and_pharmacy_percent_change_from_baseline'],
                    BdES['grocery_and_pharmacy_percent_change_from_baseline'],
                    BdIT['grocery_and_pharmacy_percent_change_from_baseline'],
                    BdGB['grocery_and_pharmacy_percent_change_from_baseline'],
                    BdAU['grocery_and_pharmacy_percent_change_from_baseline'],
                    BdNO['grocery_and_pharmacy_percent_change_from_baseline']) 


names(grocery_com)[1] <- "BR"
names(grocery_com)[2] <- "US"
names(grocery_com)[3] <- "DE"
names(grocery_com)[4] <- "FR"
names(grocery_com)[5] <- "ES"
names(grocery_com)[6] <- "IT"
names(grocery_com)[7] <- "GB"
names(grocery_com)[8] <- "AU"
names(grocery_com)[9] <- "NO"


grocery_com <- grocery_com %>% mutate_all(~replace(., is.na(.), 0))

grocery_com_mm7d <- 
  do.call(cbind, lapply(colnames(grocery_com)[1:9], function(x) TTR::SMA(grocery_com[,x], 7))) %>% 
  as_tibble %>% 
  rename_all(~ colnames(grocery_com)[1:9])

grocery_com_mm7d <-  cbind(BdBR['date'],grocery_com_mm7d) 


# parks

parks_com <- cbind(BdBR['parks_percent_change_from_baseline'],
                     BdUS['parks_percent_change_from_baseline'],
                     BdDE['parks_percent_change_from_baseline'],
                     BdFR['parks_percent_change_from_baseline'],
                     BdES['parks_percent_change_from_baseline'],
                     BdIT['parks_percent_change_from_baseline'],
                     BdGB['parks_percent_change_from_baseline'],
                     BdAU['parks_percent_change_from_baseline'],
                     BdNO['parks_percent_change_from_baseline']) 


names(parks_com)[1] <- "BR"
names(parks_com)[2] <- "US"
names(parks_com)[3] <- "DE"
names(parks_com)[4] <- "FR"
names(parks_com)[5] <- "ES"
names(parks_com)[6] <- "IT"
names(parks_com)[7] <- "GB"
names(parks_com)[8] <- "AU"
names(parks_com)[9] <- "NO"


parks_com_mm7d <- 
  do.call(cbind, lapply(colnames(parks_com)[1:9], function(x) TTR::SMA(parks_com[,x], 7))) %>% 
  as_tibble %>% 
  rename_all(~ colnames(parks_com)[1:9])

parks_com_mm7d <-  cbind(BdBR['date'],parks_com_mm7d) 


# transit

transit_com <- cbind(BdBR['transit_stations_percent_change_from_baseline'],
                   BdUS['transit_stations_percent_change_from_baseline'],
                   BdDE['transit_stations_percent_change_from_baseline'],
                   BdFR['transit_stations_percent_change_from_baseline'],
                   BdES['transit_stations_percent_change_from_baseline'],
                   BdIT['transit_stations_percent_change_from_baseline'],
                   BdGB['transit_stations_percent_change_from_baseline'],
                   BdAU['transit_stations_percent_change_from_baseline'],
                   BdNO['transit_stations_percent_change_from_baseline']) 


names(transit_com)[1] <- "BR"
names(transit_com)[2] <- "US"
names(transit_com)[3] <- "DE"
names(transit_com)[4] <- "FR"
names(transit_com)[5] <- "ES"
names(transit_com)[6] <- "IT"
names(transit_com)[7] <- "GB"
names(transit_com)[8] <- "AU"
names(transit_com)[9] <- "NO"




transit_com_mm7d <- 
  do.call(cbind, lapply(colnames(transit_com)[1:9], function(x) TTR::SMA(transit_com[,x], 7))) %>% 
  as_tibble %>% 
  rename_all(~ colnames(transit_com)[1:9])

transit_com_mm7d <-  cbind(BdBR['date'],transit_com_mm7d) 


# workplace

workplace_com <- cbind(BdBR['workplaces_percent_change_from_baseline'],
                     BdUS['workplaces_percent_change_from_baseline'],
                     BdDE['workplaces_percent_change_from_baseline'],
                     BdFR['workplaces_percent_change_from_baseline'],
                     BdES['workplaces_percent_change_from_baseline'],
                     BdIT['workplaces_percent_change_from_baseline'],
                     BdGB['workplaces_percent_change_from_baseline'],
                     BdAU['workplaces_percent_change_from_baseline'],
                     BdNO['workplaces_percent_change_from_baseline']) 


names(workplace_com)[1] <- "BR"
names(workplace_com)[2] <- "US"
names(workplace_com)[3] <- "DE"
names(workplace_com)[4] <- "FR"
names(workplace_com)[5] <- "ES"
names(workplace_com)[6] <- "IT"
names(workplace_com)[7] <- "GB"
names(workplace_com)[8] <- "AU"
names(workplace_com)[9] <- "NO"


workplace_com_mm7d <- 
  do.call(cbind, lapply(colnames(workplace_com)[1:9], function(x) TTR::SMA(workplace_com[,x], 7))) %>% 
  as_tibble %>% 
  rename_all(~ colnames(workplace_com)[1:9])

workplace_com_mm7d <-  cbind(BdBR['date'],workplace_com_mm7d) 



# residential

residential_com <- cbind(BdBR['residential_percent_change_from_baseline'],
                       BdUS['residential_percent_change_from_baseline'],
                       BdDE['residential_percent_change_from_baseline'],
                       BdFR['residential_percent_change_from_baseline'],
                       BdES['residential_percent_change_from_baseline'],
                       BdIT['residential_percent_change_from_baseline'],
                       BdGB['residential_percent_change_from_baseline'],
                       BdAU['residential_percent_change_from_baseline'],
                       BdNO['residential_percent_change_from_baseline']) 


names(residential_com)[1] <- "BR"
names(residential_com)[2] <- "US"
names(residential_com)[3] <- "DE"
names(residential_com)[4] <- "FR"
names(residential_com)[5] <- "ES"
names(residential_com)[6] <- "IT"
names(residential_com)[7] <- "GB"
names(residential_com)[8] <- "AU"
names(residential_com)[9] <- "NO"


residential_com_mm7d <- 
  do.call(cbind, lapply(colnames(residential_com)[1:9], function(x) TTR::SMA(residential_com[,x], 7))) %>% 
  as_tibble %>% 
  rename_all(~ colnames(residential_com)[1:9])

residential_com_mm7d <-  cbind(BdBR['date'],residential_com_mm7d) 













# fazer os fráficos individual para eua



# 
# 
# 
# 
# write.csv(BdUs, 'T:\\ECONOMIA\\Corona\\excel_base_google.csv')
# 
# 
# BrPesosPib <-read.xlsx(file = "C:/Users/leandro/Documents/GitHub/corona_dash/docs/pesos_pib.xlsx"
#                    , sheetName = "base"
#                    )
# 
# 
# 
# 
# 
# 
# 
# BrCodes <-read_csv(file = "C:/Users/leandro/Documents/GitHub/corona_dash/Br_dic_states.csv"
#                     ,col_names = TRUE
#                     ,col_types = 'cc')
# 
# 
# SelectedBR <- c("Brasil"
#                 ,"SP"
#                 ,"RJ"
#                 ,"CE"
#                 ,"MG"
#                 ,"RS"
#                 ,"DF"
#                 ,"PR"
#                 ,"BA"
#                 ,"AM"
#                 ,"SC"
#                 ,"RN"
#                 ,"PE"
#                 ,"ES"
#                 ,"MA"
#                 ,"GO"
#                 ,"PA"
#                 ,"MS"
#                 ,"MT"
#                 ,"AC"
#                 ,"SE"
#                 ,"AL"
#                 ,"TO"
#                 ,"RO"
#                 ,"AP"
#                 ,"PB"
#                 ,"PI"
#                 ,"RR")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# BdBrRetail <- BdBr %>%
#   replace_na(list(sub_region_1 = "Brazil")) %>%
#   left_join(BrCodes) %>%
#   select(date, Short, retail_and_recreation_percent_change_from_baseline) %>%
#   tidyr::spread(date,retail_and_recreation_percent_change_from_baseline, fill = 0) %>%
#   tibble::column_to_rownames(var = "Short") %>%
#   rename_all(function(x) format(as.Date(x), "%m/%d/%y"))
# 
# 
# 
# BdBrGrocery <- BdBr %>%
#   replace_na(list(sub_region_1 = "Brazil")) %>%
#   left_join(BrCodes) %>%
#   select(date, Short, grocery_and_pharmacy_percent_change_from_baseline) %>%
#   tidyr::spread(date,grocery_and_pharmacy_percent_change_from_baseline, fill = 0) %>%
#   tibble::column_to_rownames(var = "Short") %>%
#   rename_all(function(x) format(as.Date(x), "%m/%d/%y"))
# 
# BdBrParks <- BdBr %>%
#   replace_na(list(sub_region_1 = "Brazil")) %>%
#   left_join(BrCodes) %>%
#   select(date, Short, parks_percent_change_from_baseline) %>%
#   tidyr::spread(date,parks_percent_change_from_baseline, fill = 0) %>%
#   tibble::column_to_rownames(var = "Short") %>%
#   rename_all(function(x) format(as.Date(x), "%m/%d/%y"))
# 
# BdBrTransit <- BdBr %>%
#   replace_na(list(sub_region_1 = "Brazil")) %>%
#   left_join(BrCodes) %>%
#   select(date, Short, transit_stations_percent_change_from_baseline) %>%
#   tidyr::spread(date,transit_stations_percent_change_from_baseline, fill = 0) %>%
#   tibble::column_to_rownames(var = "Short") %>%
#   rename_all(function(x) format(as.Date(x), "%m/%d/%y"))
# 
# BdBrWork <- BdBr %>%
#   replace_na(list(sub_region_1 = "Brazil")) %>%
#   left_join(BrCodes) %>%
#   select(date, Short, workplaces_percent_change_from_baseline) %>%
#   tidyr::spread(date,workplaces_percent_change_from_baseline, fill = 0) %>%
#   tibble::column_to_rownames(var = "Short") %>%
#   rename_all(function(x) format(as.Date(x), "%m/%d/%y"))
# 
# BdBrResidential <- BdBr %>%
#   replace_na(list(sub_region_1 = "Brazil")) %>%
#   left_join(BrCodes) %>%
#   select(date, Short, residential_percent_change_from_baseline) %>%
#   tidyr::spread(date,residential_percent_change_from_baseline, fill = 0) %>%
#   tibble::column_to_rownames(var = "Short") %>%
#   rename_all(function(x) format(as.Date(x), "%m/%d/%y"))
# 
# 
# lista_gg <- list(BdBrRetail,BdBrGrocery,BdBrParks,BdBrTransit,BdBrWork,BdBrResidential)
# 
# 
# df_gg2 <- map(lista_gg,~ {
#   y <- .x %>%
#   rownames_to_column(var="States") %>%
#   mutate_at(vars("States"),as.factor) %>%
#   left_join(BrPesosPib,by="States") %>%
#   mutate_at(vars(-c("part.","States")),~ part. * .x) %>%
#   filter(States!="BR")
# 
# soma_linhas <- y %>%
#   select(-c("States","part.")) %>%
#   colSums %>%
#   setNames(colnames(y)[-c(1,length(y))])
# 
# 
# z <- bind_rows(y,soma_linhas) %>%
#   replace_na(list(States="BR")) %>%
#   select(-"part.") %>%
#   column_to_rownames("States")
# }
# ) %>% setNames(c("BdBrRetail_f","BdBrGrocery_f","BdBrParks_f","BdBrTransit_f","BdBrWork_f","BdBrResidential_f"))
# 
# 
# list2env(df_gg2, globalenv())
# 
# 
# 
# write.xlsx(BdBrRetail_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrRetail_f")
# write.xlsx(BdBrGrocery_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrGrocery_f",append=TRUE)
# write.xlsx(BdBrParks_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrParks_f",append=TRUE)
# write.xlsx(BdBrTransit_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrTransit_f",append=TRUE)
# write.xlsx(BdBrWork_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrWork_f",append=TRUE)
# write.xlsx(BdBrResidential_f, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrResidential_f",append=TRUE)
# write.xlsx(BdBrRetail, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google.xlsx', sheetName="BdBrRetail",append=TRUE)
# 
# 
# 
# 
# write.csv(BdBrRetail, 'C:\\Users\\leandro\\Documents\\GitHub\\corona_dash\\docs\\excel_base_google_teste.csv')
# 
# 
# 
# 
# m <-  as.matrix(BdBrRetail)
# p <- plot_ly(x=colnames(m), y=rownames(m), z = m, type = "heatmap") %>%
#      layout(margin = list(l=120))
#  p
# 
# 
# 
# 
# 

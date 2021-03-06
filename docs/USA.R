
USRepo          <- "https://raw.githubusercontent.com/COVID19Tracking/covid-tracking-data/master/data/states_daily_4pm_et.csv"

# US
RawUs <- readr::read_csv(USRepo,
                         col_types = paste0('cc', strrep('i', 10), strrep('c', 2), 'c','c', strrep('i', 2),'T',strrep('i', 9),'c',strrep('i', 6))
)




excel_us <- RawUs %>% select(date, state, positive, negative, pending, hospitalizedCurrently, hospitalizedCumulative, inIcuCurrently, recovered, death, totalTestsViral)

write.xlsx(excel_us, 'C:/Users/leandro/Documents/GitHub/corona_dash/docs/excel_base_us.xlsx')





DataReabertUS   <-  read.xlsx("C:/Users/leandro/Documents/GitHub/corona_dash/docs/DataReopenUS.xlsx", sheetName='data',check.names=FALSE)
DataReabertUS[DataReabertUS==0]<-as.character("2020-03-01") 
DataReabertUS %>% mutate_all(as.character)

#========================================================================================================
# USA

USstates   <- c("NY"
               ,"NJ"
               ,"MI"
               ,"CA"
               ,"MA"
               ,"FL"
               ,"IL"
               ,"LA"
               ,"WA"
               ,"PA"
               ,"GA"
               ,"TX"
               ,"CT"
               ,"CO"
               ,"TN"
               ,"OH"
               ,"IN"
               ,"MD")

SelectedUS <- c("US"
               ,"New York"
               ,"New Jersey"
               ,"Michigan"
               ,"California"
               ,"Massachusetts"
               ,"Florida"
               ,"Illinois"
               ,"Louisiana"
               ,"Washington"
               ,"Pennsylvania"
               ,"Georgia"
               ,"Texas"
               ,"Connecticut"
               ,"Colorado"
               ,"Tennessee"
               ,"Ohio"
               ,"Indiana"
               ,"Maryland"
               ,"Others")

RawUs$date <- format(as.Date(RawUs$date, format='%Y%m%d'))


UsCases <- RawUs %>% 
  select(positive, state, date) %>% 
  arrange(date) %>% 
  tidyr::spread(date, positive, fill = 0) %>% 
  rename_at(vars(-1), function(x) format(as.Date(x), "%m/%d/%y")) %>% 
  tibble::column_to_rownames("state")
  

UsCases <- UsCases %>% rbind(
    filter(UsCases, !(rownames(UsCases) %in% USstates)) %>% colSums
)

UsCases <- UsCases %>% rbind(
    filter(UsCases, rownames(UsCases) != 57) %>% colSums
)
rownames(UsCases)[57:58] <- c("Others", "US")

UsDeaths <- RawUs %>% 
  select(death, state, date) %>% 
  arrange(date) %>% 
  tidyr::spread(date, death, fill = 0) %>% 
  rename_at(vars(-1), function(x) format(as.Date(x), "%m/%d/%y")) %>% 
  tibble::column_to_rownames("state")

UsDeaths <- UsDeaths %>% rbind(
    filter(UsDeaths, !(rownames(UsDeaths) %in% USstates)) %>% colSums
)

UsDeaths <- UsDeaths %>% rbind(
    filter(UsDeaths, rownames(UsDeaths) != 57) %>% colSums
)
rownames(UsDeaths)[57:58] <- c("Others", "US")




UsHosp <- RawUs %>% 
  select(hospitalizedCurrently, state, date) %>% 
  arrange(date) %>% 
  tidyr::spread(date, hospitalizedCurrently, fill = 0) %>% 
  rename_at(vars(-1), function(x) format(as.Date(x), "%m/%d/%y")) %>% 
  tibble::column_to_rownames("state")

UsHosp <- UsHosp %>% rbind(
  filter(UsHosp, !(rownames(UsHosp) %in% UsHosp)) %>% colSums
)

UsHosp <- UsHosp %>% rbind(
  filter(UsHosp, rownames(UsHosp) != 57) %>% colSums
)
rownames(UsHosp)[57:58] <- c("Others", "US")




# Us Hospitalized
UsHospFormatted <- UsHosp[c("US", USstates, "Others"), ] %>% select(last_col(4:0)) 
rownames(UsHospFormatted) <- SelectedUS

dife_h_us <- difference(UsHosp)[c("US", USstates, "Others"), ] %>% select(last_col(4:0))
rownames(dife_h_us) <- SelectedUS

porce_h_us <- percentage(UsHosp)[c("US", USstates, "Others"), ] %>% select(last_col(4:0))
rownames(porce_h_us) <- SelectedUS






# Us Cases
UsCasesFormatted <- UsCases[c("US", USstates, "Others"), ] %>% select(last_col(4:0)) 
rownames(UsCasesFormatted) <- SelectedUS

dife_c_us <- difference(UsCases)[c("US", USstates, "Others"), ] %>% select(last_col(4:0))
rownames(dife_c_us) <- SelectedUS

porce_c_us <- percentage(UsCases)[c("US", USstates, "Others"), ] %>% select(last_col(4:0))
rownames(porce_c_us) <- SelectedUS

# Us Deaths
UsDeathsFormatted <- UsDeaths[c("US", USstates, "Others"), ] %>% select(last_col(4:0)) 
rownames(UsDeathsFormatted) <- SelectedUS

dife_d_us <- difference(UsDeaths)[c("US", USstates, "Others"), ] %>% select(last_col(4:0))
rownames(dife_d_us) <- SelectedUS

porce_d_us <- percentage(UsDeaths)[c("US", USstates, "Others"), ] %>% select(last_col(4:0))
rownames(porce_d_us) <- SelectedUS

# US Mortality
Dates <- seq.Date(from = as.Date('2020-01-22'),
                  to   = HereUSA,
                  by   = 'day')

UsMortality <- (UsDeaths / UsCases) %>% 
                 mutate_if(~ any(is.na(.)), ~ if_else(is.na(.), 0, .)) %>%
                 t() %>% 
                 as_tibble() %>% 
                 add_column(Dates) %>% 
                 rename_at(vars(-Dates), ~ rownames(UsCases))


 
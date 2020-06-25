
WorldRepoCases  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

WorldRepoDeaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

# World Cases
RawWorldCases <- readr::read_csv(WorldRepoCases, 
                                 col_types = paste0('cc', strrep('d', dias + 1))
                                ) 

# World Deaths
RawWorldDeaths <- readr::read_csv(WorldRepoDeaths, 
                                  col_types = paste0('cc', strrep('d', dias + 1))
                                 ) 


# World
SelectedCountries <- c("Italy"
                      ,"Spain"
                      ,"Germany"
                      ,"France"
                      ,"United Kingdom"
                      ,"Switzerland"
                      ,"Belgium"
                      ,"Netherlands"
                      ,"Austria"
                      ,"Portugal"
                      ,"Sweden"
                      ,"Norway"
                      ,"Denmark"
                      ,"Ireland"
                      ,"Romania"
                      ,"Luxembourg"
                      ,"Finland"
                      ,"Greece"
                      ,"Poland"
                      ,"Czechia")

WorldCases <- RawWorldCases %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  group_by(`Country/Region`) %>% 
  summarise_all(sum) %>% 
  tibble::column_to_rownames(var = "Country/Region")

# Need to fix some countings to be able to make a map plot (Somewhat Ugly Code. Think about this!) ----

totalCongo <- WorldCases %>% 
                filter(rownames(WorldCases) == "Congo (Brazzaville)" | rownames(WorldCases) == "Congo (Kinshasa)") %>%  
                colSums

totalItaly <- WorldCases %>% 
                filter(rownames(WorldCases) == "Italy" | rownames(WorldCases) == "Holy See") %>% 
                colSums

totalEgypt <- WorldCases %>% 
                filter(rownames(WorldCases) == "Egypt" | rownames(WorldCases) == "Western Sahara") %>% 
                colSums

condition   <- !rownames(WorldCases) %in% c("Italy", "Holy See", "Egypt", "Western Sahara", "Congo (Brazzaville)", "Congo (Kinshasa)")
NewRowNames <-  c(rownames(WorldCases)[condition], "Congo", "Italy", "Egypt")

WC   <- c("Bahamas", "Czechia", "Eswatini", "Gambia", "North Macedonia", "Taiwan*", "US", "West Bank and Gaza", "Congo")
out  <- c("Bahamas", "Czechia", "Swaziland", "Gambia", "Macedonia", "Taiwan", "United States", "West Bank", "Congo")

NewRowNames[NewRowNames %in% WC] <- out

WorldCases <- WorldCases %>%  
                filter(condition) %>% 
                rbind(totalCongo) %>% 
                rbind(totalItaly) %>% 
                rbind(totalEgypt)
rownames(WorldCases) <- NewRowNames

WorldDeaths <- RawWorldDeaths %>% 
  select(-`Province/State`,-Lat,-Long) %>% 
  group_by(`Country/Region`) %>% 
  summarise_all(sum) %>% 
  tibble::column_to_rownames(var = "Country/Region") 

# Need to fix some countings to be able to make a map plot (Somewhat Ugly Code. Think about this!) ----

totalCongo <- WorldDeaths %>% 
                filter(rownames(WorldDeaths) == "Congo (Brazzaville)" | rownames(WorldDeaths) == "Congo (Kinshasa)") %>%  
                colSums

totalItaly <- WorldDeaths %>% 
                filter(rownames(WorldDeaths) == "Italy" | rownames(WorldDeaths) == "Holy See") %>% 
                colSums

totalEgypt <- WorldDeaths %>% 
                filter(rownames(WorldDeaths) == "Egypt" | rownames(WorldDeaths) == "Western Sahara") %>% 
                colSums

WorldDeaths <- WorldDeaths %>%  
                filter(condition) %>% 
                rbind(totalCongo) %>% 
                rbind(totalItaly) %>% 
                rbind(totalEgypt) 
rownames(WorldDeaths) <- NewRowNames

# World Cases
dife_c  <- difference(WorldCases)
porce_c <- percentage(WorldCases)

# World Deaths
dife_d  <- difference(WorldDeaths)
porce_d <- percentage(WorldDeaths)


# World Mortality
DatesWorld <- seq.Date(from       = as.Date('2020-01-22'),
                       length.out = dias-1,
                       by         = 'day')

WorldMortality <- (WorldDeaths / WorldCases) %>%
                    mutate_if(~ any(is.na(.)), ~ if_else(is.na(.), 0, .)) %>%
                    t() %>% 
                    as_tibble() %>% 
                    add_column(Dates = DatesWorld) %>% 
                    rename_at(vars(-Dates), ~ rownames(WorldCases))

tableWorldCases  <- printDTFull(WorldCases)
tableWorldDeaths <- printDTFull(WorldDeaths) 

 
BRRepo          <- "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv"

BRpop           <- "http://api.sidra.ibge.gov.br/values/t/6579/p/2019/v/9324/n3/all/f/c/h/n"


# Fetching Main databases -----------------------------------------------------
# BR
RawBr <- readr::read_csv(BRRepo,
                         col_types = paste0('D', 'ccc', strrep('d', 10))
                         )

#BR Population
RawBrPop <- get_sidra(6579, 
                      variable = 9324,
                      period = "2019",
                      geo = "State" )

#========================================================================================================
# Brasil

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

BrCases <- RawBr %>% 
  select(date, state, totalCases) %>% 
  tidyr::spread(date, totalCases, fill = 0) %>% 
  tibble::column_to_rownames(var = "state") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BrCases <- BrCases %>% rbind(
   filter(BrCases, rownames(BrCases) %in% NULL) %>% colSums
  )
rownames(BrCases)[28:29] <- c("Brasil", "Demais")

BrDeaths <- RawBr %>% 
  select(date, state, deaths) %>% 
  tidyr::spread(date, deaths, fill = 0) %>% 
  tibble::column_to_rownames(var = "state") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BrDeaths <- BrDeaths %>% rbind(
   filter(BrDeaths, rownames(BrDeaths) %in% NULL) %>% colSums
  )
rownames(BrDeaths)[28:29] <- c("Brasil", "Demais")


N = ncol(RawBrPop)
BrPop <- RawBrPop %>% 
           rename_all(~ letters[1:N]) %>% 
           select(d, k) %>% 
           rename_all(~ c("Estado", "Pop")) %>% 
           arrange(Estado)

c("RO" ,"AC" ,"AM" ,"RR" ,"PA" ,"AP" ,"TO" ,"MA"
,"PI" ,"CE" ,"RN" ,"PB" ,"PE" ,"AL" ,"SE" ,"BA"
,"MG" ,"ES" ,"RJ" ,"SP" ,"PR" ,"SC" ,"RS" ,"MS"
,"MT" ,"GO" ,"DF")

#===================================================================

# Brazil Cases
BrCasesFormatted <- BrCases[SelectedBR, ] %>% select(last_col(4:0))
dife_c_br        <- difference(BrCases)[SelectedBR, ] %>% select(last_col(4:0))
porce_c_br       <- percentage(BrCases)[SelectedBR, ] %>% select(last_col(4:0))

# Brazil Deaths
BrDeathsFormatted <- BrDeaths[SelectedBR, ] %>% select(last_col(4:0))
dife_d_br         <- difference(BrDeaths)[SelectedBR, ] %>% select(last_col(4:0))
porce_d_br        <- percentage(BrDeaths)[SelectedBR, ] %>% select(last_col(4:0))


# Brazil Mortality
Dates <- seq.Date(from = as.Date('2020-02-25'),
                  to   = HereBR,
                  by   = 'day')

BrMortality <- (BrDeaths / BrCases) %>% 
                 mutate_if(~ any(is.na(.)), ~ if_else(is.na(.), 0, .)) %>% 
                 t() %>% 
                 as_tibble() %>% 
                 add_column(Dates) %>% 
                 rename_at(vars(-Dates), ~ rownames(BrCases))

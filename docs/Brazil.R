BRRepo          <- "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv"

BRRepoCity      <- "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"

BRpop           <- "http://api.sidra.ibge.gov.br/values/t/6579/p/2019/v/9324/n3/all/f/c/h/n"

BRDIARepo       <- "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv"


# Fetching Main databases -----------------------------------------------------
# BR
RawBr <- readr::read_csv(BRRepo,
                         col_types = paste0('D', 'ccc', strrep('d', 13))
                         )




RawBrCity <- readr::read_csv(BRRepoCity,
                         col_types = paste0('D', 'ccc', strrep('d', 8),'c')
)





#BR Population
RawBrPop <- get_sidra(6579, 
                      variable = 9324,
                      period = "2019",
                      geo = "State" )


SP_dic_reg <- read_delim(file="C:\\Users\\Padulla\\Documents\\GitHub\\corona_dash\\docs\\dic_regs.csv",delim=";") 

excel_base <- RawBr %>% select(date,country,state,city,newCases,totalCases,deaths)


write.xlsx(excel_base, 'C:\\Users\\Padulla\\Documents\\GitHub\\corona_dash\\docs\\excel_base.xlsx')




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




BrCasesCity <- RawBrCity %>% 
  select(date, state,city,ibgeID, totalCases) %>% 
  tidyr::spread(date, totalCases, fill = 0) 

SP_cases <- 
  BrCasesCity %>% 
  filter(state=="SP") %>% 
  left_join(SP_dic_reg,by="ibgeID")%>% 
  group_by(reg) %>% 
  select(-c("state","city","ibgeID","reg"))  %>% 
  group_map(~ colSums(.x)) 
  
reg_names <- arrange(unique(SP_dic_reg[,2]),reg)



CSP_cases <- 
  BrCasesCity %>% 
  filter(ibgeID=="3550308") %>% 
  column_to_rownames("city") %>% 
  select(-c("state","ibgeID")) %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))



SPCasesReg <- 
  do.call(rbind,SP_cases) %>% 
  as_tibble %>% 
  bind_cols(reg_names) %>% 
  column_to_rownames("reg") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y")) %>% 
  rbind(CSP_cases)







SPCasesMap <- BrCasesCity %>% 
  filter(state=="SP") %>% 
  select(-c("state","city")) %>% 
  column_to_rownames("ibgeID") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y")) %>% 
  select(ncol(SPCasesReg)) %>% 
  rownames_to_column("CD_MUN")





  




####

BrDeathsCity <- RawBrCity %>% 
  select(date, state,city,ibgeID, deaths) %>% 
  tidyr::spread(date, deaths, fill = 0) 

SP_deaths <- 
  BrDeathsCity %>% 
  filter(state=="SP") %>% 
  left_join(SP_dic_reg,by="ibgeID")%>% 
  group_by(reg) %>% 
  select(-c("state","city","ibgeID","reg"))  %>% 
  group_map(~ colSums(.x)) 

reg_names <- arrange(unique(SP_dic_reg[,2]),reg)

CSP_deaths <- 
  BrDeathsCity %>% 
  filter(ibgeID=="3550308") %>% 
  column_to_rownames("city") %>% 
  select(-c("state","ibgeID")) %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))




SPDeathsReg <- 
  do.call(rbind,SP_deaths) %>% 
  as_tibble %>% 
  bind_cols(reg_names) %>% 
  column_to_rownames("reg") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))%>% 
  rbind(CSP_deaths)






write.xlsx(SPCasesReg , 'C:\\Users\\Padulla\\Documents\\GitHub\\corona_dash\\docs\\excel_sp.xlsx', sheetName="SPCasesReg ")
write.xlsx(SPDeathsReg, 'C:\\Users\\Padulla\\Documents\\GitHub\\corona_dash\\docs\\excel_sp.xlsx', sheetName="SPDeathsReg",append=TRUE)






BrDeaths <- RawBr %>% 
  select(date, state, deaths) %>% 
  tidyr::spread(date, deaths, fill = 0) %>% 
  tibble::column_to_rownames(var = "state") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BrDeaths <- BrDeaths %>% rbind(
   filter(BrDeaths, rownames(BrDeaths) %in% NULL) %>% colSums
  )
rownames(BrDeaths)[28:29] <- c("Brasil", "Demais")


BrDeathsMS <- RawBr %>% 
  select(date, state, deathsMS) %>% 
  tidyr::spread(date, deathsMS, fill = 0) %>% 
  tibble::column_to_rownames(var = "state") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BrDeathsMS <- BrDeathsMS %>% rbind(
  filter(BrDeathsMS, rownames(BrDeathsMS) %in% NULL) %>% colSums
)
rownames(BrDeathsMS)[28:29] <- c("Brasil", "Demais")

BrDeaths_dif <- BrDeaths-BrDeathsMS



BrTests <- RawBr %>% 
  select(date, state, tests) %>% 
  tidyr::spread(date, tests, fill = 0) %>% 
  tibble::column_to_rownames(var = "state") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BrTests <- BrTests %>% rbind(
  filter(BrTests, rownames(BrTests) %in% NULL) %>% colSums
)
rownames(BrTests)[28:29] <- c("Brasil", "Demais")


BrTestsP100k <- RawBr %>% 
  select(date, state, tests_per_100k_inhabitants) %>% 
  tidyr::spread(date, tests_per_100k_inhabitants, fill = 0) %>% 
  tibble::column_to_rownames(var = "state") %>% 
  rename_all(function(x) format(as.Date(x), "%m/%d/%y"))

BrTestsP100k <- BrTestsP100k %>% rbind(
  filter(BrTestsP100k, rownames(BrTestsP100k) %in% NULL) %>% colSums
)
rownames(BrTestsP100k)[28:29] <- c("Brasil", "Demais")







BrCasesLb <-BrCases[-c(28, 29), ]
BrDeathsLb <-BrDeaths[-c(28, 29), ]

N = ncol(RawBrPop)
BrPop <- RawBrPop %>% 
           rename_all(~ letters[1:N]) %>% 
           select(c, k) %>% 
           rename_all(~ c("Estado", "Pop")) %>% 
           arrange(Estado)


BrPop$Estado[BrPop$Estado == "11"] <- "RO"
BrPop$Estado[BrPop$Estado == "12"] <- "AC"
BrPop$Estado[BrPop$Estado == "13"] <- "AM"
BrPop$Estado[BrPop$Estado == "14"] <- "RR"
BrPop$Estado[BrPop$Estado == "15"] <- "PA"
BrPop$Estado[BrPop$Estado == "16"] <- "AP"
BrPop$Estado[BrPop$Estado == "17"] <- "TO"
BrPop$Estado[BrPop$Estado == "21"] <- "MA"
BrPop$Estado[BrPop$Estado == "22"] <- "PI"
BrPop$Estado[BrPop$Estado == "23"] <- "CE"
BrPop$Estado[BrPop$Estado == "24"] <- "RN"
BrPop$Estado[BrPop$Estado == "25"] <- "PB"
BrPop$Estado[BrPop$Estado == "26"] <- "PE"
BrPop$Estado[BrPop$Estado == "27"] <- "AL"
BrPop$Estado[BrPop$Estado == "28"] <- "SE"
BrPop$Estado[BrPop$Estado == "29"] <- "BA"
BrPop$Estado[BrPop$Estado == "31"] <- "MG"
BrPop$Estado[BrPop$Estado == "32"] <- "ES"
BrPop$Estado[BrPop$Estado == "33"] <- "RJ"
BrPop$Estado[BrPop$Estado == "35"] <- "SP"
BrPop$Estado[BrPop$Estado == "41"] <- "PR"
BrPop$Estado[BrPop$Estado == "42"] <- "SC"
BrPop$Estado[BrPop$Estado == "43"] <- "RS"
BrPop$Estado[BrPop$Estado == "50"] <- "MS"
BrPop$Estado[BrPop$Estado == "51"] <- "MT"
BrPop$Estado[BrPop$Estado == "52"] <- "GO"
BrPop$Estado[BrPop$Estado == "53"] <- "DF"


BrPop <-BrPop %>%  arrange(Estado)

BrPop <- data.frame(BrPop, row.names = 1)

BrPop<-cbind(BrPop, replicate(ncol(BrCasesLb)-1,BrPop$Pop))

colnames(BrPop) <- colnames(BrCasesLb)

BrCasesPop <- (BrCasesLb /BrPop)*100000
BrDeathsPop <- (BrDeathsLb /BrPop)*1000000






RawBrDia <- readr::read_csv(BRDIARepo)

BrCasesDia <- RawBrDia %>%  select(state, totalCases)
BrCasesDia<-slice(BrCasesDia, 2:28)
BrCasesDia <- BrCasesDia %>%   tibble::column_to_rownames(var = "state")








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


# Brazil Tests
BrTestsFormatted <- BrTests[SelectedBR, ] %>% select(last_col(4:0))
dife_t_br        <- difference(BrTests)[SelectedBR, ] %>% select(last_col(4:0))
porce_t_br       <- percentage(BrTests)[SelectedBR, ] %>% select(last_col(4:0))



# Brazil Tests100k
BrTests100kFormatted <- BrTestsP100k[SelectedBR, ] %>% select(last_col(4:0))
dife_t100k_br        <- difference(BrTestsP100k)[SelectedBR, ] %>% select(last_col(4:0))
porce_t100k_br       <- percentage(BrTestsP100k)[SelectedBR, ] %>% select(last_col(4:0))


# SP Cases
SPCasesFormatted <- SPCasesReg %>% select(last_col(4:0))
dife_c_sp        <- difference(SPCasesReg)%>% select(last_col(4:0))
porce_c_sp       <- percentage(SPCasesReg) %>% select(last_col(4:0))

# SP Deaths
SPDeathsFormatted <- SPDeathsReg %>% select(last_col(4:0))
dife_d_sp        <- difference(SPDeathsReg)%>% select(last_col(4:0))
porce_d_sp       <- percentage(SPDeathsReg) %>% select(last_col(4:0))






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







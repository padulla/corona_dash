SPRepo <- 'https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv'

RawSp <- readr::read_delim(SPRepo 
                          , delim=";"
                        
)


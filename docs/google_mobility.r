#Google Mobily



library(tidyverse)
library(plotly)
library(xlsx)


RepoGoogle <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=57b4ac4fc40528e2"





RawGoogle <- readr::read_csv(RepoGoogle,
                             col_types = paste0('cccc','D',  strrep('d', 6))
)

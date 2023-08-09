library(magrittr)
library(data.table)
library(httr)
library(jsonlite)

library(tidyverse)

library(psrc.travelsurvey)
library(psrccensus)
library(psrcplot)
library(psrctrends)
install_psrc_fonts()


network_file_loc<-'J:/Projects/V2050/Housing/Monitoring/2023Update/Loan Denials- FFIEC'

# download csvs ----

years <- 2018:2022

# element names will be used for the region column and naming elements in master list. Adjust here.
geogs <- list(bsp = "MSAMD14740_BremertonSilverdalePortOrchard",
              sb = "MSAMD42644_SeattleBellevueKent", 
              tl = "MSAMD45104_TacomaLakewood")

# initiate master list to store final 3 tables
dfs <- list()

for(g in 1:length(geogs)) {
  temp_dfs <- list() # list to store individual cleaned tables
  
  for(i in 1:length(years)) {
   pathname <- file.path(network_file_loc, paste0(years[i], '/', geogs[g], years[i], ".csv"))
   df <- read.csv(pathname) # read raw csv
   
   # clean file
   rows_rm <- c(1, 2, 3, 13, 19, 20, 30, 36, 37, 47, 53, 54, 64, 70, 71, 81)
   cols_keep <- c(1, 2, 3, 9) 
   
   df <- df[-rows_rm, cols_keep] %>% 
     rename("race_ethnicity" ="INCOME..RACE.AND.ETHNICITY",
            "received" = "Applications.Received", 
            "denied" = "Applications.Denied",
            "median_income" = "X") %>% 
     mutate(denied = as.numeric(as.character(denied)),
            received = as.numeric(as.character(received))) %>% 
     mutate(share_denials_race = denied/received,
            data_year = years[i],
            region = names(geogs)[g]) %>% 
     select(data_year, region, everything()) # re-order cols
   
   temp_dfs[[i]] <- df # append to intermediate list
  }
  
  # bind all cleaned tables and store in master list; name is recycled from geogs element names.
  dfs[[names(geogs)[g]]]<- rbindlist(temp_dfs) 
}


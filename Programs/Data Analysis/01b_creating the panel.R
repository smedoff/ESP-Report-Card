
library(tidyverse)
library(dplyr)
library(readxl)

rm(list=ls())

source(file.path("hlpr_func", "hlprfnx_calc_real_dollars.r"))

  # Bringing in the fisheries data 
  fishery.df <- read.csv(file.path("Data", "FEPGroups_2000_2021c.csv"))  %>% 
    select(-X) %>% 
    rename(Species = CAT_NAME,
           Year = YEAR,
           tot_sold_rev = tot_sold_rev_nominal)  %>% 
    mutate(price = round(tot_sold_rev/tot_lbs_sold, digits = 2),
           fishers_sold_ratio = tot_fishers_sold/tot_fishers) %>% 
    calc_real_price.f(price_var = "price", adj_yr = 2021) %>% 
    mutate(Real_tot_sold_rev = tot_lbs_sold*Real_price) %>% 
    mutate(Species = recode(Species, 
                            "Crustaceans" = "crustaceans",
                            "Uku" = "uku"))
  
  #-----
  # Bringing in CPUE by species 
  species_cpue.df <- readRDS(file.path("Data", "species_cpue.rds")) 
  
  fishery.df <- fishery.df %>% 
    left_join(species_cpue.df)
  
  #-----
  # Bringing in diesel prices 
  diesel.df <- read.csv(file.path("Data", "Diesel_final.csv")) %>% 
    mutate(Year = as.numeric(substr(Date, 1, 4))) %>% 
    group_by(Year, Source) %>% 
    summarize(Price = mean(Price, na.rm = TRUE)) %>% 
    spread(Source, Price) %>% 
    calc_real_price.f(price_var = "DBEDT - Hawaii Prices", adj_yr = 2021) %>% 
    calc_real_price.f(price_var = "EIA - US prices", adj_yr = 2021) %>% 
    select(-c("DBEDT - Hawaii Prices", "EIA - US prices")) %>% 
    rename("Diesel_HI_2021" = "Real_DBEDT - Hawaii Prices",
           "Diesel_US_2021" = "Real_EIA - US prices") %>% 
    filter(Year %in% fishery.df$Year)
  
  
  #-----
  # Bringing in the socioeconomic data 
  # Bring in socioeconomic data
  socecon_path <- file.path("Data", "Socio-Economic")
  socecon_files <- list.files(socecon_path)
  socecon_vars <- c("Earnings", "Jobs", "GDP", "Population", 
                    "Visitor Arrivals", "Unemployment Rate")
  
  if(length(socecon_files) != length(socecon_vars)){
    stop(print("STOP!! make sure you assigned each file a name in the socecon_vars vector"))
  }
  
  socecon.l <- lapply(1:length(socecon_files), FUN = function(f){
    print(f)
    one_file <- socecon_files[f]
    
    one_var <- socecon_vars[f]
    
    one_data <- read.csv(file.path(socecon_path, one_file)) %>% 
      mutate(Level = str_remove(Level, ",") %>% as.numeric()) %>% 
      rename(Date = DateTime) %>% 
      doBy::renameCol("Level", one_var) %>% 
      mutate(Year = as.numeric(str_sub(Date, -4, -1))) %>% 
      select(Year, one_var)
  })
  
  socecon.df <- purrr::reduce(socecon.l, left_join) %>% 
    gather(Indicator, Indicator_Value, socecon_vars) %>% 
    spread(Indicator, Indicator_Value) %>% 
    filter(Year %in% unique(fishery.df$Year))
  
  rm(list=setdiff(ls(), c("fishery.df", "socecon.df", "diesel.df")))
  
  
  
  #-----
  # Bringing in the environemntal data 
  ENSO.df <- read_xlsx(file.path("Data", "Environmental", "ENSO.xlsx")) %>% 
    group_by(Year = YR) %>% 
    summarize("ENSO Tot" = mean(TOTAL, na.rm = TRUE),
              "ENSO anom" = mean(ANOM, na.rm = TRUE)) %>% 
    filter(Year %in% unique(fishery.df$Year)) 
  
  PDO.df <- read_xlsx(file.path("Data", "Environmental", "PDO.xlsx")) %>% 
    mutate_if(is.character, as.numeric) %>% 
    gather(Month, pdo, "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
           "Aug", "Sep", "Oct", "Nov", "Dec") %>% 
    group_by(Year) %>% 
    summarize(PDO = mean(pdo, na.rm = TRUE)) %>% 
    filter(Year %in% unique(fishery.df$Year))

  #-----
  # Merging the data together
  final.df <- fishery.df %>% 
    left_join(diesel.df) %>% 
    left_join(socecon.df) %>% 
    left_join(ENSO.df) %>% 
    left_join(PDO.df) 
  
  saveRDS(final.df, file.path("Data", "final.rds"))
  

library(tidyverse)
library(dplyr)
library(stringr)

rm(list=ls())

  # Bring in gear CPUE data from 01a_calc cpue by species.R
  wcpfc_uku.df <- readRDS(file.path("Data", "uku_gear_cpue.rds"))

  #---------------------
  #' Bring in indicators 
  
  indicators.v <- c("HI Diesel (2021$)" = "Diesel_HI_2021",
                    "US Diesel (2021$)" = "Diesel_US_2021",
                    "Forest, Fishing, and Other State Earnings" = "Earnings",
                    "Real GDP" = "GDP", 
                    "Uku Fisher Jobs" = "Jobs", 
                    "HI Resident Population" = "Population", 
                    "HI Unemployment Rate" = "Unemployment Rate", 
                    "Visitor Arrival Count" = "Visitor Arrivals", 
                    "ENSO Total" = "ENSO Tot", 
                    "ENSO Anom" = "ENSO anom", 
                    "PDO" = "PDO")
  
  indicators.df <- readRDS(file.path("Data", "final.rds")) %>% 
    filter(Species == "uku") %>% 
    select(Year, unname(indicators.v)) %>% 
    left_join(wcpfc_uku.df)
  
  uku.l <- list(indicators = indicators.v, 
                 data = indicators.df)
  
  saveRDS(uku.l, file.path("Data", "uku_list.rds"))
  
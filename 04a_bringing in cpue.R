
library(tidyverse)
library(dplyr)
library(stringr)

rm(list=ls())

  wcpfc_uku.df <- read.csv(file.path("Data", "wcpfc_uku_cpue_T11.csv")) 
  
  col_names.v <- c("Deep Sea Handline License" = "DSHL_license", 
                   "Deep Sea Handline Trips" = "DSHL_trips",
                   "Deep Sea Handline Caught (lbs)" = "DSHL_catch",
                   "Deep Sea Handline CPUE" = "DSHL_cpue",
                   
                   "Inshore Handline License" = "ISHL_license", 
                   "Inshore Handline Trips" = "ISHL_trips",
                   "Inshore Handline Caught (lbs)" = "ISHL_catch",
                   "Inshore Handline CPUE" = "ISHL_cpue",
                   
                   "Trolling License" = "troll_license", 
                   "Trolling Trips" = "troll_trips",
                   "Trolling Caught (lbs)" = "troll_catch",
                   "Trolling CPUE" = "troll_cpue",
                   
                   "Other Gears License" = "other_license", 
                   "Other Gears Trips" = "other_trips",
                   "Other Gears Caught (lbs)" = "other_catch",
                   "Other Gears CPUE" = "other_cpue")
  
  names(wcpfc_uku.df) <- c("Year", unname(col_names.v))

  #---------------------
  #' Calc aggregate CPUE
  #' ok... so we have CPUE by gear type but we dont have CPUE in aggregate 
  #' CPUE = catch/trips
  #' CPUE_gear1 + CPUE_gear2 does not equal (catch_gear1 + catch_gear2) / (trips_gear1 + trips_gear2)
  
  trip_vars.v <- str_subset(names(wcpfc_uku.df), "trips") %>% na.omit()
  catch_vars.v <- str_subset(names(wcpfc_uku.df), "catch") %>% na.omit()
  
  agg_uku.df <- wcpfc_uku.df %>% 
    group_by(Year) %>% 
    rowwise() %>% 
    summarize(agg_trips = sum(DSHL_trips, ISHL_trips, troll_trips, other_trips),
              catch = sum(DSHL_catch, ISHL_catch, troll_catch, other_catch)) %>% 
    mutate(agg_cpue = round(catch/agg_trips, digits = 2)) %>% 
    na.omit() %>% 
    select(Year, agg_cpue, agg_trips)
  
  final_uku.df <- wcpfc_uku.df %>% 
    select(Year, str_subset(names(wcpfc_uku.df), "trips"),
           str_subset(names(wcpfc_uku.df), "cpue")) %>% 
    left_join(agg_uku.df)  

  
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
    left_join(final_uku.df)
  
  uku.l <- list(indicators = indicators.v, 
                 data = indicators.df)
  
  saveRDS(uku.l, file.path("Data", "uku_list.rds"))
  
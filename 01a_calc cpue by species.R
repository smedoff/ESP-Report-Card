
library(tidyverse)
library(dplyr)
library(stringr)

rm(list=ls())
  
  wcpfc_uku.df <- read.csv(file.path("Data", "cpue", "wcpfc_uku_cpue_T11.csv")) 
  
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
    summarize(agg_trips = sum(DSHL_trips, ISHL_trips, troll_trips, other_trips, na.rm = TRUE),
              catch = sum(DSHL_catch, ISHL_catch, troll_catch, other_catch, na.rm = TRUE)) %>% 
    mutate(agg_cpue = round(catch/agg_trips, digits = 2)) %>% 
    na.omit() %>% 
    select(Year, agg_cpue, agg_trips)
  
  final_uku.df <- wcpfc_uku.df %>% 
    select(Year, str_subset(names(wcpfc_uku.df), "trips"),
           str_subset(names(wcpfc_uku.df), "cpue")) %>% 
    left_join(agg_uku.df) 
  
  saveRDS(final_uku.df, file.path("Data", "uku_gear_cpue.rds"))
  
  rm(list=setdiff(ls(), c("final_uku.df")))
  
  
  #---------------
  # Calc CPUE for deep 7
  wcpfc_d7.df <- read.csv(file.path("Data", "cpue", "deep7_cpue.csv")) %>% 
    select(Year, Trips, Lbs_Catch = Lbs..Caught) %>% 
    mutate(Species = "Deep 7",
           CPUE = Lbs_Catch/Trips) %>% 
    select(Year, Species, CPUE, Trips)
  
  
  #---------------
  # Calc CPUE for PMUS
  pmus_files.v <- list.files(file.path("Data", "cpue", "pelagic troll"))
  
  wcpfc_pmus_troll.l <- lapply(1:length(pmus_files.v), FUN = function(f){
    
    one_file.df <- read.csv(file.path("Data", "cpue", "pelagic troll", pmus_files.v[f]))
    
    species.v <- setdiff(names(one_file.df), "Year")
    
    one_file_long.df <- one_file.df %>% 
      gather(Species, CPUE, species.v) %>% 
      mutate(Trips = NA)
    
    return(one_file_long.df)
    
    })
  
  wcpfc_pmus_troll.df <- do.call(rbind, wcpfc_pmus_troll.l) %>% 
    group_by(Year) %>% 
    summarize(CPUE = sum(CPUE, na.rm = TRUE),
              Trips = unique(Trips)) %>% 
    mutate(Species = "PMUS")
  
  
  final.df <- final_uku.df %>% 
    mutate(Species = "uku") %>% 
    select(Year, Species, CPUE = agg_cpue, Trips = agg_trips) %>% 
    rbind(wcpfc_d7.df) %>% 
    rbind(wcpfc_pmus_troll.df)
  
  saveRDS(final.df, file.path("Data", "species_cpue.rds"))  

  
    

library(tidyverse)
library(dplyr)
library(readxl)
library(corrplot)

rm(list=ls())

  fishery_vars.v <- c("Commercial fishers reporting catch" = "tot_fishers", 
                      "Number of fishers reporting sales" = "tot_fishers_sold", 
                      "Fishers sold ratio" = "fishers_sold_ratio",
                      "Pounds kept" = "tot_lbs_kept", 
                      "Pounds sold" = "tot_lbs_sold", 
                      "Price (2021 $)" = "Real_price", 
                      "Revenue (2021 $)" = "Real_tot_sold_rev",
                      "CPUE" = "CPUE",
                      "Trips" = "Trips")
  
  uku.df <- readRDS(file.path("Data", "final.rds")) %>%  
    filter(Species == "uku") %>%
    select(Year, unname(fishery_vars.v)) 
  
  current_year <- max(uku.df$Year)
  
  #------------
  # Creating one row of the stats report
  one_var_final.l <- lapply(1:length(fishery_vars.v), FUN = function(f){
    
    one_var <- unname(fishery_vars.v[f])
    one_lab <- names(fishery_vars.v[f])
    
    one_var.df <- uku.df %>% 
      select(Year, one_var)
    
    round_to <- ifelse(one_var %in% c("tot_fishers", "tot_fishers_sold"), 0, 2)
    
    current_var <- one_var.df %>% 
      filter(Year == current_year) %>% 
      pull(one_var) %>% 
      round(digits = round_to) 
    
    mean_var <- one_var.df %>% 
      filter(Year >= current_year - 10) %>% 
      select(mean_yr = one_var) %>% 
      summarize(mean_yr = mean(mean_yr, na.rm = TRUE)) %>% 
      mutate(current_yr = current_var) %>% 
      mutate(change = round((current_yr - mean_yr)/mean_yr*100, 
                            digit = 1) %>% paste0("%")) 
    
    previous_var <- one_var.df %>% 
      filter(Year == current_year - 1) %>% 
      rename(prev_yr = one_var) %>% 
      mutate(current_yr = current_var) %>% 
      mutate(change = round((current_yr - prev_yr)/prev_yr*100, 
                            digit = 1) %>% paste0("%")) 
    
    # Clean up the current_var element 
    if(one_var %in% c("Real_price", "Real_tot_sold_rev")){
      
      current_var_clean <- current_var %>% 
        str_pad(pad = "0", side = "right", width = 2) %>% 
        prettyNum(big.mark = ",") 
      
      current_var_clean <- paste0("$", current_var_clean) 
      
    }else{
      if(one_var == "fishers_sold_ratio"){
        current_var_clean <- paste0(current_var*100, "%") 
      }else{
        current_var_clean <- formatC(current_var, format = "d", big.mark = ",")
      }
    }
    
    one_var_final.df <- data.frame(
      Var = one_lab,
      Value = current_var_clean,
      Previous_change = previous_var$change,
      Mean_change = mean_var$change) 
    
#    names(one_var_final.df) <- c("Fishery Variable",
#                                 current_year,
#                                 "Previous Year",
#                                 paste0("10yr Average (", current_year - 10,
#                                        "-", current_year, ")"),
#                                 "Summary of Change")
    
    return(one_var_final.df)
  })
  
  one_var_final.df <- do.call(rbind, one_var_final.l)
  
  
  #---------------
  # Saving the table
  library(gtable)
  library(gt)
  install.packages("webshot2") #to save table
  one_var_final.df %>% 
    gt() %>% 
    tab_header(title = "Fishery Preformance") %>% 
    cols_label(Var = "Fishery Variable",
               Value = current_year,
               Previous_change = paste0("Previous Year<br>(", current_year-1, ")"),
               Mean_change = paste0("Mean change<br>(", 
                                    current_year - 10, "-",
                                    current_year,")"),
               .fn = md) %>% 
    tab_footnote(footnote = paste0("This table reports statistics for fishery preformance. ",
                                   "The second column refelcts current year statistics, ",
                                   "the third column reflects the percentage change from the ",
                                   "prior year, and the fourth column refects the percentage ",
                                   "change from the 10-year mean.")) %>% 
    gtsave(file.path("Results", "tables", "stats_report.png"))
  
  
  #-----------------
  #  library(gridExtra)
  #  png(file.path("Results", "tables", "stats_report.png"), 
  #      height = 50*nrow(one_var_final.df), 
  #      width = 200*ncol(one_var_final.df))
  #  grid.table(one_var_final.df, rows = NULL)
  #  dev.off()
  
  
  
  
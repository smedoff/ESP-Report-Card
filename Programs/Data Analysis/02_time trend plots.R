
library(tidyverse)
library(dplyr)
library(readxl)

rm(list=ls())

source(file.path("hlpr_func", "hlpr_func_standardize_group_variables.R"))


  final.df <- readRDS(file.path("Data", "final.rds")) %>% 
    select(-c("price", "tot_sold_rev")) %>% 
    mutate(Rev_trip = Real_tot_sold_rev/Trips) %>% 
    rename("HI Diesel (2021$)" = "Diesel_HI_2021",
           "US Diesel (2021$)" = "Diesel_US_2021")
  
  #-------------
  fishery_vars.v <- c("Number of fishers" = "tot_fishers", 
                      "Pounds kept" = "tot_lbs_kept", 
                      "Number of fishers that sold uku" = "tot_fishers_sold", 
                      "Fishers sold ratio" = "fishers_sold_ratio",
                      "Pounds sold" = "tot_lbs_sold", 
                      "Price (2021 $)" = "Real_price", 
                      "Revenue (2021 $)" = "Real_tot_sold_rev", 
                      "CPUE" = "CPUE",
                      "Trips" = "Trips",
                      "Revenue per trip" = "Rev_trip")
  
  indicators.v <- setdiff(names(final.df), c(fishery_vars.v, "Year", "Species"))
    
  
  #--------------
  # Plot time trends 
  lapply(1:length(fishery_vars.v), FUN = function(f){
    
    fishery_var <- unname(fishery_vars.v[f])
    fishery_lab <- names(fishery_vars.v[f])
    
    lapply(1:length(indicators.v), FUN = function(i){
      
      one_indicator <- indicators.v[i]
      
      sub.df <- final.df %>% 
        select(Year, Species, fishery_var, one_indicator) %>% 
        standardize_group_variables.f(grouping_var = "Species", stdz_var = fishery_var) %>% 
        standardize_group_variables.f(grouping_var = "Species", stdz_var = one_indicator) 
      
      # Pull out the standardized variables 
      stdz_fishery_var <- paste0("stdz_", fishery_var)
      stdz_one_indicator <- paste0("stdz_", one_indicator)
      
      uku.df <- sub.df %>% 
        filter(Species == "uku") %>% 
        select(Year, 
               stdz_fishery_var, 
               stdz_one_indicator) %>% 
        gather(Variable, Value, stdz_fishery_var, stdz_one_indicator) %>% 
        mutate(Variable = ifelse(Variable == stdz_fishery_var, 
                                 fishery_lab, 
                                 one_indicator)) %>% 
        mutate(Variable = factor(Variable, levels = c(fishery_lab, one_indicator)))
      
      other_species.df <- sub.df %>% 
        filter(Species != "uku") %>% 
        select(Year, 
               Species,
               FISH_VAR = stdz_fishery_var) %>% 
        filter(!is.na(FISH_VAR))
      
      ggplot() + 
        geom_boxplot(data = other_species.df, aes(x = as.character(Year), y = FISH_VAR)) +
        geom_line(data = uku.df, aes(x = as.character(Year), y = Value, color = Variable, group = Variable), size = 2) + 
        xlab("Year") + 
        ylab(paste0(fishery_lab, " (Standardized)")) + 
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) + 
        labs(title = paste0(fishery_lab, " vs. ", one_indicator),
             caption = paste0("Time trends for the ", str_to_lower(fishery_lab), " in the uku fishery and average state ", 
                              one_indicator, ". \n Boxplots reflect the distribution of ", 
                              str_to_lower(fishery_lab), " for ", paste(unique(other_species.df$Species), collapse = ", "))) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 20),
              axis.text.y = element_text(size = 20),
              axis.title = element_text(size = 25),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 20),
              plot.title = element_text(size = 30),
              plot.caption = element_text(size = 20, hjust = 0, vjust = -7, face= "italic"),
              plot.caption.position = "plot",
              plot.margin = margin(1, 1, 2, 1, "cm"))
      ggsave(file.path("Results", "time trends", "boxplots", paste0("time_trends_", fishery_var, "_", one_indicator, ".png")),
             height = 15, width = 15)
      
      #-----------
      # Averages 
  
      # Creating legend labels
      legend_labels <- c(paste0(fishery_lab, " (uku)"),
                         paste0("Average ", str_to_lower(fishery_lab), 
                                " (", paste(unique(other_species.df$Species), collapse = ", "), ")"), 
                         one_indicator) %>% str_wrap(20)
      
      # Creating graphing df 
      graphing.df <- uku.df %>% 
        mutate(Variable = ifelse(Variable == fishery_lab, 
                                 "uku",
                                 one_indicator))  %>% 
        rbind(other_species.df %>% 
                group_by(Year) %>% 
                summarize(Value = mean(FISH_VAR, na.rm = TRUE)) %>% 
                mutate(Variable = "other")) %>% 
        mutate(Variable = factor(Variable, levels = c("uku", "other", one_indicator)))
    
      # Creating the plots
      ggplot(graphing.df, aes(x = as.character(Year), y = Value)) + 
        geom_line(aes(color = factor(Variable), group = factor(Variable), linetype = factor(Variable)), size = 2) +
        scale_color_manual(labels = legend_labels,
                           values = c("#CC79A7", "#56B4E9", "black")) + 
        scale_linetype_manual(labels = legend_labels,
                              values = c("solid", "solid", "longdash")) + 
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) + 
        xlab("Year") + 
        ylab(paste0(fishery_lab, " (Standardized)")) + 
        labs(title = paste0(fishery_lab, " vs. ", one_indicator),
             caption = paste0("Time trends for the ", str_to_lower(fishery_lab), " in the uku fishery and average state ", 
                              one_indicator, ". \n The dashed line reflect the average of ", 
                              str_to_lower(fishery_lab), " for ", paste(unique(other_species.df$Species), collapse = ", "), " combined.")) + 
        
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 20),
              axis.text.y = element_text(size = 20),
              axis.title = element_text(size = 25),
              legend.text = element_text(size = 20),
              legend.title = element_blank(),
              legend.key.size = unit(3, "cm"),
              plot.title = element_text(size = 30),
              plot.caption = element_text(size = 20, hjust = 0, vjust = -7, face= "italic"),
              plot.caption.position = "plot",
              plot.margin = margin(1, 1, 2, 1, "cm"))
      ggsave(file.path("Results", "time trends", "averages", paste0("time_trends_", fishery_var, "_", one_indicator, ".png")),
             height = 15, width = 15)
      
    
      }) #end of indicator lapply
    
  }) #end of fishery_var lapply
  
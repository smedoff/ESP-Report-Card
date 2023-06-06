
library(tidyverse)
library(dplyr)
library(stringr)
library("gridExtra") 

rm(list=ls())

source(file.path("hlpr_func", "hlpr_func_standardize_group_variables.R"))
source(file.path("hlpr_func", "hlpr_func_creating_uku_fishery_time_trends.R"))

  uku.l <- readRDS(file.path("Data", "uku_list.rds")) 
  
  # Bring in uku data
  uku.df <- uku.l[["data"]]
  
  # Bring in named indicator vector
  indicators.v <- uku.l[["indicators"]]
  
  # Creating a named uku vector 
  cpue_vars.v <- c("Deep Sea Handline CPUE" = "DSHL_cpue",
                   "Inshore Handline CPUE" = "ISHL_cpue",
                   "Trolling CPUE" = "troll_cpue",
                   "Other Gear CPUE" = "other_cpue",
                   "Total CPUE" = "agg_cpue")
                  
  trips_vars.v <- c("Deep Sea Handline Trips" = "DSHL_trips",
                   "Inshore Handline Trips" = "ISHL_trips",
                   "Trolling Trips" = "troll_trips",
                   "Other Gear Trips" = "other_trips",
                   "Total Trips" = "agg_trips")
  
  #----------
  # creating time trends 
  # cpue
  creating_uku_fishery_time_trends.f(df = uku.df,
                                     uku_vars.v = cpue_vars.v,
                                     uku_indicators.v = indicators.v)
  
  # trips
  creating_uku_fishery_time_trends.f(df = uku.df,
                                     uku_vars.v = trips_vars.v,
                                     uku_indicators.v = indicators.v)
  
  
  #----------------------------
  # Do ENSO and PDO 
  
  tt.df <- uku.df %>% 
    select(Year, unname(uku_vars.v), "ENSO anom", "PDO") %>% 
    gather(Variable, Value, unname(uku_vars.v), "ENSO anom", "PDO") %>% 
    mutate(Value = round(Value, digits = 2)) %>% 
    standardize_group_variables.f(grouping_var = "Variable",
                                  stdz_var = "Value")
  
  ttplot.l <- lapply(1:length(uku_vars.v), FUN = function(c){
    
    cpue_var <- uku_vars.v[c]
    
    title <- names(cpue_var)
    
    tt.p <- tt.df %>% 
      filter(Variable %in% c("ENSO anom", unname(cpue_var), "PDO")) %>% 
      mutate(Variable = factor(Variable, levels = c("ENSO anom", cpue_var, "PDO"))) %>% 
      ggplot(aes(Year, stdz_Value)) + 
      geom_line(aes(color = Variable, size = Variable, group = Variable)) +
      scale_color_manual(values = c("#0072B2", "#000000", "#56B4E9"),
                         labels = c("ENSO anom", names(cpue_var), "PDO")) + 
      scale_size_manual(values = c(2, 3, 2),
                        labels = c("ENSO anom", names(cpue_var), "PDO")) + 
      scale_linetype_manual(values = c("longdash", "longdash", "longdash"),
                            labels = c("ENSO anom", names(cpue_var), "PDO")) + 
      ylab("Standardized Value") + 
      labs(title = title) + 
      guides(color = guide_legend(nrow = 2)) +
      theme(axis.title = element_text(size = 15),
            axis.title.x = element_blank(),
            plot.title = element_text(size = 25), 
            axis.text = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_blank(),
            legend.position = "bottom", legend.box="vertical") 
    
    return(tt.p)
    
  }) #end of cpue lapply
  
  names(ttplot.l) <- unname(uku_vars.v)
  
  # Extracting the aggregate cpue plot
  agg_plot.p <- ttplot.l[["agg_cpue"]]  + 
    theme(axis.title = element_text(size = 20),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 30), 
          axis.text = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.position = "bottom")
  
  # Extracting the gear specific cpue plots and plotting them on one panel
  ttplot.l[["agg_cpue"]] = NULL
  gear_plot.p <- do.call("grid.arrange", c(ttplot.l, nrow = 2))  
  
  # Plotting on one panel
  ggsave(file = file.path("Results", "just uku", "cpue", paste0("timetrends_enso_pdo.png")), 
         gridExtra::arrangeGrob(gear_plot.p, agg_plot.p, ncol = 2), 
         device = "png", width = 20, height = 12, dpi = 72)
  
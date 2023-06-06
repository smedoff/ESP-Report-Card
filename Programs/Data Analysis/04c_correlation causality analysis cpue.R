
# Script is not complete!!!!!
# go to SM EDITS to see where you left off 

library(tidyverse)
library(dplyr)
library(readxl)
library(corrplot)

rm(list=ls())

source(file.path("hlpr_func", "hlprfnx_calc_granger_causality.R"))

  cpue.l <- readRDS(file.path("Data", "cpue_list.rds")) 
  
  # Bring in cpue data
  # to use the calc_granger_causality.f we need a Species column and we 
  # need to call the main df final.df
  final.df <- cpue.l[["data"]] %>% 
    mutate(Species = "uku")
  
  species.v <- "uku"
  
  # Bring in named indicator vector
  indicators.v <- cpue.l[["indicators"]]
  
  # Creating a named cpue vector 
  cpue_vars.v <- c("Deep Sea Handline CPUE" = "DSHL_cpue",
                   "Inshore Handline CPUE" = "ISHL_cpue",
                   "Trolling CPUE" = "troll_cpue",
                   "Other Gear CPUE" = "other_cpue",
                   "Total CPUE" = "agg_cpue")
  
  
  corr_granger.l <- lapply(1:length(cpue_vars.v), FUN = function(f){
    
    cpue_var <- unname(cpue_vars.v[f])
    cpue_lab <- names(cpue_vars.v[f])
    
    granger.df <- calc_granger_causality.f(indicators.vector = indicators.v,
                                           species.vector = species.v,
                                           fishery_variable = cpue_var) %>% 
      rownames_to_column(var = "Indicator_label")
    
    # Apply first differencing and calc corr
    correlation.df <- final.df %>% 
      select(Year, cpue_var) %>% 
      # left_join with the indicator variables
      left_join(final.df %>% 
                  select(Year, indicators.v) %>% 
                  unique()) %>% 
      # drop Year variable 
      select(-Year) %>% 
      # apply first differencing to get unbiased estimates of correlation
      apply(2, diff) %>% 
      data.frame() %>% 
      cor(use = "complete.obs") %>% 
      data.frame() %>% 
      rownames_to_column("Indicator") 
    
    
#-------------------
  # SM EDITS!!! 
    #' This script is not complete 
    #' i ended at line 56 but i need to programically remove the ... in the 
    #' indicator column so we can match the correlation.df and the granger.df 
    #' by indicator. 
    
#-------------------    
    
      select(Indicator, cpue_var) %>% 
      # we just want to keep the corr coef for the indicators and the species 
      filter(Indicator != cpue_var) %>% 
      mutate(Indicator = str_replace_all(Indicator, "\\.", " ")) %>%
      left_join(granger.df %>% 
                  select(Indicator = indicator, sig) %>% 
                  mutate(Indicator = str_replace(Indicator, "_", " "))) %>% 
      mutate(Correlation = round(Correlation, digits = 2),
             Species = str_replace(Species, "\\.", " "),
             Indicator = str_replace(Indicator, "\\.", " "),
             Fishery_var = cpue_var)
    
    # Plotting the correlations 
    # point graphs 
    ggplot(correlation.df, aes(Indicator, Correlation)) + 
      geom_point(aes(color = Species, shape = sig, size = sig), size = 6) + 
      xlab("") + 
      ylab("") +
      labs(title = cpue_lab,
           caption = paste0("The correlation coefficient for uku and each indicator are represented by the red points. The distribution of \n ", 
                            "correlation coefficients for ", paste(unique(final.df$Species), collapse = ", "), "are presented as boxplots.")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 20),
            axis.text.y = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            plot.title = element_text(size = 30),
            plot.caption = element_text(size = 20, hjust = 0, face= "italic"),
            plot.caption.position = "plot",
            plot.margin = margin(1, 1, 1, 1, "cm"))
    ggsave(file.path("Results", "correlation", paste0("corr_point_", cpue_var, ".png")),
           height = 15, width = 15)
    
    #-------------
    # distributional graphs 
    other_species.df <- correlation.df %>% 
      filter(Species %in% setdiff(species.v, "uku"))
    
    uku.df <- correlation.df %>% 
      filter(Species == "uku") %>% 
      arrange(desc(Correlation))
    
    levels(uku.df$sig) <- unique(granger.df$sig)
    
    ggplot() + 
      geom_boxplot(data = other_species.df, aes(x = Indicator, y = Correlation)) +
      geom_point(data = uku.df, aes(x = Indicator, y = Correlation, shape = sig), 
                 color = "#CC79A7", size = 6) + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      xlim(uku.df$Indicator) +
      xlab("") + 
      ylab("") +
      labs(title = fishery_lab,
           caption = paste0("The correlation coefficient for uku and each indicator are represented by the red points. The distribution of \n ", 
                            "correlation coefficients for ", paste(unique(final.df$Species), collapse = ", "), "are presented as boxplots.")) +
      scale_shape(name = "Predictive Indicator",
                  limits = unique(granger.df$sig)) + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 20),
            axis.text.y = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            plot.title = element_text(size = 30),
            plot.caption = element_text(size = 20, hjust = 0, face= "italic"),
            plot.caption.position = "plot",
            plot.margin = margin(1, 1, 1, 1, "cm"))
    ggsave(file.path("Results", "correlation", paste0("corr_distribution_", cpue_var, ".png")),
           height = 15, width = 15)
    
    return(correlation.df)
    
  }) #end of fishery_var lapply
  
  
  

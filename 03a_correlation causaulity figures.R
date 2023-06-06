
library(tidyverse)
library(dplyr)
library(readxl)
library(corrplot)

rm(list=ls())

source(file.path("hlpr_func", "hlprfnx_calc_granger_causality.R"))

  final.df <- readRDS(file.path("Data", "final.rds")) %>% 
    select(-c("price", "tot_sold_rev"))
  
  #-------------
  fishery_vars.v <- c("Number of fishers" = "tot_fishers", 
                      "Pounds kept" = "tot_lbs_kept", 
                      "Number of fishers that sold uku" = "tot_fishers_sold", 
                      "Fishers sold ratio" = "fishers_sold_ratio",
                      "Pounds sold" = "tot_lbs_sold", 
                      "Price (2021 $)" = "Real_price", 
                      "Revenue (2021 $)" = "Real_tot_sold_rev", 
                      "CPUE" = "CPUE")
  
  indicators.v <- setdiff(names(final.df), c(fishery_vars.v, "Year", "Species"))
  
  species.v <- unique(final.df$Species)
  
  
  
  #-------------
  
  corr_granger.l <- lapply(1:length(fishery_vars.v), FUN = function(f){
    
    print(f)
    
    fishery_var <- unname(fishery_vars.v[f])
    fishery_lab <- names(fishery_vars.v[f])
    
    granger.df <- calc_granger_causality.f( df = final, 
                                            indicators.vector = indicators.v,
                                           species.vector = species.v,
                                           fishery_variable = fishery_var) %>% 
      filter(!is.na(pval))
    
    species_with_correlations.v <- unique(granger.df$species)
    other_species_with_correlations.v <- setdiff(species_with_correlations.v, "uku")
    
    # Apply first differencing and calc corr
    correlation.df <- final.df %>% 
      select(Year, Species, fishery_var) %>% 
      # We need the data to be in a wide format 
      spread(Species, fishery_var) %>% 
      # left_join with the indicator variables
      left_join(final.df %>% 
                  select(Year, indicators.v) %>% 
                  unique()) %>% 
      # drop Year variable 
      select(-Year) %>% 
      # apply first differencing to get unbiased estimates of correlation
      apply(2, diff) %>% 
      data.frame() %>% 
      # Remove columns with only NA
      select_if(~!all(is.na(.))) %>%
      cor(use = "complete.obs") %>% 
      data.frame() %>% 
      rownames_to_column("Indicator") %>% 
      # we just want to keep the corr coef for the indicators and the species 
      filter(Indicator %in% str_replace(indicators.v, " ", ".")) %>% 
      select(Indicator, str_replace(species_with_correlations.v, " ", ".")) %>% 
      gather(Species, Correlation, str_replace(species_with_correlations.v, " ", ".")) %>% 
      left_join(granger.df %>% 
                  select(Species = species, Indicator = indicator, sig) %>% 
                  mutate(Species = str_replace(Species, " ", "."),
                         Indicator = str_replace(Indicator, " ", "."))) %>% 
      mutate(Correlation = round(Correlation, digits = 2),
             Species = str_replace(Species, "\\.", " "),
             Indicator = str_replace(Indicator, "\\.", " "),
             Fishery_var = fishery_var) %>% 
      mutate(Indicator = ifelse(Indicator == "Diesel_HI_2021", "HI Diesel (2021$)", 
                                ifelse(Indicator == "Diesel_US_2021", "US Diesel (2021$)",
                                       Indicator)))

    # Plotting the correlations 
    # point graphs 
    ggplot(correlation.df, aes(Indicator, Correlation)) + 
      geom_point(aes(color = Species, shape = sig, size = sig), size = 6) + 
      xlab("") + 
      ylab("") +
      labs(title = fishery_lab,
           caption = paste0("The correlation coefficient for uku and each indicator are represented by the red points. The distribution of \n ", 
                            "correlation coefficients for ", paste(other_species_with_correlations.v, collapse = ", "), " are presented as boxplots.")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 20),
            axis.text.y = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            plot.title = element_text(size = 30),
            plot.caption = element_text(size = 20, hjust = 0, face= "italic"),
            plot.caption.position = "plot",
            plot.margin = margin(1, 1, 1, 1, "cm"))
    ggsave(file.path("Results", "correlation", paste0("corr_point_", fishery_var, ".png")),
           height = 15, width = 15)
    
    #-------------
    # distributional graphs 
    other_species.df <- correlation.df %>% 
      filter(Species %in% other_species_with_correlations.v)
    
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
                            "correlation coefficients for ", paste(other_species_with_correlations.v, collapse = ", "), " are presented as boxplots.")) +
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
    ggsave(file.path("Results", "correlation", paste0("corr_distribution_", fishery_var, ".png")),
           height = 15, width = 15)
    
    return(correlation.df)
    
  }) #end of fishery_var lapply

  
  corr_granger.df <- do.call(rbind, corr_granger.l)

  saveRDS(corr_granger.df, file.path("Data", "corr_granger.rds"))   
  
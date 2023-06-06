
library(tidyverse)
library(dplyr)
library(readxl)

rm(list=ls())

source(file.path("hlpr_func", "hlprfnx_price_rev_analysis.R"))
source(file.path("hlpr_func", "hlpr_func_standardize_group_variables.R"))

  price.df <- readRDS(file.path("Data", "final.rds")) %>% 
    select(Year, Species, price2021 = Real_price, rev2021 = Real_tot_sold_rev) %>% 
    standardize_group_variables.f(grouping_var = "Species", stdz_var = "rev2021")
  
#--------
# Price trends for each species
  creating_money_trends.f(df = price.df, 
                          money_var = c("Price per pound (2021 $)" = "price2021"))
  
  creating_money_trends.f(price.df, 
                          money_var = c("Standardized Revenue (2021 $)" = "stdz_rev2021"))
  
  #PMUS is excluded from this because they make a ton more revenue then the others
  creating_money_trends.f(df = price.df %>% filter(Species != "PMUS"), 
                          money_var = c("Revenue (2021 $)" = "rev2021"))
  
#--------
# Price trends of uku against boxplot 
  uku_price.df <- price.df %>% 
    filter(Species == "uku")
  
  other_price.df <- price.df %>% 
    filter(Species != "uku")
  
  ggplot() +
    geom_boxplot(data = other_price.df, aes(as.character(Year), price2021)) + 
    geom_line(data = uku_price.df, aes(x = as.character(Year), y = price2021, group = 1), size = 2, color = "red") + 
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) + 
    xlab("Year") + 
    ylab("Price per pound (2021 $)") + 
    labs(title = "Price trends",
         caption = paste0("Price trends are given in 2021 $. \n", 
                          "The price trend for uku is denoted by the red line. ",
                          "Distribution of prices for \n", 
                          paste(unique(other_price.df$Species), collapse = ", "), 
                          " are given by the boxplots")) + 
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
  ggsave(file.path("Results", "Price", paste0("price_boxplot.png")),
         height = 15, width = 15)  
  
  
  
#--------
# Correlation plots 
  species.v <- unique(price.df$Species) %>% setdiff("uku")
  
  granger.l <- lapply(1:length(species.v), FUN = function(s){
    one_species <- species.v[s]
    
    one_species.df <- price.df %>% 
      filter(Species %in% c("uku", one_species)) %>% 
      mutate(Species = ifelse(Species == one_species, "other", Species)) %>% 
      select(Year, Species, price = price2021) %>% 
      spread(Species, price)
    
    granger.reg <- lmtest::grangertest(one_species.df$uku ~ one_species.df$other)$`Pr(>F)` %>% 
      na.omit() %>% round(digits = 3)
    
    granger.df <- data.frame(
      Species = one_species,
      pval = granger.reg,
      sig = ifelse(granger.reg <= 0.05, "Significant Predictor", "Not Significant"))
    
    return(granger.df)
    
  })
  
  granger.df <- do.call(rbind, granger.l)
  
  corr.df <- price.df %>% 
    select(Year, Species, price2021) %>% 
    spread(Species, price2021) %>% 
    select(-Year) %>% 
    apply(2, diff) %>% 
    data.frame() %>% 
    cor(use = "complete.obs") %>% 
    data.frame() %>% 
    rownames_to_column("Species") %>% 
    # we just want to keep the corr coef for the indicators and the species 
    mutate(Species = str_replace(Species, "\\.", " ")) %>% 
    select(Species, Correlation = uku) %>% 
    filter(Species != "uku") %>% 
    left_join(granger.df) %>% 
    arrange(Correlation)
  
  # Plotting the correlations 
  # point graphs 
  ggplot(corr.df, aes(Correlation, Species)) + 
    geom_point(aes(color = Species, shape = sig, size = sig), size = 10) + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    xlab("") + 
    ylab("") +
    ylim(corr.df$Species) + 
    labs(title = "Price correlation coefficients",
         caption = paste0("The correlation coefficient for uku and each species: ", paste(species.v, collapse = ", "))) +
    scale_shape(name = "Predictive Indicator",
                limits = c("Not Significant", "Significant Predictor")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 20),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 30),
          plot.caption = element_text(size = 20, hjust = 0, face= "italic"),
          plot.caption.position = "plot",
          plot.margin = margin(1, 1, 1, 1, "cm"))
  ggsave(file.path("Results", "price", paste0("corr_price.png")),
         height = 15, width = 15)

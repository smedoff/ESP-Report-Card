
library(tidyverse)
library(dplyr)
library(readxl)
library(corrplot)

rm(list=ls())

source(file.path("hlpr_func", "hlprfnx_calc_granger_causality.R"))

  # ID'ing participation and effort vars
  participation.v <- c("tot_fishers" = "Commercial fishers reporting catch", 
                       "tot_fishers_sold" = "Number of fishers reporting sales", 
                       "fishers_sold_ratio" = "Fishers sold ratio",
                       "Trips" = "Trips")
  
  effort.v <- c("tot_lbs_sold" = "Pounds sold", 
                "Real_price" = "Price (2021 $)", 
                "Real_tot_sold_rev" = "Revenue (2021 $)")
  
  # Creating the uku df
  uku.df <- readRDS(file.path("Data", "final.rds")) %>%  
    filter(Species == "uku") %>%
    select(Year, names(participation.v), names(effort.v)) 
  
  # Calculating the grnager causality
  granger.l <- lapply(1:length(participation.v), FUN = function(f){
    
    fishery_var <- names(participation.v[f])
    fishery_lab <- unname(participation.v[f])
    
    calc_granger_causality.f(df = uku.df %>% mutate(Species = "uku"),
                             indicators.vector = names(effort.v),
                             species.vector = "uku",
                             fishery_variable = fishery_var)
  })
  
  granger.df <- do.call(rbind, granger.l) %>% 
    mutate(Participation = recode(fish_var, !!! participation.v),
           Effort = recode(indicator, !!! effort.v)) %>% 
    select(Participation, Effort, pval, sig)
  
  # calculating correlation and left joiing with granger causality
  corr.df <- uku.df %>% 
    select(-Year) %>% 
    apply(2, diff) %>% 
    data.frame() %>% 
    cor(use = "complete.obs") %>% 
    data.frame() %>% 
    rownames_to_column("Participation") %>% 
    select(Participation, names(effort.v)) %>% 
    setNames(c("Participation", unname(effort.v))) %>% 
    filter(Participation %in% names(participation.v)) %>% 
    mutate(Participation = recode(Participation, !!! participation.v))%>% 
    gather(Effort, Correlation, unname(effort.v)) %>% 
    left_join(granger.df)
  
  # Plotting correlation coef
  ggplot(corr.df, aes(Effort, Correlation)) + 
    geom_point(aes(color = Participation), size = 8) +
    theme(axis.text.x = element_text(angle = 45, 
                                     vjust = 1, 
                                     hjust = 1, 
                                     size = 40),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 40),
          axis.title.y = element_text(size = 40),
          legend.text = element_text(size = 30),
          legend.title = element_blank(),
          plot.title = element_text(size = 40),
          plot.caption = element_text(size = 40, hjust = 0, face= "italic"),
          plot.caption.position = "plot",
          plot.margin = margin(1, 1, 1, 1, "cm"))
  ggsave(file.path("Results", "just uku", "participation and effort plot.png"),
         height = 15, width = 30)
  
  # Constructing granger causality/ correlation table
  corr.df %>% 
    mutate(Correlation = cut(Correlation, 
                         breaks = seq(0,1, by = 0.3), 
                         labels = c("-", "Positive", "Strongly Positive"),
                         include.lowest = TRUE),
           sig = recode(sig, 
                        "Not Significant" = "",
                        "Significant Predictor" = "*"),
           Ranking = paste0(Correlation, sig)) %>% 
    select(Participation, Effort, Ranking) %>% 
    spread(Effort, Ranking) %>% 
    gt() %>% 
    tab_header(title = "Participation and Effort") %>% 
    tab_footnote(footnote = paste0("The signs of the correlation coefficient, denoted corr, are presented. A 'Strongly Positive' ", 
                                   "ranking indicates corr > 0.66. A 'Positive' ranking indicates ",
                                   "0.33 < corr < 0.66. All other values are Null, (-). ", 
                                   "(*) indicate statistically significant predictive ", 
                                   "indicators using the Granger Causality Test.")) %>% 
    data_color(columns = unname(effort.v),
               colors = scales::col_factor(
                 palette = c("white", "white", "#69cfd5", "#69cfd5", "lightcyan", "lightcyan"),
                 domain = c("Positive", "Positive*", "Strongly Positive", 
                            "Strongly Positive*", "-", "-*"))) %>% 
    gtsave(file.path("Results", "just uku", "participation and effort table.png"))
  
  
  

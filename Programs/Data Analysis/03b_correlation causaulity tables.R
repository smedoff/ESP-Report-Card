

library(tidyverse)
library(dplyr)

rm(list=ls())
  
  corr_granger.df <- readRDS(file.path("Data", "corr_granger.rds")) 
  
  corr_granger_labs.df <- corr_granger.df %>% 
    mutate(Correlation = ifelse(Correlation >=0.3, "Positive",
                                ifelse(Correlation <= -0.3, "Negative", 
                                       "-")),
           sig = recode(sig, 
                        "Not Significant" = "",
                        "Significant Predictor" = "*"),
           Ranking = paste0(Correlation, sig)) %>% 
    select(Indicator, Species, Fishery_var, Ranking) %>% 
    mutate(Ranking = factor(Ranking, 
                            levels = c("Negative", "Negative*", 
                                       "Positive", "Positive*", 
                                       "-", "-*")),
           Fishery_var = recode(Fishery_var,
                                "tot_fishers" = "Fishers",
                                "tot_lbs_kept" = "Pounds Kept",
                                "tot_fishers_sold" = "Sold Fishers",
                                "fishers_sold_ratio" = "Sold Fishers Ratio",
                                "tot_lbs_sold" = "Pounds sold",
                                "Real_price" = "Price (2021$)",
                                "Real_tot_sold_rev" = "Revenue (2021$)")) 



#---------
# Creating the table for uku and all indicators correlation VALUES
library(gt)  
#install.packages("webshot2") #to save table
  
  # Full fishery variables 
  uku_correlation.df <- corr_granger.df %>% 
    filter(Species == "uku") %>% 
    select(-c(Species, sig)) %>% 
    mutate(Fishery_var = recode(Fishery_var,
                                "tot_fishers" = "Fishers",
                                "tot_lbs_kept" = "Pounds Kept",
                                "tot_fishers_sold" = "Sold Fishers",
                                "fishers_sold_ratio" = "Sold Fishers Ratio",
                                "tot_lbs_sold" = "Pounds sold",
                                "Real_price" = "Price (2021$)",
                                "Real_tot_sold_rev" = "Revenue (2021$)")) %>% 
    spread(Fishery_var, Correlation) %>% 
    mutate(Indicator = recode(Indicator,
                              "Jobs" = paste0("Natural resources, \n",
                                              "mining, and construction jobs"),
                              "Earnings" = paste0("Earnings: Forestry, \n",
                                                  "Fishing, and Other")))
  
  uku_correlation.df %>% 
    gt() %>% 
    tab_header(title = "Indicator Ranking") %>% 
    tab_footnote(footnote = paste0("The signs of the correlation coefficient are presented ", 
                                   "for the recreational fishery, commercial fishery, and price of uku. (*) indicate ", 
                                   "statistically significant predictive indicators using the Granger Causality Test. ", 
                                   "Overall rankings identify influential indicators. ", 
                                   "The signs of the correlation coefficient are presented for the recreational fishery, ", 
                                   "commercial fishery, and price of uku. (*) indicate statistically significant predictive ", 
                                   "indicators using the Granger Causality Test. Overall rankings identify influential ", 
                                   "indicators")) %>% 
    data_color(columns = c("CPUE", "Fishers", "Pounds Kept", 
                           "Sold Fishers", "Sold Fishers Ratio", 
                           "Pounds sold", "Price (2021$)", 
                           "Revenue (2021$)"),
               method = "numeric",
               palette = "viridis") %>%
    gtsave(file.path("Results", "tables", "correlations.png"))
  


#---------
# Creating the table for uku and all indicators 
library(gt)  
#install.packages("webshot2") #to save table

# Full fishery variables 
  uku_labs.df <- corr_granger_labs.df %>% 
    filter(Species == "uku") %>% 
    select(-Species) %>% 
    spread(Fishery_var, Ranking) %>% 
    #filter(!(Indicator %in% c("US Diesel (2021$)",
    #                          "GDP",
    #                          "Population",
    #                          "Earnings"))) %>% 
    mutate(Indicator = recode(Indicator,
                              "Jobs" = paste0("Natural resources, \n",
                                              "mining, and construction jobs"),
                              "Earnings" = paste0("Earnings: Forestry, \n",
                                                  "Fishing, and Other")))

  uku_labs.df %>% 
    gt() %>% 
    tab_header(title = "Indicator Ranking") %>% 
    tab_footnote(footnote = paste0("The signs of the correlation coefficient are presented ", 
                                   "for the recreational fishery, commercial fishery, and price of uku. (*) indicate ", 
                                   "statistically significant predictive indicators using the Granger Causality Test. ", 
                                   "Overall rankings identify influential indicators. ", 
                                   "The signs of the correlation coefficient are presented for the recreational fishery, ", 
                                   "commercial fishery, and price of uku. (*) indicate statistically significant predictive ", 
                                   "indicators using the Granger Causality Test. Overall rankings identify influential ", 
                                   "indicators")) %>% 
    data_color(columns = c("CPUE", "Fishers", "Pounds Kept", 
                           "Sold Fishers", "Sold Fishers Ratio", 
                           "Pounds sold", "Price (2021$)", 
                           "Revenue (2021$)"),
               colors = scales::col_factor(
                 palette = c("white", "white", "#69cfd5", "#69cfd5", "lightcyan", "lightcyan"),
                 domain = c("Negative", "Negative*", "Positive", "Positive*", "-", "-*"))) %>%
    gtsave(file.path("Results", "tables", "ranking.png"))
  
  
# Summarized across fishery variables 
  corr_granger.df %>% 
    filter(Species == "uku") %>% 
    select(-Species) %>% 
    group_by(Indicator) %>% 
    summarize(Correlation = round(mean(Correlation, na.rm = TRUE), digits = 2)) %>% 
    mutate(Ranking = ifelse(Correlation >=0.3, "Positive",
                                ifelse(Correlation <= -0.3, "Negative", 
                                       "-"))) %>% 
    rename("Average Correlation" = "Correlation") %>% 
    gt() %>% 
    tab_header(title = "Indicator Ranking") %>% 
    tab_footnote(footnote = paste0("Correlations are averaged across fishery variable. ",
                                   "Average correlations are presented in the second column. The third column ",
                                   "gives a value of positive (cor > 0.3), negative (cor < -0.3), and null (otherwise)")) %>% 
    data_color(columns = c("Ranking"),
               colors = scales::col_factor(
                 palette = c("white", "white", "#69cfd5", "#69cfd5", "lightcyan", "lightcyan"),
                 domain = c("Negative", "Negative*", "Positive", "Positive*", "-", "-*"))) %>%
    tab_options(table.width = 15,
                table.margin.left = 6,
                table.margin.right = 6) %>% 
    gtsave(file.path("Results", "tables", "summary_ranking.png"))

#---------
# creating the table for most influential indicators and all species 
#' Look at the table in Results/tables/rankings.png to see which indicators 
#' influence uku the most. then create tables for those indicators that 
#' rank them against the whole species group 
#' Positive: Earnings, GDP, Visitor Arrivals
#' Negative: Unemployment Rate

  uku_corr.df <- corr_granger.df %>% 
    filter(Species == "uku") %>% 
    arrange(Correlation) %>% 
    mutate(Fishery_var = recode(Fishery_var,
                                "tot_fishers" = "Fishers",
                                "tot_lbs_kept" = "Pounds Kept",
                                "tot_fishers_sold" = "Sold Fishers",
                                "fishers_sold_ratio" = "Sold Fishers Ratio",
                                "tot_lbs_sold" = "Pounds sold",
                                "Real_price" = "Price (2021$)",
                                "Real_tot_sold_rev" = "Revenue (2021$)"))
  
  
  top3_lowest.df <- uku_corr.df %>% 
    filter(Correlation < 0) %>% 
    arrange(Correlation) %>% 
    slice(1:3)
  
  top3_highest.df <- uku_corr.df %>% 
    filter(Correlation > 0) %>% 
    arrange(desc(Correlation)) %>% 
    slice(1:3)
  
  strongest_indic.df <- rbind(top3_lowest.df, top3_highest.df)
  
  lapply(1:nrow(strongest_indic.df), FUN = function(i){
    
    print(i)
    
    one_indic <- strongest_indic.df[i, ]
    
    strong_ind.df <- corr_granger.df %>% 
      mutate(Fishery_var = recode(Fishery_var,
                                  "tot_fishers" = "Fishers",
                                  "tot_lbs_kept" = "Pounds Kept",
                                  "tot_fishers_sold" = "Sold Fishers",
                                  "fishers_sold_ratio" = "Sold Fishers Ratio",
                                  "tot_lbs_sold" = "Pounds sold",
                                  "Real_price" = "Price (2021$)",
                                  "Real_tot_sold_rev" = "Revenue (2021$)"),
             Corr_lab = ifelse(Correlation >=0.3, "Positive",
                                  ifelse(Correlation <= -0.3, "Negative", 
                                         "-")),
             sig = recode(sig, 
                          "Not Significant" = "",
                          "Significant Predictor" = "*"),
             Ranking = paste0(Corr_lab, sig)) %>% 
      filter(Indicator == one_indic$Indicator,
             Fishery_var == one_indic$Fishery_var) %>% 
      select(Species, Correlation, Ranking) 
    
    #-----------
    # Full fishery variables
    strong_ind.df %>% 
      gt() %>% 
      tab_header(title = paste0(one_indic$Indicator, " and ", one_indic$Fishery_var)) %>% 
      tab_footnote(footnote = paste0("Correlation coefficients between ", one_indic$Indicator, " and ", one_indic$Fishery_var, 
                                     " are presented for each species group. ",
                                     "Overall rankings are given a Positive if corr > 0.3, Negative if corr < -0.3, and (-) otherwise. ", 
                                     "A (*) indicate statistically ", 
                                     "significant predictive indicators using the Granger Causality Test.")) %>% 
      data_color(columns = "Ranking",
                 colors = scales::col_factor(
                   palette = c("white", "white", "#69cfd5", "#69cfd5", "lightcyan", "lightcyan"),
                   domain = c("Negative", "Negative*", "Positive", "Positive*", "-", "-*"))) %>%
      tab_options(table.width = pct(27)) %>% 
      gt::gtsave(file.path("Results", "tables", paste0("ranking_", 
                                                       one_indic$Indicator, "_",
                                                       one_indic$Fishery_var,
                                                       ".png")))
    
    
  })
  


#---------
# This method is more useful for a R shiny project
#  library(DT)  

#  format_table.f <- function(dT, fishing_var){
#    dT %>% 
#      formatStyle(columns = fishing_var,
#                  background = styleEqual(c("Positive", "Positive*",
#                                            "Negative", "Negative*",
#                                            "Null", "Null*"),
#                                          c("pink", "pink",
#                                                  "lightblue", "lightblue",
#                                                  "gray", "gray")))}

#  rank.dT <- corr_granger.df %>% 
#    datatable(rownames = FALSE) %>% 
#    format_table.f(fishing_var = "Fishers") %>% 
#    format_table.f(fishing_var = "Pounds Kept") %>% 
#    format_table.f(fishing_var = "Sold Fishers") %>% 
#    format_table.f(fishing_var = "Sold Fishers Ratio") %>% 
#    format_table.f(fishing_var = "Pounds sold") %>% 
#    format_table.f(fishing_var = "Price (2021$)") %>% 
#    format_table.f(fishing_var = "Revenue (2021$)")

#  print(rank.dT)

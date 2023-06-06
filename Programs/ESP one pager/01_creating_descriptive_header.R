

library(dplyr)
library(tidyverse)

overview <- c("This report card provides an update on the uku fishery from 2022 for the fishery scientists ", 
              "and managers in Hawai‘i.") %>% 
  paste0(collapse = " \n ")

fish_obs <- #"Fisher observations: "
              c("- Fishers reported less uku targeting due to a productive ‘ahi and onaga season",
              "- Abundance of uku at select O‘ahu bottomfishing spots",
              "- Unusually large aggregation of uku late season (October) on the Penguin Banks",   
              "- Shark depredation remains an issue")%>% 
  paste0(collapse = " \n ")

sum_findings <- #"Summary findings and meethods:"
                c(#"- 73% of uku was caught by recreational fisherman in 2021",
                  "- Beginning in 2022, recreational or non-commercial catch was included in the annual catch ", 
                  "limit.",
                  "- Our exploratory analyses track the non-commercial fishery against ecosystem and socioeconomic ", 
                  "indicators. ",
                  "- We ranked relevant indicators using a low, medium, and high are given based on their level ", 
                  "of influence.",
                  "- Influence was measured using correlation and Granger Causality Test.",
                  "- Correlation coefficients measure how the indicator moves with fishery metrics (e.g., catch, ", 
                  "participation). ",
                  "- The Granger Causality Test gauges the predictive power each indicator's lagged values has on ", 
                  "fishery metrics. ",
                  "- These ecosystem indicators track the interconnectivity of the uku fishery with other fisheries, ", 
                  "both pelagic and ", 
                  "archipelagic in a cross fishery dependence analysis.")%>% 
  paste0(collapse = " \n ")

header_text <- 6
info_text <- 4


h_txt <- ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggplot2::annotate("text", x = 1, y = 1, 
                    label = "About: ", 
                    vjust = -20, hjust = 0, 
                    size = header_text, fontface = "bold") +
  ggplot2::annotate("text", x = 1, y = 1, 
                    label = overview, 
                    vjust = -8.5, hjust = 0, 
                    size = info_text) +
  ggplot2::annotate("text", x = 1, y = 1, 
                    label = "Fisher observations: ", 
                    vjust = -14, hjust = 0, 
                    size = header_text, fontface = "bold") +
  ggplot2::annotate("text", x = 1, y = 1, 
                    label = fish_obs, 
                    vjust = -1.8, hjust = 0, 
                    size = info_text) +
  ggplot2::annotate("text", x = 1, y = 1, 
                    label = "Summary findings and methods: ", 
                    vjust = -5, hjust = 0, 
                    size = header_text, fontface = "bold") +
  ggplot2::annotate("text", x = 1, y = 1, 
                    label = sum_findings, 
                    vjust = .8, hjust = 0, 
                    size = info_text) +
  xlim(1, 1.5) 

  saveRDS(h_txt, file.path("Programs", "ESP one pager", "h_txt.rds"))
  
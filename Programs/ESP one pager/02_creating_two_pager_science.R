##############
#' Purpose: Create a one page ESP report
#' Author: Sarah Medoff
#' 
#' This code will produce a one page ESP report for UKU
#' All figures are made externally in other scripts and saved in the Final/Figures folder
#' The code will call on these pngs and knit them into a pdf Final/Uku.pdf
##############


library(dplyr)
library(tidyverse)

rm(list=ls())

  
  footnote_text <- "Point of Contact: Adam Ayers (adam.ayers@noaa.gov)"

  # header ----
  head <- png::readPNG(file.path("Programs", "ESP one pager", "Images", 
                                 "header_uku.png"))

  header <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 1 / 4) +
    ggpubr::background_image(head)

  # text header ----
  h_txt <- readRDS(file.path("Programs", "ESP one pager", "h_txt.rds")) +
    ggplot2::theme(aspect.ratio = 10/ 10) 

  # text footer ----
  f_txt <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotate("text", x = 1, y = 1, label = footnote_text) +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  
  summary_txt <- ggpubr::ggarrange(header,
                                  h_txt,
                                  f_txt,
                                  nrow = 3,
                                  heights = c(1.5, 4, .5)
  )
  print(summary_txt)
  ggplot2::ggsave(file.path("Deliverables", "UKU_SCI_PG1.pdf"),
                  width = 8.5,
                  height = 11
  )
  
  
  
  #-------------------------
  # Figures Page 
  #-------------------------
  
#  fish_obs <- #"Fisher observations: "
#    c("- Fishers reported less uku targeting due to a productive ‘ahi and onaga season",
#      "- Abundance of uku at select O‘ahu bottomfishing spots",
#      "- Unusually large aggregation of uku late season (October) on the Penguin Banks",   
#      "- Shark depredation remains an issue")%>% 
#    paste0(collapse = " \n ")
  
#  h_txt <- ggplot2::ggplot() +
#    ggplot2::theme_void() +
#    ggplot2::annotate("text", x = 1, y = 1, hjust = 0, label = fish_obs, size = 3) +
#    xlim(1, 1.5) 
    
  
  # 1 figure ----
  one_fig <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 1.8 / 2) +
    #ggpubr::background_image(jpeg::readJPEG(file.path("..",
    ggpubr::background_image(png::readPNG(file.path("Results",
                                                    "Tables",
                                                    "ranking.png")))
  
  
  # 2 figure ----
  two_fig <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 1.8 / 2) +
    ggpubr::background_image(png::readPNG(file.path("Results",
                                                    "correlation",
                                                    "corr_distribution_Real_price.png")))
  
  
  # 3 fig ----
  three_fig <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 1.8 / 2) +
    ggpubr::background_image(png::readPNG(file.path("Results",
                                                    "just uku",
                                                    "cpue",
                                                    "timetrends_cpue_Earnings.png")))
  
  
  # 4 fig ----
  four_fig <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 1.8 / 2) +
    ggpubr::background_image(png::readPNG(file.path("Results",
                                                    "time trends",
                                                    "averages",
                                                    "time_trends_Trips_GDP.png")))
  
  
  # right hand figures - score and three_fig ----
  
  # all figs ----
  figs <- ggpubr::ggarrange(
    one_fig,
    two_fig,
    three_fig,
    four_fig,
    nrow = 2,
    ncol = 2
  )
  # put it all together ----
  summary_fig <- ggpubr::ggarrange(header,
                                   #h_txt,
                                  figs,
                                  f_txt,
                                  #nrow = 4,
                                  #heights = c(2, 1, 5.5, .5)
                                  nrow = 3,
                                  heights = c(2, 5.5, .5)
  )
  print(summary_fig)
  ggplot2::ggsave(file.path("Deliverables", "UKU_SCI_PG2.pdf"),
                  width = 8.5,
                  height = 11
  )

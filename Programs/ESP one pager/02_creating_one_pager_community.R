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

  header_text <-   header_text <- c("   ",
                                    "This report card provides an update on the uku fishery from 2022 for the Hawai‘i fishing community.",
                                    "   ",
                                    "Fisher observations: ",
                                    "- Fishers reported less uku targeting due to a productive ‘ahi and onaga season",
                                    "- Abundance of uku at select O‘ahu bottomfishing spots",
                                    "- Unusually large aggregation of uku late season (October) on the Penguin Banks",  
                                    "- Shark depredation remains an issue",
                                    "   ",
                                    "Summary findings and methods:",
                                    "- Our report card ranks 8 fishery relevant categories",
                                    "- In 2021, there was a −30% change in commercial landings relative to the historical mean (86,121.47 lbs)",
                                    "- The average catch from the recreational fishery is 94,755 lbs",
                                    "- The average wholesale price for uku in 2022 was $3.14(2000 dollars), a 13% increase the historical mean ($2.77). ",
                                    "- Fishers took 1006 commercial trips for uku in 2021, a 30% decrease from the historical mean (1,427.47 trips). ",
                                    "- 278 commercial licenses targeted uku in 2021, a 24% decrease from the historical mean (365.14).",
                                    "   ",
                                    "    ") %>% 
                                paste0(collapse = " \n ")
  
  footnote_text <- "Point of Contact: Adam Ayers (adam.ayers@noaa.gov)"
  

  
  # 1 figure ----
  one_fig <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    # aspect.ration = height/width
    ggplot2::theme(aspect.ratio = 1.4 / 2) +
    ggpubr::background_image(png::readPNG(file.path("Results",
                                                    "time trends",
                                                    "averages",
                                                    "time_trends_Real_price_Earnings.png")))
  

  # 2 figure ----
  two_fig <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 1.4 / 2) +
    ggpubr::background_image(png::readPNG(file.path("Results",
                                                    "rec_com_bar.png")))

  # 3 fig ----
  three_fig <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 2 / 2.2) +
    ggpubr::background_image(png::readPNG(file.path("Results",
                                                    "price",
                                                    "price2021.png")))
  
  
  # 4 fig ----
  four_fig <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 2 / 2.2) +
    #ggpubr::background_image(jpeg::readJPEG(file.path("..",
    ggpubr::background_image(png::readPNG(file.path("Results",
                                                    "tables",
                                                    "ranking_Visitor Arrivals_CPUE.png")))

# right hand figures - score and three_fig ----
  #right_figs <- ggpubr::ggarrange(
  #  one_fig,
  #  two_fig,
  #  nrow = 2
  #)

  # all figs ----
  figs <- ggpubr::ggarrange(
    one_fig,
    two_fig,
    three_fig,
    four_fig,
    ncol = 2,
    nrow = 2
  )

  # header ----
  head <- png::readPNG(file.path("Programs", "ESP one pager", "Images", 
                                 "header_uku.png"))

  header <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 1 / 4) +
    ggpubr::background_image(head)

  # text header ----
  h_txt <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotate("text", x = 1, y = 1, hjust = 0, label = header_text, size = 2.5) +
    xlim(1, 1.5) 

  # text footer ----
  txt <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::annotate("text", x = 1, y = 1, label = footnote_text) +
    ggplot2::theme(text = ggplot2::element_text(size = 6))

  # put it all together ----
  summary_pg <- ggpubr::ggarrange(header,
    h_txt,
    figs,
    txt,
    nrow = 4,
    heights = c(2, 2, 5.5, .5)
  )
  print(summary_pg)
  ggplot2::ggsave(file.path("Deliverables", "UKU_community.pdf"),
    width = 8.5,
    height = 11
  )



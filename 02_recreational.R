

library(tidyverse)
rm(list=ls())

source(file.path("hlpr_func", "hlprfnc_creating_df.R"))
  
  #---------------
  # Pull data in from the stock assessment reports and compile them into a data frame 
  # https://repository.library.noaa.gov/view/noaa/24190 
  # This data records landings in metric tons for commercial and recreational 
  # Just use this for the non-comerical info 
  
  # Defining variable names for the data set 
  # Commercial variables: Deep-Sea Handline, Inshore Handline, Trolling, Other Gears
  # Recreational variables: Recreational
  variables.v <- c("Year", "Deep-Sea Handline",
                   "Inshore handline",
                   "Trolling", "Other Gears", "Recreational")
  
  # Bringing in the data 
  file_name <- "T3_stock_assessment.csv"
  com_rec_catch.v <- read_csv(file.path("Data",
                                        file_name),
                              col_names = FALSE) %>% pull(X1) 
  
  # Compiling data 
  com_rec_catch.l <- lapply(1:length(variables.v),
                            FUN = creating_df.f,
                            csv.v = com_rec_catch.v,
                            ncolumns = 6,
                            nrows =70,
                            column_names.v = variables.v)
  
  com_rec_catch.df <- do.call(cbind, com_rec_catch.l)

  
  #-------
  # Stacked bar graph
  
  # summarize commercial catch across gear types
  com_rec_catch_agg.df <- com_rec_catch.df %>% 
    select(-Recreational) %>%
    mutate(Commercial = rowSums(across(-Year), na.rm = TRUE)) %>% 
    select(Year, Commercial) %>% 
    left_join(com_rec_catch.df %>% 
                select(Year, Recreational)) %>% 
    gather(Fishery, Catch, Commercial, Recreational) %>% 
    mutate(Catch = round(Catch*2204.62/10000, digits =2),
           Fishery = factor(Fishery, levels = c("Recreational", "Commercial")))
  
  percent.df <- com_rec_catch_agg.df %>% 
    group_by(Year) %>% 
    summarise(Total_catch = sum(Catch, na.rm = TRUE)) %>% 
    right_join(com_rec_catch_agg.df) %>% 
    mutate(Percent = paste0(round(Catch/Total_catch*100, digit = 1), "%"),
           Fishery = recode(Fishery, 
                            "Commercial" = "CML reports",
                            "Recreational" = "MRIP estimates")) %>% 
    select(-Total_catch) 

  
  ggplot(percent.df %>% filter(Year >= 2010), 
         aes(x = Year, y = Catch, fill = Fishery)) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = Percent), 
              position = position_stack(vjust = 0.5), size = 10) +
    ylab("Catch (10,000Lbs)") +
    labs(title = "Total uku catch (2012 - 2021)") + 
    theme(axis.title.y = element_text(size = 50),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 35),
          axis.text.x = element_text(size = 35),
          plot.title = element_text(size = 50),
          legend.text = element_text(size = 50),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.key.width = unit(5,"cm"))
  ggsave(file = file.path("Results", "rec_com_bar.png"), 
         device = "png", width = 30, height = 15, dpi = 72)
  
  
  
  

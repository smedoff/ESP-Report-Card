
# The point of this script is to scrape the following websites for diesel prices

rm(list = ls())

#-----------------------------
# Jon S. Data Source 
  # - https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=emd_epd2d_pte_nus_dpg&f=m

library(rio)
library(janitor)
library(dplyr)
library(tidyverse)

  url1.xls <- "https://www.eia.gov/dnav/pet/hist_xls/EMD_EPD2D_PTE_NUS_DPGm.xls"
  
  no2_diesel1.df <- rio::import(file = url1.xls,which = 2) 
  
  colnames(no2_diesel1.df) <- c("Date", "Price")
  
  no2_diesel1.df <- no2_diesel1.df %>% 
    slice(-c(1:2)) #get rid of the first 2 row 

  
  dates_as_date_object.v <- janitor::excel_numeric_to_date(as.numeric(no2_diesel1.df$Date))
  
  no2_diesel1.df$Date <- dates_as_date_object.v
  
  no2_diesel1.df$Source <- "EIA - US prices"
  

#-----------------------------  
# Michel's Data Source  
  
  url2.xlsx <- "https://dbedt.hawaii.gov/economic/files/2021/02/Monthly_Energy_Data.xlsx"
  
  no2_diesel2.df <- rio::import(file = url2.xlsx, which = 1) 

  # Reformat the data frame 
  colnames(no2_diesel2.df) <- no2_diesel2.df[3, ]
  
  no2_diesel2.df <- no2_diesel2.df %>% 
    dplyr::filter(Series == "Diesel, State") %>% 
    t() %>% 
    data.frame() %>% 
    add_rownames(var = "Date") %>% 
    rename("Price" = ".") %>% 
    slice(-c(1:2))
  
  
  dates_as_date_object.v <- janitor::excel_numeric_to_date(as.numeric(no2_diesel2.df$Date))
  
  no2_diesel2.df$Date <- dates_as_date_object.v
  
  no2_diesel2.df$Source <- "DBEDT - Hawaii Prices"
  
  
#----------------------------
# Plotting and Saving the data
  
  # Make sure colnames match for rbind
  names(no2_diesel1.df) == names(no2_diesel2.df)
  
  # Combine all data sets
  no2_diesel.df <- rbind(no2_diesel1.df, no2_diesel2.df) %>% 
    mutate(Price = as.numeric(Price))
  
  write.csv(no2_diesel.df, file = file.path(".",
                                            "Data",
                                            "Diesel_final.csv"), 
            row.names=FALSE)
  
  # Produce a time series plot
  ggplot(no2_diesel.df, aes(Date, Price, color = Source)) + 
    geom_line(aes(group = Source, color = Source)) + 
    ylab("Diesel $/Gal") +
    labs(title = "Time Series of Monthly Diesel Prices",
         caption = "US: https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=emd_epd2d_pte_nus_dpg&f=m
                    \n HI: https://dbedt.hawaii.gov/economic/energy-trends-2/") + 
    theme(plot.caption=element_text(hjust = 0))

  
  
  
  
  
  
  

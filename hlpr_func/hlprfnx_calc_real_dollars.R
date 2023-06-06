

adjusting_cpi.f <- function(nominal.df, adj_yr){
  
  nominal_yr.df <- nominal.df %>% 
    group_by(YEAR) %>% 
    summarize(CPI = mean(CPI, na.rm = TRUE))
  
  nominal_adj_yr.df <- nominal.df %>% 
    filter(YEAR == adj_yr)
  
  cpi_adj.df <- nominal.df %>% 
    mutate(CPI_index = nominal_adj_yr.df$CPI/CPI)
  
  return(cpi_adj.df)
}


calc_real_price.f <- function(df, price_var, adj_yr){
  # generate the CPI index
  CPI <- read_csv(file.path("hlpr_func",
                            "UHERO_CPI_FOOD_BEVERAGE.csv")) %>% 
    mutate(YEAR = str_sub(DateTime, -4, -1)) %>% 
    select(YEAR, CPI= Level) %>% 
    adjusting_cpi.f(adj_yr = adj_yr)
  
  real.df <- df %>% 
    left_join(CPI %>% rename(Year = YEAR) %>% mutate(Year = as.numeric(Year))) %>% 
    rename(PRICE_VAR = price_var) %>% 
    mutate(Real_PRICE_VAR = round(PRICE_VAR * CPI_index, digits = 2)) %>% 
    select(-c(CPI, CPI_index)) %>% 
    doBy::renameCol("PRICE_VAR", price_var) %>%
    doBy::renameCol("Real_PRICE_VAR", paste0("Real_", price_var))
  
  return(real.df)
}



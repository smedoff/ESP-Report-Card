
standardize_group_variables.f <- function(df, grouping_var, stdz_var){
  
  df_rename <- df %>% 
    rename(VAR = stdz_var,
           GROUP_VAR = grouping_var)  
  
  df_mean <- df_rename %>% 
    group_by(GROUP_VAR) %>% 
    summarize(mean_VAR = mean(VAR, na.rm = TRUE))
  
  df_sd <- df_rename %>% 
    group_by(GROUP_VAR) %>% 
    summarize(sd_VAR = sd(VAR, na.rm = TRUE))
  
  df_stdz <- df_rename %>% 
    left_join(df_mean) %>% 
    left_join(df_sd) %>% 
    mutate(STDZ_VAR = (VAR - mean_VAR)/sd_VAR) %>% 
    select(-c(mean_VAR, sd_VAR)) %>% 
    doBy::renameCol("STDZ_VAR", paste0("stdz_", stdz_var)) %>% 
    doBy::renameCol("VAR", paste0(stdz_var)) %>% 
    doBy::renameCol("GROUP_VAR", paste0(grouping_var))
  
  return(df_stdz)
}

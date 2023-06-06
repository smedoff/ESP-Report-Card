

  creating_df.f <- function(ith_col,
                            csv.v,
                            ncolumns,
                            nrows,
                            column_names.v){
    
    one_label <- column_names.v[ith_col]
    
    full_index <- 0:nrows 
    
    one_var_index <- ith_col+(full_index*ncolumns)
    one_var.v <- csv.v[one_var_index]
    one_var.df <- data.frame(VAR = one_var.v) %>% 
      doBy::renameCol("VAR", one_label)
    
    return(one_var.df)
  }
  
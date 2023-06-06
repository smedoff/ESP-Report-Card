
#df <- uku.df
#uku_vars.v <- trips_vars.v
#uku_indicators.v <- indicators.v

creating_uku_fishery_time_trends.f <- function(df, uku_vars.v, uku_indicators.v){
  
  # Identifying if its a cpue or trips analysis
  if(str_detect(uku_vars.v[1], "cpue")){
    file_lab <- "cpue"
  }
  
  if(str_detect(uku_vars.v[1], "trips")){
    file_lab <- "trips"
  }
  
  lapply(1:length(uku_indicators.v), FUN = function(i){
    
    indicator <- uku_indicators.v[i]
    
    tt.df <- df %>% 
      select(Year, unname(uku_vars.v), unname(indicator)) %>% 
      gather(Variable, Value, unname(uku_vars.v), indicator) %>% 
      mutate(Value = round(Value, digits = 2)) %>% 
      standardize_group_variables.f(grouping_var = "Variable",
                                    stdz_var = "Value")
    
    ttplot.l <- lapply(1:length(uku_vars.v), FUN = function(c){
      
      cpue_var <- uku_vars.v[c]
      
      title <- names(cpue_var)
      
      tt.p <- tt.df %>% 
        filter(Variable %in% c(unname(indicator), unname(cpue_var))) %>% 
        mutate(Variable = factor(Variable, levels = c(indicator, cpue_var))) %>% 
        ggplot(aes(Year, stdz_Value)) + 
        geom_line(aes(color = Variable, size = Variable, group = Variable)) +
        scale_color_manual(values = c("#009E73", "#CC6666"),
                           labels = c(names(indicator), names(cpue_var))) + 
        scale_size_manual(values = c(2, 1),
                          labels = c(names(indicator), names(cpue_var))) + 
        ylab("Standardized Value") + 
        labs(title = title) + 
        guides(color = guide_legend(nrow = 2)) +
        theme(axis.title = element_text(size = 15),
              axis.title.x = element_blank(),
              plot.title = element_text(size = 25), 
              axis.text = element_text(size = 15),
              legend.text = element_text(size = 15),
              legend.title = element_blank(),
              legend.position = "bottom")
      
      return(tt.p)
      
    }) #end of cpue lapply
    
    names(ttplot.l) <- unname(uku_vars.v)
    
    # Extracting the aggregate cpue plot
    # locate where the agg uku-var value is 
    agg_index <- str_which(uku_vars.v, "agg")
    
    agg_plot.p <- ttplot.l[[agg_index]]  + 
      theme(axis.title = element_text(size = 20),
            axis.title.x = element_blank(),
            plot.title = element_text(size = 30), 
            axis.text = element_text(size = 15),
            legend.text = element_text(size = 15),
            legend.title = element_blank(),
            legend.position = "bottom")
    
    # Extracting the gear specific cpue plots and plotting them on one panel
    ttplot.l[[agg_index]] = NULL
    gear_plot.p <- do.call("grid.arrange", c(ttplot.l, nrow = 2))  
    
    # Plotting on one panel
    ggsave(file = file.path("Results", "just uku", "cpue", paste0("timetrends_", file_lab, "_", indicator, ".png")), 
           gridExtra::arrangeGrob(gear_plot.p, agg_plot.p, ncol = 2), 
           device = "png", width = 20, height = 12, dpi = 72)
    
  }) #end of lapply indicators
  
} #end of function

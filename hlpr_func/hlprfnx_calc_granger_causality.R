

calc_granger_causality.f <- function(df, indicators.vector, species.vector, fishery_variable){
  
  indicator.l <- lapply(1:length(indicators.vector), FUN = function(i){
    
    one_indicator <- indicators.vector[i]
    
    species.l <- lapply(1:length(species.vector), FUN = function(s){
      
      one_species <- species.vector[s]
      
      model.df <- df %>% 
        filter(Species == one_species) %>% 
        select(ONE_INDICATOR = one_indicator, ONE_FVAR = fishery_variable) 
      
      if(all(is.na(model.df$ONE_FVAR))){
        granger.reg <- NA
        sig.reg <- NA
      }else{
        granger.reg <- lmtest::grangertest(model.df$ONE_FVAR ~ model.df$ONE_INDICATOR)$`Pr(>F)` %>% 
          na.omit() %>% round(digits = 3)
        sig.reg <- ifelse(granger.reg <= 0.05, "Significant Predictor", "Not Significant")
      }

      
      granger.df <- data.frame(
        species = one_species,
        indicator = one_indicator,
        fish_var = fishery_variable,
        pval = granger.reg,
        sig = ifelse(granger.reg <= 0.05, "Significant Predictor", "Not Significant"))
      
      return(granger.df)
    }) #end of species lapply
    
    species.df <- do.call(rbind, species.l)
    
  }) #end of indicator lapply
  
  indicator.df <- do.call(rbind, indicator.l)
  
  return(indicator.df)
  
} #end of function

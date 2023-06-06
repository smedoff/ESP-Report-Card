
#df <- price.df
#money_var <- c("Price per pound (2021 $)" = "price2021")

creating_money_trends.f <- function(df, money_var){
  
  # Price trends for each species
  df %>% 
    rename(MONEY_VAR = unname(money_var)) %>% 
  ggplot(aes(Year, MONEY_VAR)) +
    geom_line(aes(color = Species, linetype = Species), size = 2) +
    scale_color_manual(breaks = unique(price.df$Species),
                       values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#661100")) + 
    scale_linetype_manual(breaks = unique(price.df$Species), 
                          values = c("solid", "solid", "solid", "solid", "longdash")) +  
    xlab("Year") + 
    ylim(c(0, 10.2)) +
    ylab(names(money_var)) + 
    labs(title = names(money_var)) + 
    theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1, size = 20),
          axis.text.y = element_text(size = 35),
          axis.title = element_text(size = 50),
          legend.text = element_text(size = 30),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.size = unit(3, "cm"),
          plot.title = element_text(size = 50),
          plot.caption = element_text(size = 20, hjust = 0, vjust = -7, face= "italic"),
          plot.caption.position = "plot",
          plot.margin = margin(1, 1, 2, 1, "cm"))
  ggsave(file.path("Results", "Price", paste0(unname(money_var), ".png")),
         height = 15, width = 30)  
  
}

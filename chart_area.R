chart_area <- function(df, frontier_name, num_assets = 9) {
  ####
  # df: Dataframe
  # frontier_name: string
  # num_assets: integer. Number of assets displayed on legend. 
  
  
  # Load required packages
  library(tidyverse)
  library(ggthemes)
  library(forcats)
  
  # Tidy format
  datos <- df %>%
     gather(key = 'asset', value = 'allocation', -1) %>% 
     arrange(Return)
  
  # Plot
  plot <- datos %>% 
    ggplot(mapping = aes(x = Return, y = allocation)) +
    geom_area(aes(x = Return, y = allocation, fill = fct_reorder(factor(asset), allocation, .desc = TRUE))) +
    theme_minimal(legend.position = "top") +
    scale_fill_gdocs(breaks = c(levels(fct_reorder(factor(datos$asset), datos$allocation, .desc = TRUE))[1:num_assets])) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    guides(fill=guide_legend(title = "Activo")) +
    labs(x = "Retorno",
         title = frontier_name,
         subtitle = "Asset Allocation por nivel de retorno",
         y = "Allocation")
  plot
}
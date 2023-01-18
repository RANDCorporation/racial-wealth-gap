

#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Colors & Fonts

color_a <- "#af61a7"
color_b <- "#ce8644"
four_collor_pallete <- c("#61c6c6", "#ce8644", "#c1536b", "#595a5a")

plot_font <- "Arial"

# Plot the networth of races

networth_race_plot = function(wealth_data, lower_limit, upper_limit){
  
  medians_by_race = wealth_data %>%
    group_by(racecl4) %>%
    summarise(value = median(networth))
  
  
  distr_plot = wealth_data %>%
    ggplot(data = ., mapping = aes(y = networth, x = racecl4, fill = racecl4)) + 
    geom_violin(adjust = 3) + 
    geom_point(data = medians_by_race, mapping = aes(x = racecl4, y = value)) + 
    xlab("Race") + 
    ylab("Net Worth") + 
    randplot::theme_rand(font = plot_font)+ 
    scale_fill_brewer(palette = "Dark2", name = "Race")
  
  format_ggplot_y_continuous_scale(plot = distr_plot, number_format = "dollar", limits = c(lower_limit,upper_limit))
}



# Formatting functions ----------------------------------------------------

format_for_humans <- function(x, digits = 2){
  grouping <- pmax(floor(log(abs(x), 1000)), 0)
  paste(signif(x / (1000 ^ grouping), digits = digits),
        c('', 'K', 'M', 'B', 'T')[grouping + 1],sep = " ")
}

format_percentage_for_humans = function(x, digits = 2){
  paste(signif(100 * x, digits), "%", sep = " ")
}

format_currency_for_humans = function(x, currency = "$", digits = 2) {
  paste(currency, format_for_humans(x, digits = digits), sep = " ")
}

format_ggplot_y_continuous_scale = function(plot, number_format = "number", log_scale = F, limits) {
  
  if(log_scale) {
    plot = plot + ggplot2::scale_y_log10()
  }
  
  if(number_format == "number") {
    plot = plot + ggplot2::scale_y_continuous(labels = format_for_humans, limits = limits)
  } else if (number_format == "percent"){
    plot = plot + scale_y_continuous(labels = scales::percent)
    # Alternatively, I could use my function:
    #plot = plot + ggplot2::scale_y_continuous(labels = format_percentage_for_humans)
  } else if (number_format == "dollar"){
    plot = plot + ggplot2::scale_y_continuous(labels = format_currency_for_humans, limits = limits)
  }
  
  plot
  
}

format_ggplot_x_continuous_scale = function(plot, number_format = "number", log_scale = F, limits) {
  
  if(log_scale) {
    plot = plot + ggplot2::scale_y_log10()
  }
  
  if(number_format == "number") {
    plot = plot + ggplot2::scale_x_continuous(labels = format_for_humans, limits = limits)
  } else if (number_format == "percent"){
    plot = plot + scale_y_continuous(labels = scales::percent)
  } else if (number_format == "dollar"){
    plot = plot + ggplot2::scale_x_continuous(labels = format_currency_for_humans, limits = limits)
  }
  
  plot
  
}

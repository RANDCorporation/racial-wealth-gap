

#----------------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#----------------------------------------------------------------------------------------#


# Set up Plot theme
library(showtext)

g_font <- "Noto Sans" 
mono_font <- "Roboto Mono"

font_add_google(name = g_font,family = g_font)
font_add_google(name = mono_font,family = mono_font)

base_theme <- randplot::theme_rand(font = g_font) + theme(axis.text = 
                                                          element_text(family = mono_font),
                                                          legend.position="bottom",
                                                          legend.text=element_text(size=9),
                                                          panel.spacing.y=unit(0.5, "lines"),
                                                          panel.grid.minor=element_blank(),
                                                          panel.grid.major=element_blank())
showtext_auto()


# This script assumes you ran the 03_wealth_optimization_script


ams_fig1 = figs_data[["3"]]%>%
  ggplot(data = ., mapping = aes(x = year, y = disparity)) + 
  geom_line() + 
  geom_point() + 
  ylim(c(0,1)) + 
  ylab("Black-White Wealth Disparity") + 
  xlab("Year") + 
  base_theme

ams_fig1

ggplot2::ggsave(filename = "./outputs/ams_fig1.svg",plot =  ams_fig1, width = 6, height = 4, units = "in", scale = 1)



# figure 2
ams_fig2 = full_results %>%
  dplyr::filter(white_black_disparity <= 1) %>%
  dplyr::filter(Scenario != "$ 2 trillion allocation to the least-wealthy black households") %>%
  dplyr::filter(Scenario != "$ 2 trillion allocation to the least-wealthy households") %>%
  dplyr::filter(d_measure == "D") %>%
  mutate(Scenario = case_when(
    Scenario == "Equal allocations to all black Households" ~ "Equal allocations to all Black Households",
    Scenario == "Optimal Transfer, Black Households" ~ "Targeted allocations to Black Households",
    Scenario == "Optimal Transfer, Race-neutral" ~ "Targeted allocations to all Households",
    TRUE ~ Scenario
    )
    ) %>%
  ggplot(data = ., mapping = aes(x = wealth_distributed, y = white_black_disparity, color = Scenario)) +
  geom_line(size = 1.5) + 
  scale_color_manual(values = four_collor_pallete) + 
  xlim(c(0,2e12)) +
  guides(color=guide_legend(nrow=2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(c(0,1.1)))

ams_fig2 = format_ggplot_x_continuous_scale(ams_fig2, number_format = "dollar", limits = c(0,8e12)) + 
  base_theme + 
  xlab("Total Allocation") + 
  ylab("Disparity (D)")

ams_fig2

ggplot2::ggsave(filename = "./outputs/ams_fig2.svg",plot =  ams_fig2, width = 7, height = 4, units = "in", scale = 1)

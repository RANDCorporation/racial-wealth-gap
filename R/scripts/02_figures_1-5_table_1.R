

#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Global Variables --------------------------------------------------------
# use these constants to set the behavior of this script.
# if TRUE, do not re-do expensive calculations.
USE_BAKE = F       

# MOEA Settings:
# number of generations to use in the evolutionary algorithm. larger values imply larger chance of convergence.
# Increasing the number of generations and population size did not change results meaningfully, suggesting that
# the solutions were a good approximaation of the pareto surface.

GENERATIONS = 50 
POPSIZE = 100
YEAR = 2019

# Dependencies ------------------------------------------------------------
library(purrr)
library(patchwork)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randplot) 
library(mco)
library(survey)
library(mitools)
library(readxl)

# Source all functions:
invisible(sapply(X = paste0(list.files(path =  "./R/funs/", pattern = "*.R",full.names = T)),FUN = source, echo = F)) 

# Figures based on Time-Series Summaries ----------------------------------

# all ggplots are saved within this list for easier access:
figs = list()
# and data underlying figures and tables are stored within this list:
figs_data = list()

# Timeseries Data --------------------------------------------------------

# Reading time-series:
wealth_timeseries = read.csv("./inputs/scf-timeseries/scf/interactive_bulletin_charts_racecl4_median.csv") %>%
  # Keeping it in line with the microdata race names and color orders.
  mutate(Category = recode(Category, 'Black, non-Hispanic' = "Black", "White, non-Hispanic" = "White")) %>%
  mutate(Category = factor(Category, levels = c("White", "Black", "Hispanic", "Other"))) %>%
  dplyr::filter(Category %in% c("White", "Black"))

# Proportion of households that hold assets:
prop_hold_timeseries = read.csv("./inputs/scf-timeseries/scf/interactive_bulletin_charts_racecl4_have.csv") %>%
  # Keeping it in line with the microdata race names and color orders.
  mutate(Category = recode(Category, 'Black, non-Hispanic' = "Black", "White, non-Hispanic" = "White")) %>%
  mutate(Category = factor(Category, levels = c("White", "Black", "Hispanic", "Other"))) %>%
  dplyr::filter(Category %in% c("White", "Black"))

# Mean Timeseries:
mean_timeseries = read.csv("./inputs/scf-timeseries/scf/interactive_bulletin_charts_racecl4_mean.csv") %>%
  # Keeping it in line with the microdata race names and color orders.
  mutate(Category = recode(Category, 'Black, non-Hispanic' = "Black", "White, non-Hispanic" = "White")) %>%
  mutate(Category = factor(Category, levels = c("White", "Black", "Hispanic", "Other"))) %>%
  dplyr::filter(Category %in% c("White", "Black"))

asset_variables = names(wealth_timeseries %>% select(Assets:Unrealized_Capital_Gains) %>% select(-Assets, -Financial_Assets, -Nonfinancial_Assets))

debt_variables = names(wealth_timeseries %>% select(Home_Secured_Debt:Other_Installment_Loans) %>% select(-Installment_Loans))

# Calculating Differences  ------------------------------------------------

# Difference between black and white:
white_ts = wealth_timeseries %>%
  dplyr::filter(Category == "White") %>%
  arrange(year) %>%
  select(-year,-Category)

black_ts = wealth_timeseries %>%
  dplyr::filter(Category == "Black") %>%
  arrange(year) %>%
  select(-year,-Category)

# Black_white_relative gaps:
black_white_ratios = (white_ts / black_ts) %>%
  mutate(year = unique(wealth_timeseries$year)) %>%
  tidyr::pivot_longer(data = ., cols = -year, names_to = "category", values_to = "disparity")

# Black white differences
black_white_diff = (white_ts - black_ts) %>%
  mutate(year = unique(wealth_timeseries$year)) %>%
  tidyr::pivot_longer(data = ., cols = -year, names_to = "category", values_to = "disparity")

# Black white relative differences
black_white_relative_diff = ((white_ts - black_ts)/white_ts) %>%
  mutate(year = unique(wealth_timeseries$year)) %>%
  tidyr::pivot_longer(data = ., cols = -year, names_to = "category", values_to = "disparity")


# Proportions of the population who have these asset / liabilities classes
prop_wealth = prop_hold_timeseries %>%
  filter(year == YEAR) %>%
  select(-year) %>%
  tidyr::pivot_longer(data = ., cols = -Category, names_to = "wealth_category", values_to = "value") %>%
  tidyr::pivot_wider(id_cols = wealth_category, names_from = Category,values_from = value)
 



# Create household dataset for Figure 2 and Table 1 ----------------------------
set.seed(1234)

# Create the dataset with a large synthetic population (10 million) to calculate descriptive statistics and create the density plot:
w_data = bake("./outputs/w_data.rds", use_bake = USE_BAKE, expr =  {
  w_data = wopt_create_model_data_scf(n = 1e7, write_csv = F, scf_year = YEAR, scf_data_path = './inputs/scf-microdata/')
})

# Here I confirm that our median estimates using weighted resampling match the estimates in the original survey of consumer finances estimates.
# https://www.federalreserve.gov/econres/scf/dataviz/scf/chart/#series:Net_Worth;demographic:racecl4;population:all;units:median;range:1989,2019

# These numbers should be < 0.01, meaning that we are within 1% of their estimates:
# Compute Medians
m_white = median(w_data$networth[w_data$racecl4 == "White"])
m_black = median(w_data$networth[w_data$racecl4 == "Black"])

avg_white = mean(w_data$networth[w_data$racecl4 == "White"])
avg_black = mean(w_data$networth[w_data$racecl4 == "Black"])

# proportions
table(w_data$racecl4) / nrow(w_data)



# Difference between our estimaes is less than 1%
assertthat::assert_that((m_white - 189100) / 189100 < 0.01)
assertthat::assert_that((m_black - 24100) / 24100 < 0.01)

# Calculate disparity measure from the micro-data and compare to the time-series:
m_disparity = abs(m_white - m_black) / m_white

# this is calculated from the timeseries

ts_disparity = black_white_relative_diff %>%
  dplyr::filter(category == "Net_Worth", year == YEAR) %>%
  .$disparity

# disparity calculated with the time-series:
ts_disparity

# disparity calculated from our synthetic population:
m_disparity

# This number should be very close to zero:
assertthat::assert_that((ts_disparity - m_disparity) / ts_disparity < 0.01)


# Figure 1 ----------------------------------------------------------------

# A
figs_data[["1A"]] = wealth_timeseries %>%
  dplyr::select(year, Category, Net_Worth)

figs[["1A"]] = figs_data[["1A"]] %>%
  ggplot(data = ., mapping = aes(x = year, color = Category, y = Net_Worth)) + 
  geom_line() + 
  ylab("Median Household Net Worth") + 
  xlab("Year") +
  randplot::theme_rand(font = plot_font)+ 
  scale_color_brewer(palette = "Dark2", name = "Race") + 
  geom_point() + 
  geom_text(aes(label = round(Net_Worth, 0)),nudge_x = 0, nudge_y = 15, check_overlap = T,
            #vjust = "inward", hjust = "inward",
            show.legend = FALSE)

figs[["1A"]] = format_ggplot_y_continuous_scale(figs[["1A"]], number_format = "dollar", limits = c())


# B
figs_data[["1B"]] = black_white_diff %>%
  dplyr::filter(category == "Net_Worth") %>%
  dplyr::mutate(category = gsub(pattern = "_", replacement = " ", x = category))

figs[["1B"]] = figs_data[["1B"]] %>%
  ggplot(data = ., mapping = aes(x = year, y = disparity)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(disparity, 0)),nudge_x = -1, nudge_y = 9, check_overlap = T,
            vjust = "inward", hjust = "inward",
            show.legend = FALSE) + 
  ylab("White - Black Wealth Disparity") + 
  xlab("Year") + 
  randplot::theme_rand()

figs[["1B"]] = format_ggplot_y_continuous_scale(figs[["1B"]], number_format = "dollar", limits = c())


# C - Income Timeseries
figs_data[["1C"]] = wealth_timeseries %>%
  dplyr::select(year, Category, Before_Tax_Income)

figs[["1C"]] = figs_data[["1C"]] %>%
  ggplot(data = ., mapping = aes(x = year, color = Category, y = Before_Tax_Income)) + 
  geom_line() + 
  ylab("Median Before Tax Household Income") + 
  xlab("Year") +
  randplot::theme_rand(font = plot_font)+ 
  scale_color_brewer(palette = "Dark2", name = "Race") + 
  geom_point() + 
  geom_text(aes(label = round(Before_Tax_Income, 0)),nudge_x = 0, nudge_y = 3, check_overlap = T,
            #vjust = "inward", hjust = "inward",
            show.legend = FALSE) 

# Wealth gap timeseries:
figs[["1C"]] = format_ggplot_y_continuous_scale(figs[["1C"]], number_format = "dollar", limits = c())

# D - Income Disparity
figs_data[["1D"]] = black_white_diff %>%
  dplyr::filter(category == "Before_Tax_Income") %>%
  dplyr::mutate(category = gsub(pattern = "_", replacement = " ", x = category))

figs[["1D"]] = figs_data[["1D"]] %>%
  ggplot(data = ., mapping = aes(x = year, y = disparity)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(disparity, 0)),nudge_x = 0, nudge_y = 2, check_overlap = T,
            vjust = "inward", hjust = "inward",
            show.legend = FALSE) + 
  ylab("White - Black Income Disparity") + 
  xlab("Year") + 
  randplot::theme_rand()

figs[["1D"]] = format_ggplot_y_continuous_scale(figs[["1D"]], number_format = "dollar", limits = c())


# patch plot:
figs[["1"]] = ((figs[["1A"]] / figs[["1B"]]) | (figs[["1C"]]/figs[["1D"]])) + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect")  & theme(legend.position = 'bottom')

ggplot2::ggsave(filename = "./outputs/fig1.png",plot =  figs[["1"]],device = "png",width = 7, height = 4, units = "in", scale = 1.2)


# Figure 2 ----------------------------------------------------------------

# Overall percentiles:
overall_percentiles = w_data %>%
  summarize_at(vars(networth),.funs = funs(!!!all_p_funs)) # computes percentiles by race

wealth_white_black = w_data %>%
  dplyr::filter(racecl4 %in% c("White", "Black"))

percs_plot = c("100%", "95%","75%", "60%")

figs[["2A"]] = networth_race_plot(wealth_white_black, -100e3, overall_percentiles[[percs_plot[1]]]) + ggtitle('All Population')
figs[["2B"]] = networth_race_plot(wealth_white_black, -100e3, overall_percentiles[[percs_plot[2]]]) + ggtitle(paste0('Bottom ', percs_plot[2]))
figs[["2C"]] = networth_race_plot(wealth_white_black, -100e3, overall_percentiles[[percs_plot[3]]]) + ggtitle(paste0('Bottom ', percs_plot[3]))
figs[["2D"]] = networth_race_plot(wealth_white_black, -100e3, overall_percentiles[[percs_plot[4]]]) + ggtitle(paste0('Bottom ', percs_plot[4]))


figs[["2"]] = (figs[["2A"]] | figs[["2B"]] | figs[["2C"]] | figs[["2D"]]) + plot_layout(guides = "collect")  & theme(legend.position = 'bottom') # + plot_annotation(tag_levels = 'A')

figs[["2"]] 

ggplot2::ggsave(filename = "./outputs/fig2.png",plot =  figs[["2"]],device = "png",width = 7, height = 4, units = "in", scale = 1.2)

# Table 1 -----------------------------------------------------------------

# Compute relative wealth differences at the Current Distribution:

disparities_table = w_data %>%
  calculate_white_black_wealth_disparities(.)

# At this point, we no longer need the large w_data object, so we remove it to save memory:
rm(w_data)

table_1 = disparities_table %>%
  format_disparities_for_humans()

# Table 1:
writexl::write_xlsx(table_1, path = "./outputs/table_1.xlsx")
writexl::write_xlsx(disparities_table, path = "./outputs/table_1_raw.xlsx")

# Figure 3 ----------------------------------------------------------------

# Median disparity timeseries plot:
figs_data[["3"]] = black_white_relative_diff %>%
  dplyr::filter(category == "Net_Worth") %>%
  dplyr::mutate(category = gsub(pattern = "_", replacement = " ", x = category))

figs[["3"]] = figs_data[["3"]]%>%
  ggplot(data = ., mapping = aes(x = year, y = disparity)) + 
  geom_line() + 
  geom_point() + 
  geom_text(aes(label = round(disparity, 2)),nudge_x = 0, nudge_y = -0.05, check_overlap = T,
            vjust = "inward", hjust = "inward",
            show.legend = FALSE) + 
  geom_hline(yintercept = 0, colour = color_a) + 
  geom_hline(yintercept = 1, colour = color_b) +
  ylab("Black-White Wealth Disparity") + 
  xlab("Year") + 
  randplot::theme_rand()

ggplot2::ggsave(filename = "./outputs/fig3.png",plot =  figs[["3"]],device = "png",width = 7, height = 4, units = "in", scale = 1.2)


# Figure 4 ----------------------------------------------------------------

# A - Median Asset Value Gap
figs_data[["4A"]] = black_white_ratios %>%
  filter(category %in%  asset_variables) %>%
  mutate(category = gsub(pattern = "_", replacement = " ", x = category)) %>%
  mutate(disparity = round(disparity, 1)) %>%
  filter(year %in% c(YEAR)) %>%
  filter(!is.na(disparity))

figs[["4A"]] = figs_data[["4A"]] %>%
  ggplot(., aes(x = reorder(category, -disparity) , y = disparity)) +
  geom_segment(aes(x = reorder(category, -disparity), xend = reorder(category, -disparity), y = 0, yend = disparity),
               color = "gray", lwd = 1) +
  geom_point(size = 7) +
  geom_text(aes(label = disparity), color = "white", size = 2.5) +
  ylab("White / Black Median Asset Value Ratio") + 
  xlab("Asset Category") +
  coord_flip() + 
  randplot::theme_rand()


# B - Asset Value Ratio
figs_data[["4B"]] = black_white_diff %>%
  filter(category %in%  asset_variables) %>%
  mutate(category = gsub(pattern = "_", replacement = " ", x = category)) %>%
  mutate(disparity = round(disparity, 1)) %>%
  filter(year %in% c(YEAR)) %>%
  filter(!is.na(disparity)) %>%
  filter(category != "Directly Held Bonds")

figs[["4B"]] = figs_data[["4B"]] %>%
  ggplot(., aes(x = reorder(category, -disparity) , y = disparity)) +
  geom_segment(aes(x = reorder(category, -disparity), xend = reorder(category, -disparity), y = 0, yend = disparity),
               color = "gray", lwd = 1) +
  geom_point(size = 7) +
  geom_text(aes(label = disparity), color = "white", size = 2.5) +
  ylab("Median Asset Value Gap") + 
  xlab("Asset Category") +
  coord_flip() +
  randplot::theme_rand()

# patchwork plot:
figs[["4"]] = (figs[["4A"]] | figs[["4B"]]) + plot_annotation(tag_levels = 'A')

figs[["4"]] 

ggplot2::ggsave(filename = "./outputs/fig4.png",plot =  figs[["4"]],device = "png",width = 7, height = 4, units = "in", scale = 1.2)


# Figure 5 ----------------------------------------------------------------

# Plot for Liabilities:

# A - Differences
figs_data[["5A"]] = black_white_diff %>%
  filter(category %in%  debt_variables) %>%
  mutate(category = gsub(pattern = "_", replacement = " ", x = category)) %>%
  mutate(disparity = round(disparity, 1)) %>%
  filter(year %in% c(YEAR)) %>%
  filter(!is.na(disparity))


figs[["5A"]] = figs_data[["5A"]] %>%
  ggplot(., aes(x = reorder(category, -disparity) , y = disparity)) +
  geom_segment(aes(x = reorder(category, -disparity), xend = reorder(category, -disparity), y = 0, yend = disparity),
               color = "gray", lwd = 1) +
  geom_point(size = 7) +
  geom_text(aes(label = disparity), color = "white", size = 2.5) +
  ylab("Median Debt Value Gap") + 
  xlab("Liability Category") +
  coord_flip() +
  randplot::theme_rand()

# B - Ratios
figs_data[["5B"]] = black_white_ratios %>%
  filter(category %in%  debt_variables) %>%
  mutate(category = gsub(pattern = "_", replacement = " ", x = category)) %>%
  mutate(disparity = round(disparity, 1)) %>%
  filter(year %in% c(YEAR)) %>%
  filter(!is.na(disparity))

figs[["5B"]] = figs_data[["5B"]] %>%
  ggplot(., aes(x = reorder(category, -disparity) , y = disparity)) +
  geom_segment(aes(x = reorder(category, -disparity), xend = reorder(category, -disparity), y = 0, yend = disparity),
               color = "gray", lwd = 1) +
  geom_point(size = 7) +
  geom_text(aes(label = disparity), color = "white", size = 2.5) +
  ylab("White / Black Median Debt Value") + 
  xlab("Liability Category") +
  coord_flip() +
  randplot::theme_rand()

# patchwork plot
figs[["5"]] = (figs[["5A"]] | figs[["5B"]]) + plot_annotation(tag_levels = 'A')

ggplot2::ggsave(filename = "./outputs/fig5.png",plot =  figs[["5"]],device = "png",width = 7, height = 4, units = "in", scale = 1.2)


# Figure 9 ----------------------------------------------------------------

transfer_ammount = (m_white - m_black)

figs_data[["9"]] = disparities_table %>% 
  mutate(med_transfer = difference - transfer_ammount) %>%
  mutate(name = factor(x = name,levels = name, ordered = T))


figs[["9"]] = figs_data[["9"]] %>%
  filter(name <= "90%" & name >= "10%") %>%
  pivot_longer(cols = c(difference, med_transfer), names_to = "Scenario", values_to = "difference") %>%
  mutate(Scenario = recode_factor(Scenario, difference = "Baseline", med_transfer = "Median-Equalizing Transfer", .ordered = T)) %>%
  filter(Scenario != "Average-Equalizing Transfer") %>%
  ggplot() + 
  geom_col(mapping = aes(y = difference, x = name, fill = Scenario), position =  position_dodge()) + 
  geom_hline(yintercept = transfer_ammount, linetype = "dashed") + 
  geom_hline(yintercept = 0) + 
  randplot::theme_rand(font = plot_font)+ 
  scale_fill_brewer(palette = "Dark2", name = "Scenario") + 
  xlab("15th to 90th percentile of wealth distribution") + 
  ylab("White-Black Wealth difference") # + 
#annotate(geom = "text", y = 200*1e3, x = "10%")

figs[["9"]] = format_ggplot_y_continuous_scale(figs[["9"]], number_format = "dollar", limits = c())

figs[["9"]]

ggplot2::ggsave(filename = "./outputs/fig9.png",plot =  figs[["9"]],device = "png",width = 7, height = 4, units = "in", scale = 1.2)

# Table B1 -----------------------------------------------------------------

table_2 = wealth_timeseries %>%
  filter(year == YEAR) %>%
  select(-year) %>%
  tidyr::pivot_longer(data = ., cols = -Category, names_to = "wealth_category", values_to = "prop") %>%
  tidyr::pivot_wider(id_cols = wealth_category, names_from = Category,values_from = prop) %>%
  na.omit() %>%
  filter(wealth_category %in% c(asset_variables, debt_variables)) %>%
  mutate(Diff = White-Black) %>%
  mutate(Rel.Diff = White/Black) %>%
  left_join(prop_wealth, suffix = c("", ".Prop"), by = "wealth_category") %>%
  mutate(across(where(is.numeric), .fns = ~signif(.x, digits = 3))) %>%
  mutate(Class = ifelse(wealth_category %in% asset_variables, "Asset", "Liability")) %>%
  arrange(Class, wealth_category) %>%
  mutate(wealth_category = gsub(pattern = "_", replacement = " ", x = wealth_category))

writexl::write_xlsx(table_2, path = "./outputs/table_2.xlsx")

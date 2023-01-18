

#----------------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#----------------------------------------------------------------------------------------#

# Global Variables --------------------------------------------------------
# use these constants to set the behavior of this script.
USE_BAKE = F        

# Many-Objective Optimization Settings:
# number of generations to use in the evolutionary algorithm. larger values imply larger chance of convergence.
# We verified that duplicating the number of generations and popsize did not improve results, thus these values result in appropriate approximate non-dominated solutions.
GENERATIONS = 50 # 50
POPSIZE = 100 # 100

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

# Running the model -----------------------------------------

# This is the number of households in the synthetic population dataset.
# This number needs to larrge enough to generate a small difference between the quantile estimates and the official SCF estimates, and yet small enough to allow the optimization algorithm to run.

set.seed(12345)

# Setting the number of households in the model:
# According to the table on pg. 40, 128.6 million households are represented by the sample in the 2019 SCF: https://www.federalreserve.gov/publications/files/scf20.pdf
n_model = 128.6e6 / 10000
# According to the table on pg. 40, 128.6 million households are represented by the sample in the 2019 SCF: https://www.federalreserve.gov/publications/files/scf20.pdf 
us_households = 128.6e6
scaling_factor = us_households / n_model

w_data_model = bake("./outputs/w_data_model.rds",use_bake = USE_BAKE, expr =  {
  w_data_model = wopt_create_model_data_scf(n = n_model, write_csv = F, scf_year = 2019, scf_data_path = './inputs/scf-microdata/')
})

# To run the model, we need a smaller population, otherwise the model takes too long to run:
m_white = median(w_data_model$networth[w_data_model$racecl4 == "White"])
m_black = median(w_data_model$networth[w_data_model$racecl4 == "Black"])

disparity_model = abs(m_white - m_black) / m_white


# Creating a model object to test the objective function:

# Testing the model function:
w_model = wopt(wopt_data = w_data_model, scaling_factor = scaling_factor, percentiles = "D") 

w_model$fn(x = c(wealth_to_redistribute = 0, eligibility_percentile = 15),disparity_measure = "D", race_blind = T, model = w_model)

w_model$fn(x = c(wealth_to_redistribute = 1, eligibility_percentile = 15),disparity_measure = "median", race_blind = T, model = w_model)

w_model$fn(x = c(wealth_to_redistribute = 9, eligibility_percentile = 100),disparity_measure = "mean", race_blind = F, model = w_model)

# Race-Targeted policy:
w_model$fn(x = c(wealth_to_redistribute = 7, eligibility_percentile = 100), race_blind = F, model = w_model)

# Optimize and Run for the average disparity - this step is computationally intensive:
system.time({
  results_mean = run_all_scenarios(wopt_data = w_data_model, 
                                     disparity_measure = "mean", 
                                     percentiles = "median", # this is not used when disparity measure is mean 
                                     scaling_factor = scaling_factor, 
                                     fixed_wealth = 100 * (2/130), # 2 Trillion
                                     use_bake = USE_BAKE
  )
})


# Optimize and Run for the median disparity - this step is computationally intensive:
system.time({
  
  results_median = run_all_scenarios(wopt_data = w_data_model, 
                                     disparity_measure = "median", 
                                     percentiles = "median", 
                                     scaling_factor = scaling_factor, 
                                     fixed_wealth = 100 * (2/130), # 2 Trillion
                                     use_bake = USE_BAKE
  )
  
})


# Running the model using the "D" disparity measure.

system.time({
  results_d = run_all_scenarios(wopt_data = w_data_model, 
                                disparity_measure = "D", # Use the D measure
                                percentiles = "D", 
                                scaling_factor = scaling_factor, 
                                fixed_wealth = 100 * (2/130), # 2 Trillion
                                use_bake = USE_BAKE
  )
  
})

full_results = rbind(results_mean, results_median, results_d)

writexl::write_xlsx(x = full_results, path = "./outputs/wealth_opt_results.xlsx")

# Looking at Total transfer vs disparities for the median disparity measure.
fig6 = full_results %>%
  dplyr::filter(Scenario != "$ 2 trillion allocation to the least-wealthy black households") %>%
  dplyr::filter(Scenario != "$ 2 trillion allocation to the least-wealthy households") %>%
  dplyr::filter(d_measure == "mean") %>%
  ggplot(data = ., mapping = aes(x = wealth_distributed, y = white_black_disparity, color = Scenario)) +
  geom_line(size = 1.5) + 
  xlab("Total Allocation") + 
  ylab("White-Black Mean Disparity") + 
  randplot::theme_rand(font = plot_font)+ 
  scale_color_manual(values = four_collor_pallete) + 
  xlim(c(0,2e12)) +
  guides(color=guide_legend(nrow=2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(c(0,1.1)))

fig6 = format_ggplot_x_continuous_scale(fig6, number_format = "dollar", limits = c())

fig6

ggplot2::ggsave(filename = "./outputs/fig6.png",plot =  fig6,device = "png",width = 7, height = 4, units = "in", scale = 1.2)



# Looking at Total transfer vs disparities for the median disparity measure.
fig7 = full_results %>%
  dplyr::filter(Scenario != "$ 2 trillion allocation to the least-wealthy black households") %>%
  dplyr::filter(Scenario != "$ 2 trillion allocation to the least-wealthy households") %>%
  dplyr::filter(d_measure == "median") %>%
  ggplot(data = ., mapping = aes(x = wealth_distributed, y = white_black_disparity, color = Scenario)) +
  geom_line(size = 1.5) + 
  xlab("Total Allocation") + 
  ylab("White-Black Median Disparity") + 
  randplot::theme_rand(font = plot_font)+ 
  scale_color_manual(values = four_collor_pallete) + 
  xlim(c(0,2e12)) +
  guides(color=guide_legend(nrow=2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(c(0,1.1)))

fig7 = format_ggplot_x_continuous_scale(fig7, number_format = "dollar", limits = c(0,8e12))

ggplot2::ggsave(filename = "./outputs/fig7.png",plot =  fig7,device = "png",width = 7, height = 4, units = "in", scale = 1.2)


# Looking at Total transfer vs disparities for the d measure:
fig8 = full_results %>%
  dplyr::filter(white_black_disparity <= 1) %>%
  dplyr::filter(Scenario != "$ 2 trillion allocation to the least-wealthy black households") %>%
  dplyr::filter(Scenario != "$ 2 trillion allocation to the least-wealthy households") %>%
  dplyr::filter(d_measure == "D") %>%
  ggplot(data = ., mapping = aes(x = wealth_distributed, y = white_black_disparity, color = Scenario)) +
  geom_line(size = 1.5) + 
  xlab("Total Allocation") + 
  ylab("Disparity (D)") + 
  randplot::theme_rand(font = plot_font)+ 
  scale_color_manual(values = four_collor_pallete) + 
  xlim(c(0,2e12)) +
  guides(color=guide_legend(nrow=2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(c(0,1.1)))

fig8 = format_ggplot_x_continuous_scale(fig8, number_format = "dollar", limits = c(0,8e12))

ggplot2::ggsave(filename = "./outputs/fig8.png",plot =  fig8,device = "png",width = 7, height = 4, units = "in", scale = 1.2)




#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Functions to Calculate Percentiles --------------------------------------

p_all <- seq.default(from = 0, to = 1, length.out = 21)

p_names_all <- purrr::map_chr(p_all, ~paste0(.x*100, "%"))

all_p_funs <- purrr::map(p_all, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  purrr::set_names(nm = p_names_all)

# Calculate Black-Wealth Disparities at each Percentile of Distribution --------

# Compute Percentiles Table

calculate_white_black_wealth_disparities = function(wealth_data){
  wealth_data %>%
    group_by(racecl4) %>%
    summarize_at(vars(networth),.funs = funs(!!!all_p_funs)) %>% # computes percentiles by race
    filter(racecl4 %in% c("White", "Black")) %>% # select white and black households
    tidyr::pivot_longer(cols = -racecl4) %>% # reshape the dataframe
    tidyr::pivot_wider(names_from = racecl4, values_from = value) %>%
    mutate(difference = White-Black) %>%
    mutate(abs_difference = abs(difference)) %>% # compute the absolute difference between percentiles
    mutate(relative_difference = abs_difference / White)
}

# Format Disparities Table for humans

format_disparities_for_humans = function(disparities_table){
  disparities_table %>%
    rename(Percentile = name, 
           'Difference' = difference,
           'Absolute Difference' = abs_difference,
           'Relative Difference' = relative_difference) %>%
    mutate(across(.cols = White:`Absolute Difference`, .fns = format_currency_for_humans)) %>%
    mutate(across(.cols = `Relative Difference`, .fns = format_percentage_for_humans))
}

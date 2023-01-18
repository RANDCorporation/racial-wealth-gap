

#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# set objective function --------------------------------------------------

#' Optimiation Objective Functions
#'
#' This function takes a vector of inputs x and returns a set of outcomes, all of which should be minimized.
#' This function needs to be compatible with the mco::nsga2 optimization function. Hence, the input vector is named "x"
#'
#' @param x vector of decision variables. 
#' @param race_blind if TRUE, use a race-blind transfer policy. If F, target only black households.
#' @param relative_wealth If T, uses a relative wealth differences, if F uses absolute differences.
#' @param disparity_measure one of "D", "mean", or "median" see report for the definitions.
#' @param optimization if T, runs the "optimization mode", in which only the necessary output is returned by the function. If "F", returns a list with more details.
#' @param model an object of the wopt class.
#' @param single_objective if T, uses only the disparity measure as the objective function. defaults to F.
#'
#' @return if optimization = T, returns a vector of objectives to be minimized. if optimization = F, returns more objects.
wopt_obj_function = function(x, race_blind, relative_wealth = T, optimization = T, single_objective = F, disparity_measure = "median", model) {
  
  # X Inputs
  
  # This is the percentage of total wealth to transfer.
  fraction_of_wealth_to_redistribute = x[1]   
  
  # Maximum Wealth one can have to be eligible for the transfer.
  wealth_eligibility_criteria = x[2]
  
  # sum of wealth in the dataset:
  total_wealth = sum(model$data$networth)
  
  # Sum of actual household wealth in the US:
  # Financial Accounts of the United States, FED, release of March 11th, 2021
  # https://www.reuters.com/article/us-usa-fed-wealth/u-s-households-ended-2020-with-record-130-2-trillion-in-wealth-fed-says-idUSKBN2B32H5
  actual_total_Wealth = 130.2 * 1e12
  
  # 122.8 million households
  # From the 2019 1-year ACS estimates
  # https://www.census.gov/newsroom/press-kits/2020/acs-1year.html
  number_of_households = 122.8 * 1e6
  
  # Total Money to distribute in reality
  actual_money_to_redistribute = actual_total_Wealth * fraction_of_wealth_to_redistribute / 100
  
  # Number of households in the dataset:
  n_households_dataset <- nrow(model$data)
  
  # In this model, we scale the wealth to distribute based on the number
  # of households in this dataset:

  money_to_redistribute = actual_money_to_redistribute * (n_households_dataset / number_of_households)
  
  # Determine eligibility:

  updated_wealth = model$data %>%
    arrange(networth) %>%
    mutate(top_networth_p = 100 - ntile(networth,10000)/100, # Compute percentiles, subtract from 100, so that the numbers reflect the bottom x %.
           top_income_p = 100 - ntile(income,10000)/100) %>% # Do the same with income.
    mutate(is_black = racecl4 == "Black") %>%
    mutate(is_race_eligible = is_black | race_blind) %>%
    mutate(eligible = is_race_eligible & (networth <= wealth_eligibility_criteria))
  
  # Count the number of eligible people:
  
  eligible_households = sum(updated_wealth$eligible)
  
  value_per_household = money_to_redistribute / eligible_households
  
  # Transfer wealth to the eligible:
  updated_wealth = updated_wealth %>%
    mutate(new_networth = ifelse(eligible, networth + value_per_household,networth))
  
  
  if(disparity_measure %in% c("median", "D")) {
    
    # Compute relative wealth differences, only for the percentiles used in the optimization model.
    quantiles = updated_wealth %>%
      group_by(racecl4) %>%
      summarize_at(vars(new_networth),.funs = funs(!!!model$p_funs)) %>% # computes percentiles by race
      filter(racecl4 %in% c("White", "Black")) %>% # select white and black households
      tidyr::pivot_longer(cols = -racecl4) %>% # reshape the dataframe
      tidyr::pivot_wider(names_from = racecl4, values_from = value) %>%
      mutate(white_black_difference = White-Black) %>%
      mutate(abs_difference = abs(white_black_difference)) %>% # compute the absolute difference between percentiles
      mutate(relative_difference = abs_difference / White) # compute the relative difference.
    
    # Compute relative wealth differences, for all percentiles

    all_quantiles = updated_wealth %>%
      calculate_white_black_wealth_disparities(.)
    
    # Compute alternative disparity measures:
    
    average_difference = mean(quantiles$white_black_difference)
    
    average_abs_difference = mean(quantiles$abs_difference) # take the average of this difference,
    
    average_relative_difference = mean(quantiles$relative_difference)
    
  } else { 
    # Use averages:
    mean_white = mean(updated_wealth$new_networth[updated_wealth$racecl4 == "White"])
    mean_black = mean(updated_wealth$new_networth[updated_wealth$racecl4 == "Black"])
    
    average_abs_difference = abs(mean_white - mean_black)
    
    average_relative_difference = average_abs_difference / mean_white
    
  }
  
  # y needs to be a numeric vector for compatibility with nsgaii

  y <- numeric(2)
  y[1] <- ifelse(relative_wealth, average_relative_difference, average_abs_difference)
  y[2] <- actual_money_to_redistribute
  
  # Decide what objects to return based on the optimization and single_objective parameter.

  if(optimization) {
    if(single_objective){
      return(y[1])
    } else {
      return(y)  
    }
  } else {
    return(data.frame(
      race_blind = race_blind,
      wealth_eligibility_criteria = wealth_eligibility_criteria,
      actual_money_to_redistribute = actual_money_to_redistribute, 
      eligible_households = eligible_households * model$scaling_factor,
      value_per_household = value_per_household,
      average_difference = average_difference,
      average_abs_difference = average_abs_difference,
      average_relative_difference = average_relative_difference
    ))
  }
  
}


vectorized_obj_function = function(x, race_blind, relative_wealth = T, optimization = T, model) {
  #library(future.apply)
  #future::plan(multisession)
  apply(X = x, MARGIN = 1, FUN = model$fn , race_blind = race_blind, relative_wealth = relative_wealth, optimization = optimization, model = w_model)
  #future.apply::future_apply(X = x, MARGIN = 1, FUN = w_model$fn , race_blind = T, relative_wealth = T, optimization = T, model = w_model)
}

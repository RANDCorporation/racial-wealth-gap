

#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#


# Function to Run all Scenarios -------------------------------------------

run_all_scenarios = function(wopt_data, disparity_measure = "median", percentiles = "median", scaling_factor = scaling_factor, fixed_wealth = 100 * (2/130.2), use_bake) {
  
  
  # The w_model object is the model object responsible for running the wealth transfers simulations.
  w_model = wopt(wopt_data = wopt_data, scaling_factor = scaling_factor, percentiles = percentiles) 
  
  
  # Percentiles by race:
  percentiles_by_race = w_model$data %>%
    group_by(racecl4) %>%
    summarize_at(vars(networth),.funs = funs(!!!all_p_funs)) %>% # computes percentiles by race
    tidyr::pivot_longer(cols = -racecl4)
  
  medians_by_race = percentiles_by_race %>%
    filter(name == "50%")
  
  # maximum wealth:
  max_wealth = max(w_model$data$networth) + 1
  
  # Use the black wealth percentiles as a reference point:
  black_wealth_percentiles = percentiles_by_race %>% 
    dplyr::filter(racecl4 == "Black") %>%
    .$value
  
  
  # We can also use the white wealth percentiles as reference points:
  white_wealth_percentiles = percentiles_by_race %>% 
    dplyr::filter(racecl4 == "White") %>%
    .$value
  
  
  # Running Scenarios without optimization ----------------------------------
  
  # There are three decision variables involved in this problem:
  
  # The three decision variables are passed to the w_model function with this call:
  
  # w_model$fn(c(wealth_to_redistribute = A, wealth_eligibility_criteria = w_tilde), race_blind = F, model = w_model)
  
  # 1. Wealth to Allocate (A)
  # We define wealth to allocate as a proportion of the total wealth held by society.
  # The range of this variable is between 0 and 100 (it is defined as percentage points).
  # The wealth_proportion_vector contains the values that will be used in the runs without optimization.
  
  # 2. Wealth Eligibility criteria
  # Wealth is allocated to households that hold less than w_tilde in networth.
  # The parameter is called wealth_eligibility_criteria, and refers to dollars of networth.
  
  # 3. Racial Neutral vs Race-targeted.
  # if race_blind = T, then wealth is allocated to all households. If not, it is allocated to black households.
  # Targeted allocations to other races is not considered in this analysis.
  
  # ranges of total wealth proportion to consider
  wealth_proportion_vector = c(seq.default(from = 0,to = 20, by = 0.05))
  
  # Scenario 1: Equal allocation to all black households.
  # This run considers equal allocations to all black households considering various allocation sizes.
  
  fixed_eligibility_black_list = purrr::map(.x = wealth_proportion_vector, .f = function(x) w_model$fn(c(wealth_to_redistribute = x, wealth_eligibility_criteria = max_wealth), race_blind = F, disparity_measure = disparity_measure, model = w_model))
  
  fixed_eligibility_black_df = as.data.frame(do.call(rbind, fixed_eligibility_black_list))
  
  names(fixed_eligibility_black_df) =  c("white_black_disparity","wealth_distributed")
  
  fixed_eligibility_black_df = fixed_eligibility_black_df %>%
    mutate(wealth_eligibility =  max_wealth, 
           Scenario = "Equal allocation to all black households")
  
  
  # Scenario 2: Equal allocation to all black households.
  # This run considers equal allocations to all households considering various allocation sizes.

  fixed_eligibility_list = purrr::map(.x = wealth_proportion_vector, .f = function(x) w_model$fn(c(wealth_to_redistribute = x, wealth_eligibility_criteria = max_wealth), race_blind = T, disparity_measure = disparity_measure, model = w_model))
  
  fixed_eligibility_df = as.data.frame(do.call(rbind, fixed_eligibility_list))
  
  names(fixed_eligibility_df) =  c("white_black_disparity","wealth_distributed")
  
  fixed_eligibility_df = fixed_eligibility_df %>%
    mutate(wealth_eligibility =  max_wealth, 
           Scenario = "Equal allocation to all households")
  
  
  # Scenario 3: Equal allocation to poor households.
  # This run considers equal allocations to households below various wealth percentiles, considering a single total allocation (fixed_wealth).
  # We use white wealth percentiles because this percentile vector encompasses all other races.

  fixed_transfer_race_neutral_list = purrr::map(.x = white_wealth_percentiles, .f = function(x) w_model$fn(c(wealth_to_redistribute = fixed_wealth, wealth_eligibility_criteria = x), race_blind = T, disparity_measure = disparity_measure, model = w_model))
  
  fixed_transfer_race_neutral_df = as.data.frame(do.call(rbind, fixed_transfer_race_neutral_list))
  
  names(fixed_transfer_race_neutral_df) = c("white_black_disparity","wealth_distributed")
  
  fixed_transfer_race_neutral_df = fixed_transfer_race_neutral_df %>%
    mutate(wealth_eligibility = white_wealth_percentiles,
           Scenario = "$ 2 trillion allocation to the least-wealthy households")
  
  # Scenario 4: Equal allocation to all black households.
  # This run considers equal allocations to BLACK households below various wealth percentiles, considering a single total allocation (fixed_wealth).

  fixed_transfer_list = purrr::map(.x = black_wealth_percentiles, .f = function(x) w_model$fn(c(wealth_to_redistribute = fixed_wealth, wealth_eligibility_criteria = x), race_blind = F, disparity_measure = disparity_measure, model = w_model))
  
  fixed_transfer_df = as.data.frame(do.call(rbind, fixed_transfer_list))
  
  names(fixed_transfer_df) = c("white_black_disparity","wealth_distributed")
  
  fixed_transfer_df = fixed_transfer_df %>%
    mutate(wealth_eligibility = black_wealth_percentiles,
           Scenario = "$ 2 trillion allocation to the least-wealthy black households")
  
  
  # Running Scenarios with optimization -------------------------------------
  
  # Run Wealth distribution optimization exercises --------------------------
  
  # This optimization exercise chooses the wealth eligibility criteria and 
  # allocation value that minimizes a disparity measure and the total allocation.
  
  # Uses bake because it is expensive to run the optimization:
  
  w_modelnsga_race_blind = bake(paste0("outputs/",disparity_measure,"w_modelnsga_race_blind.rds"),use_bake = use_bake, expr =  {
    
    w_modelnsga_race_blind = nsga2(w_model$fn, 2, 2,
                                   generations=GENERATIONS, popsize=POPSIZE,
                                   cprob=0.7, cdist=20,
                                   mprob=0.2, mdist=20,
                                   lower.bounds=c(0, 0),
                                   upper.bounds=c(20, 10e6), 
                                   model = w_model, race_blind = T,
                                   disparity_measure = disparity_measure)
    
    
  })
  
  # Name Parameters

  colnames(w_modelnsga_race_blind$par) = c("fraction_of_wealth_to_redistribute","wealth_eligibility")
  
  # Name Results

  colnames(w_modelnsga_race_blind$value) = c("white_black_disparity","wealth_distributed")
  
  w_modelnsga_race_targeted = bake(paste0("outputs/",disparity_measure,"w_modelnsga_race_targeted.rds"),use_bake = use_bake, expr =  {
    
    w_modelnsga_race_targeted = nsga2(w_model$fn, 2, 2,
                                      generations=GENERATIONS, popsize=POPSIZE,
                                      cprob=0.7, cdist=20,
                                      mprob=0.2, mdist=20,
                                      lower.bounds=c(0, 0),
                                      upper.bounds=c(20, 10e6), 
                                      model = w_model, 
                                      race_blind = F,
                                      disparity_measure = disparity_measure)
    
  })
  
  # Naming results

  colnames(w_modelnsga_race_targeted$par) = c("fraction_of_wealth_to_redistribute","wealth_eligibility")
  
  colnames(w_modelnsga_race_targeted$value) = c("white_black_disparity","wealth_distributed")
  
  # Assign Results:
  results_race_blind = as.data.frame(cbind(w_modelnsga_race_blind$par, w_modelnsga_race_blind$value)) %>%
    mutate(Scenario = "Optimal Transfer, Race-neutral") %>%
    select(-fraction_of_wealth_to_redistribute)
  
  results_race_targeted = as.data.frame(cbind(w_modelnsga_race_targeted$par, w_modelnsga_race_targeted$value)) %>%
    mutate(Scenario = "Optimal Transfer, Black Households") %>%
    select(-fraction_of_wealth_to_redistribute)
  
  
  # Consolidate results

  wealth_opt_results = rbind(results_race_blind, results_race_targeted, fixed_transfer_df, fixed_transfer_race_neutral_df, fixed_eligibility_df, fixed_eligibility_black_df) %>%
    mutate(d_measure = disparity_measure)
  
  return(wealth_opt_results)
  
}

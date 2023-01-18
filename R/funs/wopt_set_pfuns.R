

#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

#' Defines the percentile functions for a wopt model.
#'
#' @param wopt The wopt model to be returned.
#' @param measure if "median", only computes the 50th percentile. else, computes all percentiles between min and max percentile
#' @param min_percentile initial percentile to compute
#' @param max_percentile final percentile to compute
#' @param by width of intervals between percentiles to compute
#'
#' @return a `wopt` model with a `p_funs` object that can be used to compute all sets of percentiles.
wopt_set_pfuns = function(wopt_model, measure, min_percentile = 0.15, max_percentile = 0.9, by = 0.05) {
  
  # Defines the Quantiles for Comparison:
  # Compute difference in percentiles between White and black:
  
  # p can be defined either as a set of percentiles we use or a single percentile.
  
  if(measure == "median"){
    p = 0.5
  } else {
    p <- seq.default(from = min_percentile, to = max_percentile, by = by)
  }
  
  p_names <- purrr::map_chr(p, ~paste0(.x*100, "%"))
  
  # Percentile functions allows us to compute several percentiles.
  wopt_model$p_funs <- purrr::map(p, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    purrr::set_names(nm = p_names)
  
  return(wopt_model)
  
}



#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# WOPT S3 Class
# The Wealth Optimization S3 class instantiates a model of wealth transfers.
# This model class represents policies that changes the wealth distribution of 
# US households.
# 
# We create a representative sample of US households using weighted resampling
# with data from the Survey of Consumer finances.
#
#------------------------------------------------------------------------------#

# wopt S3 class -----------------------------------------------------------
# Creates the wealth optimization model S3 class.

#' Wealth Optimization model class
#' 
#' This function creates a self-contained model including both the wealth distribution data as well the methods for the wealth disparities analysis.
#'
#' @param wopt_data data.frame created by the wopt_create_model_data_scf function.
#' @param scaling_factor number of actual US households represented by each line in the wopt_data data.frame.
#'
#' @return an object of the class wopt capable of performing the wealth optimization tasks
#' @export
#'
wopt = function(wopt_data, scaling_factor, percentiles = "median"){
  
  wopt_model = list()
  
  wopt_model$data = wopt_data
  
  wopt_model$fn = wopt_obj_function
  
  wopt_model$vec_fn = vectorized_obj_function
  
  wopt_model$scaling_factor = scaling_factor
  
  # min_percentile and max_percentile are hard coded here because we don't need to change them in this paper.
  wopt_model = wopt_set_pfuns(wopt_model, measure = percentiles, min_percentile = 0.15, max_percentile = 0.9, by = 0.05)
  
  class(wopt_model) = c("wopt")  
  
  return(wopt_model)
  
}



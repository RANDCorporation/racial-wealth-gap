

#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#


# create model scf data ---------------------------------------------------
#' Creates a dataset of US households
#'
#' @param n Desired size of the population
#' @param write_csv if TRUE, writes a csv file
#' @param scf_data_path relative location of the scf micro datasets
#' @param scf_year year to use
#' @param scf_manual_dataset location of a manual-data.xlsx file
#' @param seed random seed to use
#'
#' @return a synthetic dataset of US households
wopt_create_model_data_scf = function(n = 10000
                                      ,write_csv = T
                                      ,scf_data_path = './inputs/scf-microdata/'
                                      ,scf_year = 2019
                                      ,scf_manual_dataset = "./inputs/manual-data.xlsx"
                                      ,seed) {
  
  # Set seed because these steps will involve sampling
  if(!missing(seed)) {
    set.seed(seed)
  }
  
  # Read the multiply-imputed datasets list:

  scf_imp = readRDS(paste0(scf_data_path, "scf ", scf_year, ".rds"))
  
  # And read a dataset with the replication weights:

  scf_rw = readRDS(paste0(scf_data_path, "scf ", scf_year, " rw.rds"))
  
  
  # first, read manual-data and select variables of interest:

  variables = readxl::read_xlsx(path = scf_manual_dataset, sheet = "scf-variables")
  
  
  # Combining the full dataset and selecting variables of interest:

  full_dataset = do.call(rbind, scf_imp)
  
  
  # Crreate a new id for the dataset:

  full_dataset = full_dataset %>%
    dplyr::select(dplyr::any_of(variables$variable_name)) %>%
    mutate(row_id = row_number()) %>%
    mutate(norm_wgt = wgt / sum(wgt)) %>%
    mutate(year = scf_year)
  
  # Sample from the dataset with replacement and recode variables:

  new_dataset = data.frame(row_id = sample(x = full_dataset$row_id, size = n, replace = T, prob = full_dataset$norm_wgt)) %>%
    mutate(new_id = row_number()) %>%
    left_join(full_dataset, by = "row_id") %>%
    mutate(
      hhsex = factor( hhsex , labels = c( "male" , "female" ) ) ,
      racecl4 = factor(racecl4, labels = c("White", "Black", "Hispanic", "Other")),
      married = as.numeric( married == 1 ) ,
      edcl = 
        factor( 
          edcl , 
          labels = 
            c( 
              "less than high school" , 
              "high school or GED" , 
              "some college" , 
              "college degree" 
            ) 
        )
    )
  
  
  # Write dataset to the working directory:

  if(write_csv) {
    
    write.csv(new_dataset, file = "population.csv")
    
  }
  
  return(new_dataset)
  
}

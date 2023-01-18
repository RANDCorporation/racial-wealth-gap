

#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# Use this script to install dependencies for this project.

# Alternatively, first install the renv package with install.packages("renv"), 
# then run renv::restore() to install the same package versions we used.

# CRAN packages
packages = c("purrr", "patchwork", "dplyr", "tidyr", "ggplot2", "mco", "survey", "mitools", "readxl", "writexl", "remotes")

install.packages(packages)

# github packages
remotes::install_github("https://github.com/RANDCorporation/randplot")

remotes::install_github("https://github.com/ajdamico/lodown")


#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# install dependencies if needed
# source("./R/scripts/00_install_dependencies.R")

# Note: This script will take about 30-45 minutes to run for the first time.

# download data
print(Sys.time())

source("./R/scripts/01_download_data.R")

print(Sys.time())

source("./R/scripts/02_figures_1-5_table_1.R")

print(Sys.time())

source("./R/scripts/03_wealth_optimization_script.R")

print(Sys.time())

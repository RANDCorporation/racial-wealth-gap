

#----------------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#----------------------------------------------------------------------------------------#

# This script assumes that the Federal Reserve will not change names of their files, or the structure of each file.

# Time-series summaries from the SCF are downloaded from:

download.file(url = "https://www.federalreserve.gov/econres/scf/dataviz/download/zips/scf.zip",destfile = "./inputs/scf-timeseries/scf.zip")

unzip(zipfile = "./inputs/scf-timeseries/scf.zip",overwrite = T, exdir = "./inputs/scf-timeseries/scf")

# We also download microdata using the lodown package.
# http://asdfree.com/survey-of-consumer-finances-scf.html

library(lodown)

# Get data catalog:
scf_cat <-
  get_catalog( "scf" ,
               output_dir = "./inputs/scf-microdata")

# We are using 2019 data:
scf_cat <- subset( scf_cat , year == 2019 )

# download microdata to your local computer
scf_cat <- lodown( "scf" , scf_cat )

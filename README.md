# Overcoming Compound Racial Inequity Code Repository

This repository contains code used in the [Overcoming Compound Racial Inequity: Policies and Costs for Closing the Black-White Wealth Gap](https://www.rand.org/pubs/research_reports/RRA1259-2.html) RAND Report. 

All figures and tables from our analysis can be reproduced with publicly available data and free software. 

### Dependencies

We used R version 4.2.1 (2022-06-23) and RStudio 2022.07.1 to perform this analysis. Using an up-to-date R installation (i.e., R> 4.0.0), run the `R/00_install_dependencies.R` script to install all dependencies. Alternatively, open the `racial-wealth-gap.Rproj` file with RStudio and use `renv::restore()` to use the same package versions we used. 

### Reproducing the Analysis

Open the `racial-wealth-gap.Rproj` project with RStudio and run the `run_analysis.R` script to reproduce our analysis. The script takes about 30-45 minutes to run. Outputs will be created in the `./outputs` folder. Alternatively, one can step through the scripts listed in the `run_analysis.R` file to reproduce specific steps of the analysis.

### Folder structure

- **./R/scripts**: contains the scripts used in the analysis,. Run the scripts in that folder in the specified order to reproduce our analysis. Alternatively, run the `run_analysis.R` script to perform all steps in the analysis.
- **./R/funs**: contains the functions and classes used in the analysis.
- **./inputs**: contains all analysis inputs. SCF microdata from 2019 (1.3 Gb) will be automatically downloaded to this folder.
- **./outputs**: houses intermediate outputs created by the scripts.


## Contact

Reach out to [Pedro Nascimento de Lima](https://www.rand.org/about/people/l/lima_pedro_nascimento_de.html) for questions related to this repository.

## License 

Copyright (C) 2022 by The [RAND Corporation](https://www.rand.org). This repository is released as open-source software under a GPL-2.0 license. See the LICENSE file.
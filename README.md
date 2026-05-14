# Gender-Norms-and-Political-Violence
Replication materials for Birnir and Overos n.d

The analysis folder contains scripts that generate model output and tables for the paper. Running the 00_masater.R script runs all scripts, loads appropriate packages and installs needed packages if necessary. Each section of the script is saved as its own file.

- 01_load.R loads the data and packages
- 02_rf_prep.R creates objects requried for running the Random Forest model
- 03_rf_model.R runs the Random Forest model and generates variable importance results
- 04_ols_models.R runs all OLS models in both the main article and supplementary appendices
- 05_output.R generates the tables and saves them as latex files in the tables folder

The data folder contains the survey data in both RData and csv form.

The tables folder will hold the latex files for the research paper.

Output is an optional folder to save model results or other artifacts from the research pipeline if desired. It is currently empty.

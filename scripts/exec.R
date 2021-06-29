# create data directory and and then a project sub-directory into which you dump csv files 
# THAT project directory will be used perpetually
my_project_dir <- "sor-pilot3" # this will be placed under the 'data' directory
fn_name <- "my_data.xlsx"

# NO "/" allowed in these variables!!
drug_name <- "sorafenib"
drug_dosage <- "300"
trial_num <- "3"
freq <- "mgkgd"

# pick your colors! only HEX
# For inspiration -- https://www.colorhexa.com/ 
veh_color <- "#1e81b0" # blue
tx_color <- "#ed9942" # orange

## BP parameters
FILTER_OUT_LESS_THAN_CYCLES <- 5

################################################
########### SOURCE FILES ######################
################################################

fct_phases <- c("training", "baseline", "vehicle", "treatment", "treatment 2x")
tx_phases <- c("treatment", "treatment 2x")


source(file.path("scripts", "process-data.R"))
source(file.path("scripts", "main.R"))
source(file.path("scripts", "analysis.R"))

###################################
############### RUN ###############
###################################

process_data_fn(project_dir = my_project_dir)
res_lst <- run_main(my_project_dir)
run_plots_and_analysis(res_lst, my_project_dir)



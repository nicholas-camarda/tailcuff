#' @author Nicholas Camarda
# contact: ncamarda93@gmail.com

####################################################################
############# TAILCUFF BP ANALYSIS USER PARAMETERS ################
####################################################################

#' @EDIT-1
#' [1] set the working directory to the cloned repository 
#' (Option + right-click on cloned directory in finder, select 'copy as Pathname')
#' Paste this into setwd()

setwd("~/OneDrive - Tufts/phd/ws/tail-cuff")
# setwd("~/Desktop/tailcuff")

#' [2] create data directory and and then a project sub-directory into which you dump csv files 
#' [3] edit these variables below to your liking

# THAT project directory will be used perpetually
my_project_dir <- "sor-pilot3" # this is the name of the project under the "data" directory
fn_name <- "my_data.xlsx"

#' @EDIT-2
# NO "/" allowed in these variables!!
drug_name <- "sorafenib" # the name of the drug you are experimenting with, in quotes
drug_dosage <- "300" # the dosage of the drug, in quotes
trial_num <- "3" # the trial number (1,2,3), in quotes
freq <- "mgkgd" # the amount of drug per kg of body weight, per day, in quotes

#' @EDIT-3 pick your colors! only HEX
# For inspiration -- https://www.colorhexa.com/ 
veh_color <- "#1e81b0" # blue
tx_color <- "#ed9942" # orange

## BP parameters
FILTER_OUT_LESS_THAN_CYCLES <- 5

#' @TO-DO ADD in case control or cohort wide BP analysis modes
mode <- c("case-control")


############# DO NOT EDIT ####################
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

#' @RUN-1
#' [1] First run process_data_fn()
process_data_fn(project_dir = my_project_dir)

#' @RUN-2
#' [2] See example data and metadata files written into examples/
#' **BEFORE** running the next functions, adjust the Phases of the experiment in Excel!!
res_lst <- run_main(my_project_dir)
run_plots_and_analysis(res_lst, my_project_dir)



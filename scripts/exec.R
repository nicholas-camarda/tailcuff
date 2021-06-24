

# create data directory and and then a project subdirectory into which you dump csv files 
# THAT project directory will be used perpetually
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


source("./scripts/process-data.R")
source("./scripts/main.R")


# message("Checking for required packages...")
list_of_packages <- c("tidyverse", "GetoptLong")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# message("Done!")

# load
library(tidyverse)
library(GetoptLong)

###################################
########## FUNCTIONS ############
###################################

setwd(getwd())

#' @param project_dir the parent directory of your data, e.g. data/project_dir/*.xlsx
#' @note underneath *project_dir* should simply be .xlsx files
#' @output writes a processed data file to data/ directory called data-{date}.csv
process_data_fn <- function(project_dir){
  message("Processing data...")
  raw_data_dir_full <- file.path("data", project_dir)
  dir.create(raw_data_dir_full, showWarnings = FALSE, recursive = TRUE)
  
  suppressMessages(raw_data_tbl <- tibble(fn = list.files(raw_data_dir_full, full.names = TRUE)) %>%
    mutate(Date = str_extract(string = fn, pattern = "[0-9][0-9_]{7,}")) %>%
    unnest(cols = c(Date)) %>%
    mutate(Date = as.Date(Date, "%m_%d_%Y")) %>%
    mutate(dat = map(fn, read_csv)) %>%
    unnest(cols = c(dat)) %>%
    select(-fn) %>%
    relocate(Date, .after = last_col()) %>%
    mutate(Phase = "FILL IN") %>%
    arrange(Date, `Specimen Name`))
  
  clean_output_dir <- file.path("output", project_dir, "cleaned-data")
  dir.create(clean_output_dir, recursive = T, showWarnings = F)
  write_csv(raw_data_tbl, file.path(clean_output_dir, "my_data.csv"))
  message(qq("Done! Check output/@{project_dir}/cleaned-data"))
  message("BEFORE running the next step, adjust the Phases of the experiment in Excel!!")
}



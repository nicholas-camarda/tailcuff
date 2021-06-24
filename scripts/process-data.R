
list_of_packages <- c("tidyverse", "GetoptLong")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# load
library(tidyverse)
library(GetoptLong)

###################################
########## FUNCTIONS ############
###################################

#' @param data_dir the parent directory of your data, e.g. my_data/
#' @note underneath *data_dir* should simply be .xlsx files
#' @output writes a processed data file to data/ directory called data-{date}.csv
process_data_fn <- function(data_dir){
  message("Processing data...")
  raw_data_dir_full <- file.path(data_dir)
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
  
  # datetime <- gsub(x = format(Sys.time(), "%Y_%m_%d-%X"), pattern = ":", replacement = "")
  dir.create("output/cleaned-data", recursive = T, showWarnings = F)
  write_csv(raw_data_tbl, qq("output/cleaned-data/my_data.csv"))
  message("Done! Check output/cleaned-data and adjust the Phases of the experiment in Excel.")
}


###################################
############### RUN ###############
###################################

process_data_fn(data_dir = "data")


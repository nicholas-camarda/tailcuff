
list_of_packages <- c("tidyverse")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# load
library(tidyverse)

#' @param data_dir the parent directory of your data, e.g. my_data/
#' @note underneath *data_dir* should simply be .xlsx files
#' @output writes a processed data file to data/ directory
process_data_fn <- function(data_dir){
  raw_data_dir_full <- file.path(data_dir, project_dir)
  raw_data_tbl <- tibble(fn = list.files(raw_data_dir_full, full.names = TRUE)) %>%
    mutate(Date = str_split(string = fn, pattern = "-a_|-b_|\\.", simplify = TRUE)[,2]) %>%
    unnest() %>%
    mutate(Date = as.Date(Date, "%m_%d_%Y")) %>%
    mutate(dat = map(fn, read_csv)) %>%
    unnest() %>%
    select(-fn) %>%
    relocate(Date, .after = last_col()) %>%
    mutate(Phase = "training") %>%
    arrange(Date)
  
  write_csv(raw_data_tbl, "~/Downloads/temp.csv")
}

# move your dat
data_dir <- "data/"
process_data_fn()
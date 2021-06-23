
list_of_packages <- c("tidyverse")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# load
library(tidyverse)

#' @param data_dir the parent directory of your data, e.g. my_data/
#' @param project_dir the subdirectory containing your csv tailcuff files under *data_dir*, e.g. sor-pilot3/
process_data_fn <- function(data_dir, project_dir){
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



process_data_fn()
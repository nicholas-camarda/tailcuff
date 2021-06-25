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
  
  
  # example data and metadata - make once
  example_data_fn <- file.path(cleaned_data_dir, "example-data.pdf")
  example_metadata_fn <- file.path(cleaned_data_dir, "example-metadata.pdf")
  if (!file.exists(example_data_fn) | !file.exists(example_metadata_fn)) {
    message(qq("Writing example data and metadata files, for reference, into @{cleaned_data_dir}"))
    
    # make up an example that doesn't rely on previousyl stored data
    kable(head(data_temp_b, n = 15), caption = "Metadata", align = "c") %>%
      column_spec(column = c(ncol(data_temp_b)), background = "greenyellow") %>%
      kable_styling("striped") %>%
      save_kable(example_data_fn)
    
    kable(head(meta_df_all %>% arrange(`Date`, `Specimen Name`)), caption = "Metadata", align = "c") %>%
      kable_styling("striped") %>%
      save_kable(example_metadata_fn)
    message("Done!")
  }
  
  message(qq("Done! Check output/@{project_dir}/cleaned-data"))
  message(qq("See example data and metadata files written into output/@{project_dir}/cleaned-data"))
  message("\n**BEFORE** running the next step, adjust the Phases of the experiment in Excel!!\n")
 
}



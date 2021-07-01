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


#' @param project_dir the parent directory of your data, e.g. data/project_dir/*.xlsx
#' @note underneath *project_dir* should simply be .xlsx files
#' @output writes a processed data file to data/ directory called data-{date}.csv
process_data_fn <- function(project_dir){
  message("Processing data...")
  raw_data_dir_full <- file.path("data", project_dir)
  dir.create(raw_data_dir_full, showWarnings = FALSE, recursive = TRUE)
  
  raw_data_tbl_temp <- tibble(fn = list.files(raw_data_dir_full, recursive = TRUE, full.names = TRUE))
  print(raw_data_tbl_temp)
  
  if (nrow(raw_data_tbl_temp) == 0) {
    stop("There are no files in the data/<my_project_dir> directory")
  } 
  
  raw_data_tbl <- raw_data_tbl_temp %>%
    mutate(Date = str_extract(string = fn, pattern = "[0-9][0-9_]{7,}")) %>%
    unnest(cols = c(Date)) %>%
    mutate(Date = as.Date(Date, "%m_%d_%Y")) %>%
    mutate(dat = map(fn, read_csv)) %>%
    unnest(cols = c(dat)) %>%
    select(-fn) %>%
    relocate(Date, .after = last_col()) %>%
    mutate(Phase = "FILL IN") %>%
    arrange(Date, `Specimen Name`)
  
  clean_output_dir <- file.path("output", project_dir, "cleaned-data")
  dir.create(clean_output_dir, recursive = T, showWarnings = F)
  write_csv(raw_data_tbl, file.path(clean_output_dir, "my_data.csv"))
  
  example_data_dir <- file.path("examples")
  dir.create(example_data_dir, recursive = T, showWarnings = F)
  
  
  # example data and metadata - make once
  example_data_fn <- file.path(example_data_dir, "example-data.pdf")
  example_data_tsv_fn <- file.path(example_data_dir, "example-data-tsv.tsv")
  example_metadata_fn <- file.path(example_data_dir, "example-metadata.pdf")
  example_metadata_tsv_fn <- file.path(example_data_dir, "example-metadata-tsv.tsv")
  if (!file.exists(example_data_fn) | !file.exists(example_metadata_fn)) {
    message(qq("Writing example data and metadata files, for reference, into @{cleaned_data_dir}"))
    
    example_data_df <- tribble(~`Specimen Name`, ~Systolic, ~Mean, ~Rate, ~Cycle, ~Date, ~Phase,
                               "M1", 123, 140, 600, 1, as.Date("1994-04-04"), "training",
                               "M1", 150, 120, 700, 2, as.Date("1994-04-04"), "training",
                               "M2", 123, 140, 600, 1, as.Date("1994-04-04"), "training",
                               "M2", 150, 120, 700, 2, as.Date("1994-04-04"), "training",
                               "M1", 123, 140, 600, 1, as.Date("1994-04-04"), "baseline",
                               "M1", 150, 120, 700, 2, as.Date("1994-04-04"), "baseline",
                               "M2", 123, 140, 600, 1, as.Date("1994-04-04"), "baseline",
                               "M2", 150, 120, 700, 2, as.Date("1994-04-04"), "baseline")
    # make up an example that doesn't rely on previousyl stored data
    kable(head(example_data_df, n = 15), caption = "Metadata", align = "c") %>%
      column_spec(column = c(ncol(data_temp_b)), background = "greenyellow") %>%
      kable_styling("striped") %>%
      save_kable(example_data_fn)
    # for easy editing
    write_tsv(example_data_df, file = example_data_tsv_fn)
    
    example_meta_df <- tribble(~"Specimen Name", ~"Mouse Unique ID",	~"Gender",	~"Notch",	~"DOB",	~"Wean Date",	~"CageID",	~"Current Age (weeks)",	~"Date ready for Telemetry implant",	~"New CageID",	~"Body weight (g)", ~"Date",	~"Status",	~"Date of death", ~"Machine ID",
                               "M1", "1A", "Male", "BN", "2021-05-03", "2021-05-24", "662069", "8.14", "2021-06-28", "", "22.1", "2021-06-16", "Alive", "", 1,
                               "M2", "2A", "Male", "NN", "2021-05-03", "2021-05-24", "662069", "8.14", "2021-06-28", "", "22.1", "2021-06-16", "Alive", "", 1,
                               "M3", "1B", "Male", "RN", "2021-05-03", "2021-05-24", "662069", "8.14", "2021-06-28", "", "22.1", "2021-06-16", "Alive", "", 2,
                               "M1", "1A", "Male", "BN", "2021-05-03", "2021-05-24", "662069", "8.14", "2021-06-28", "", "22.1", "2021-06-18", "Alive", "", 1,
                               "M2", "2A", "Male", "NN", "2021-05-03", "2021-05-24", "662069", "8.14", "2021-06-28", "", "22.1", "2021-06-18", "Alive", "", 1,
                               "M3", "1B", "Male", "RN", "2021-05-03", "2021-05-24", "662069", "8.14", "2021-06-28", "", "22.1", "2021-06-18", "Dead", "2021-06-20", 2)
                      
    kable(example_meta_df, caption = "Metadata", align = "c") %>%
      kable_styling("striped") %>%
      save_kable(example_metadata_fn)
    write_tsv(example_meta_df, file = example_metadata_tsv_fn)
  
    message("Done!")
  }
  
  message(qq("Done! Check output/@{project_dir}/cleaned-data"))
  message(qq("See example data and metadata files written into examples/"))
  message("\n**BEFORE** running the next step, adjust the Phases of the experiment in Excel!!\n")
}



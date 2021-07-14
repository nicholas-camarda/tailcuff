



run_low_overhead <- function(my_project_dir){
  
  my_cur_dir <- getwd()
  data_dir <- file.path(my_cur_dir, "data", my_project_dir)
  output_dir <- file.path(my_cur_dir, "output", my_project_dir)
  cleaned_data_dir <- file.path(output_dir, "cleaned-data")
  
  results_dir <- file.path(output_dir, "results")
  dir.create(results_dir, recursive = T, showWarnings = FALSE)
  
}
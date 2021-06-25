# check for packages that need to be installed
# message("Checking for required packages...")
list_of_packages <- c("tidyverse", "rstatix", "ggthemes", "ggforce", "ggsci", "ggpubr", 
                      "readxl", "forcats", "knitr", "kableExtra", "gridExtra", "GetoptLong", "zoo", "randomizr")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# message("Done!")


# load
library(tidyverse)
library(rstatix)
library(ggthemes)
library(ggforce)
library(ggsci)
library(ggpubr)
library(rstatix) 
library(readxl)
library(forcats)
library(knitr)
library(kableExtra)
library(gridExtra)
library(GetoptLong)
library(zoo)
library(randomizr)


run_main <- function(my_project_dir){

  my_cur_dir <- getwd()
  data_dir <- file.path(my_cur_dir, "data", my_project_dir)
  dir.create(data_dir, showWarnings = FALSE)
  output_dir <- file.path(my_cur_dir, "output", my_project_dir)
  dir.create(output_dir, showWarnings = FALSE)
  cleaned_data_dir <- file.path(output_dir, "cleaned-data")
  dir.create(cleaned_data_dir, showWarnings = FALSE)
  
  #' reads in mega excel file 
  full_path_fn <- file.path(cleaned_data_dir, fn_name)
  if(!file.exists(full_path_fn)) {
    stop(qq("@{full_path_fn} does not exist."))
  }

  attr_df <- tibble(`Specimen Name` = str_c("M",seq(1, 12, 1)), 
                    shape_id = c(0:6,10:14)) 
  
  ## Read in the data
  data_temp_b <- read_excel(full_path_fn, sheet = 1) %>% 
    dplyr::select(`Specimen Name`, Systolic, Mean, Rate, `Cycle #`, `Date`, Phase) %>%
    mutate(Date = as.Date(Date, "GMT")) %>%
    arrange(Date, `Specimen Name`, `Cycle #`)

  
  num_mice <- nrow(data_temp_b %>% distinct(`Specimen Name`))
  data_temp <- data_temp_b %>%
    mutate(`Specimen Name` = factor(`Specimen Name`, levels = str_c("M", seq(1, num_mice, 1)))) %>%
    mutate(Phase = factor(Phase, levels = fct_phases)) %>%
    arrange(Phase, `Specimen Name`) 
  
  if(any("FILL IN" %in% data_temp_b$Phase)){
    message("There was an error. Please see example data and metadata in data/my-project/cleaned-data folder")
    stop("You must adjust the phases in the final excel file to one of:\ntraining, baseline, vehicle, treatment, treatment 2x")
  }
  
  meta_df_all <- read_excel(full_path_fn,
                            sheet = 2) %>%
    mutate(Date = as.Date(Date, "GMT"),
           DOB = as.Date(DOB, "GMT")) %>%
    arrange(Date, `Specimen Name`) %>%
    mutate(`Specimen Name` = factor(`Specimen Name`, levels = str_c("M", seq(1, num_mice, 1))),
           `Date` = as.factor(Date),
           `Body weight (g)` = as.numeric(`Body weight (g)`)) %>%
    group_by(`Specimen Name`)

  
  # Check dead mice
  dead_mice <- meta_df_all %>% 
    distinct(`Specimen Name`, `Date of death`) %>%
    na.omit() 
  
  meta_df_temp <- meta_df_all %>% 
    summarize(`Average body weight (g)` = mean(`Body weight (g)`, na.rm = T), .groups = "keep") 
  
  meta_df <- inner_join(meta_df_all %>% 
                          dplyr::select(-`Body weight (g)`, -`Date`) %>% 
                          distinct(), 
                        meta_df_temp, by = "Specimen Name") %>%
    arrange(`Specimen Name`) %>%
    anti_join(dead_mice %>% distinct(`Specimen Name`), by = "Specimen Name")
  
  
  # https://cran.r-project.org/web/packages/randomizr/vignettes/randomizr_vignette.html
  for_randomization_df <- data_temp %>% 
    group_by(`Specimen Name`, Phase) %>%
    summarize(mean_systolic = mean(`Systolic`), .groups = "keep") %>%
    filter(Phase == "baseline") %>%
    .$mean_systolic
  
  #' @note get random assignments such that blood pressure is roughly equal
  #' @param vec the vector of average BPs for the *baseline phase*
  #' @param m_each_ the number of subjects in each group
  #' @param tol_diff max BP difference (in mmHg) that you are willing to tolerate between groups, autoset to 3
  #' @return a list containing: 
  #' *rand_assignemnts* = the randomization assignments in order 
  #' *veh_mean* and *tx_mean* = the average BP values of each group, 
  #' *random_seed* = the random seed, 
  #' *machine_prop* = the proportion of veh/sun samples assigned to each machine (1 means equal proportions of veh/tx assigned to each machine)
  #' *num_veh* and *num_tx* = the number of samples in each group
  get_random_assign <- function(vec, m_each_ = c(4,6), tol_diff = 3) {
    rand <- sample(9999,1)
    set.seed(rand)
    # const initialize
    veh_mean <- 14359
    tx_mean <- 2234
    
    # machine placement
    prop <- 2
    
    while (abs(veh_mean - tx_mean) > tol_diff || abs(prop - 1) > 0.1 ){
      rand_assign <- complete_ra(N = length(vec),
                                 conditions = c("vehicle", drug_name), 
                                 m_each = m_each_)
      table(rand_assign) 
      veh_mean <- mean(vec[rand_assign == "vehicle"])
      tx_mean <- mean(vec[rand_assign == drug_name])
      
      m1 <- seq(1,length(vec)/2, by=1)
      m2 <- seq(m1[length(m1)]+1,length(vec), by=1)
      
      prop <- sum(as.integer(rand_assign[m1])) / sum(as.integer(rand_assign[m2]))
    }
    
    num_veh <- sum(rand_assign == "vehicle")
    num_tx <- sum(rand_assign == drug_name)
    
    return(list("rand_assignments" = rand_assign, "veh_mean" = veh_mean, 
                "tx_mean" = tx_mean, "random_seed" = rand, "machine_prop" = prop,
                "num_veh" = num_veh, "num_tx" = num_tx))
  }
  
  # create and write random assignment to file or load it
  random_assignment_dir <- file.path(output_dir, "random_assignment")
  dir.create(random_assignment_dir, showWarnings = FALSE, recursive = TRUE)
  rand_assign_fn <- file.path(random_assignment_dir, "group_assignments.tsv")
  if (!file.exists(rand_assign_fn)) {
    message("Performing randomization...")
    rand_lst <- get_random_assign(vec = for_randomization_df, m_each_ = c(4,6))
    
    # write down random assignment and random seed
    rand_samp_assign <- bind_cols(meta_df %>% distinct(`Specimen Name`), 
                                  group = rand_lst$rand_assignments) %>%
      bind_cols(as_tibble(rand_lst))
    
    write_tsv(rand_samp_assign, file = rand_assign_fn)
  } else {
    message("Found stored randomization information, loading cached data")
    rand_samp_assign <- suppressMessages(read_tsv(rand_assign_fn))
  }
  message("Done!")
  
  # meta data
  meta_df_w_assign <- meta_df_all %>% 
    left_join(rand_samp_assign %>% distinct(`Specimen Name`, group), by = "Specimen Name") %>%
    mutate(group = factor(group, levels = c(drug_name, "vehicle")),
           `Specimen Name` = factor(`Specimen Name`, levels = str_c("M", seq(1, num_mice, 1)))) %>%
    arrange(`Specimen Name`, group) %>%
    dplyr::select(group, everything()) 
  
  summary_meta_df_w_assign <- left_join(meta_df_w_assign %>%
                                          group_by(`Specimen Name`) %>%
                                          summarize(`Average body weight (g)` = mean(`Body weight (g)`)), 
                                        meta_df_w_assign %>% select(-`Body weight (g)`, -Date) %>% distinct(), 
                                        by = "Specimen Name")
  
  # merge data and meta
  data <- data_temp %>% 
    left_join(summary_meta_df_w_assign, by = "Specimen Name") %>% # inner join? right join?
    left_join(attr_df, by = "Specimen Name") %>%
    mutate(color = ifelse(group == "vehicle", veh_color, tx_color)) %>%
    mutate(color = factor(color, levels = c(veh_color, tx_color))) %>%
    mutate(group = factor(group, levels = c("vehicle", drug_name))) %>% # ensure factor in correct order
    mutate(`Specimen Name` = factor(`Specimen Name`, levels = unique(.$`Specimen Name`))) 
  
  message("Done loading data!")
  
  main_lst <- list("data_df" = data, "meta_data_df" = meta_df_w_assign, "meta_summary" = summary_meta_df_w_assign, "random_assignment_df" = rand_samp_assign)
  return(main_lst)
}



# check for packages that need to be installed
# message("Checking for required packages...")
list_of_packages <- c("tidyverse", "rstatix", "ggthemes", "ggforce", "ggsci", "ggpubr", 
                      "readxl", "forcats", "knitr", "kableExtra", "gridExtra","conflicted", "GetoptLong", "zoo", "randomizr", "magick")
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
library(magick)
library(gridExtra)
library(GetoptLong)
library(zoo)
library(randomizr)
library(conflicted)

conflict_prefer("recode", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("slice", "dplyr")

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
    stop(qq("@{full_path_fn} does not exist. Please see the README and/or 'examples/' for exampels of data and metadata structure.\nUpdate the 'output/<my_project_dir>/cleaned_data/my_data.xlsx file and try again."))
  }

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
  
  sample_id_df <- data_temp %>% 
    distinct(`Specimen Name`)
  
  attr_df <- sample_id_df %>%
    mutate(shape_id = seq(0, num_mice-1, 1))
  
  if(any("FILL IN" %in% data_temp_b$Phase)){
    message("There was an error. Please see example data and metadata in data/my-project/cleaned-data folder")
    stop("You must adjust the phases in the final excel file to one of:\ntraining, baseline, vehicle, treatment, treatment 2x")
  }
  
  meta_df_all_temp <- read_excel(full_path_fn, sheet = 2)
  if (nrow(meta_df_all_temp) != 0) {
    message("Loading meta data...")
    meta_df_all <- meta_df_all_temp %>% 
      mutate(Date = as.Date(Date, "GMT"),
           DOB = as.Date(DOB, "GMT")) %>%
      arrange(Date, `Specimen Name`) %>%
      mutate(`Specimen Name` = factor(`Specimen Name`, levels = str_c("M", seq(1, num_mice, 1))),
             `Date` = as.factor(Date),
             `Body weight (g)` = as.numeric(`Body weight (g)`)) %>%
      group_by(`Specimen Name`) %>%
      arrange(`Specimen Name`)
    
    message("Checking for dead mice...")
    # Check dead mice
    dead_mice <- meta_df_all %>% 
      filter(!is.na(`Date of death`)) %>%
      distinct()
    
    if (nrow(dead_mice) > 0 ){
      message(qq("Removing @{nrow(dead_mice)} dead mice..."))
      
      removed_dir <- file.path(output_dir, "removed")
      dir.create(removed_dir, recursive = TRUE, showWarnings = FALSE)
      dead_mice_fn <- file.path(removed_dir, "dead_mice.pdf")
      kable(dead_mice, caption = "Dead mice", align = "c") %>%
        kable_styling("striped") %>%
        save_kable(dead_mice_fn)
    }
    
    meta_df_temp <- meta_df_all %>% 
      summarize(`Average body weight (g)` = mean(`Body weight (g)`, na.rm = T), .groups = "keep") 
    
    meta_df <- meta_df_all %>% 
      filter(!(`Specimen Name` %in% dead_mice$`Specimen Name`)) %>%
      bind_rows(dead_mice) %>%
      left_join(meta_df_temp, by = "Specimen Name") %>%
      select(-`Body weight (g)`, -`Date`) %>%
      arrange(`Specimen Name`) %>%
      distinct()
    
    message("Done.")
  } else {
    message("No meta data. See examples to add additional information.")
    meta_df_all <- NULL
    meta_df <- NULL
  }
  
  
  diet_df_temp <- read_excel(full_path_fn, sheet = 3)
  if (nrow(diet_df_temp) != 0 & nrow(meta_df_all_temp) != 0){
    
    alive_meta_df <- meta_df %>% 
      filter(Status == "Alive") %>%
      arrange(`Specimen Name`)
    cage_order_int <- c(0,cumsum(as.numeric(with(alive_meta_df, `New CageID`[1:(length(`New CageID`)-1)] !=  `New CageID`[2:length(`New CageID`)])))) + 1
    cage_weight_df <- alive_meta_df %>%
      bind_cols(tibble(`Cage #` = cage_order_int)) %>%
      group_by(`Cage #`) %>%
      summarize(cage_weight = mean(`Average body weight (g)`)) %>%
      mutate(`Cage #` = str_c("C",`Cage #`, sep="")) 
    # num_mice_per_cage <- tibble(num_mice = as.numeric(table(cage_order_int)))
    
    diet_df <- diet_df_temp %>% 
      group_by(`Cage #`) %>%
      arrange(`Cage #`, Date) %>%
      left_join(cage_weight_df, by = "Cage #") %>%
      summarize(norm_water_diff = -1*c(0, diff(`Water (g)`))/`# Mice`, 
                norm_food_diff = -1*c(0, diff(`Food (g)`))/`# Mice`, .groups = "keep") %>% 
      bind_cols(diet_df_temp %>% arrange(`Cage #`, Date) %>% select(Date)) %>%
      mutate(Date = as.character(as.Date(Date))) %>%
      filter(norm_water_diff != 0) 
    
    water_g <- ggbarplot(diet_df, x = "Cage #", y = "norm_water_diff", 
           group = "Cage #", fill = "Cage #", palette = "jco", ggtheme = theme_bw()) + 
      ylab("Normalized water consumption (g water / mouse)")+
      ggtitle("Water consumption", subtitle = diet_df$Date); water_g
    
    food_g <- ggbarplot(diet_df, x = "Cage #", y = "norm_food_diff", 
           group = "Cage #", fill = "Cage #",  palette = "jco", ggtheme = theme_bw()) + 
      ylab("Normalized food consumption (g food / mouse)") +
      geom_hline(yintercept=3.5, linetype="dashed", 
                 color = "red", size=2) +
      ggtitle("Food consumption", subtitle = diet_df$Date) +
      labs(caption = "Red line indicates Lauren's minimum measurement\nfor average food consumption"); food_g
    
    diet_g_lst <- ggarrange(water_g, food_g, nrow = 1, ncol = 2, common.legend = TRUE)
    diet_g_lst_fn <- file.path(results_dir, "diet.pdf")
    ggsave(diet_g_lst, filename = diet_g_lst_fn, width = 8, height = 6)
    
  } else {
    diet_df <- NULL
  }
  

    
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
  get_random_assign <- function(vec, m_each_ = c(4,6), tol_diff = 2) {
    rand <- sample(9999,1)
    set.seed(rand)
    # mean const initialize
    veh_mean <- 14359
    tx_mean <- 2234
    
    # machine placement const. initialization
    prop <- 2
    
    while (abs(veh_mean - tx_mean) > tol_diff | abs(prop - 1) > 0.1 ){
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
    rand_samp_assign <- bind_cols(sample_id_df, 
                                  group = rand_lst$rand_assignments) %>%
      bind_cols(as_tibble(rand_lst))
    
    message("Done! Writing files...")
    write_tsv(rand_samp_assign, file = rand_assign_fn)
    
    # write it as a kable pdf as well
    sun_color_vec_kbl <- which(rand_samp_assign$group == drug_name)
    veh_color_vec_kbl <- which(rand_samp_assign$group == "vehicle")
    
    rand_assign_fn_pdf <- file.path(random_assignment_dir, "group_assignments.pdf")
    kable(rand_samp_assign, caption = "Randomization assignments", align = "c") %>%
      kable_styling("striped") %>%
      row_spec(sun_color_vec_kbl, bold = T, color = "white", background = "#ed9942") %>%
      row_spec(veh_color_vec_kbl, bold = T, color = "white", background = "#1e81b0") %>%
      save_kable(rand_assign_fn_pdf)
    
  } else {
    message("Found stored randomization information, loading cached data")
    rand_samp_assign <- suppressMessages(read_tsv(rand_assign_fn))
  }
  message("Done!")
  
  # meta data
  if (nrow(meta_df_all) != 0 | !is.null(meta_df_all)) {
    meta_df_w_assign <- meta_df_all %>% 
      left_join(rand_samp_assign %>% distinct(`Specimen Name`, group), by = c("Specimen Name")) %>%
      mutate(group = factor(group, levels = c(drug_name, "vehicle")),
             `Specimen Name` = factor(`Specimen Name`, levels = levels(data_temp$`Specimen Name`))) %>%
      arrange(`Specimen Name`, group) %>%
      dplyr::select(group, everything())  %>%
      filter(!(`Specimen Name` %in% unique(dead_mice$`Specimen Name`)))
    
    # dangerous join...
    summary_meta_df_w_assign <- suppressMessages(left_join(meta_df, 
                                          meta_df_w_assign %>% select(-`Body weight (g)`, -Date) %>% distinct()))  %>%
      filter(!(`Specimen Name` %in% unique(dead_mice$`Specimen Name`)))
    
    # merge data and meta
    data <- data_temp %>% 
      left_join(summary_meta_df_w_assign, by = "Specimen Name") %>% # inner join? right join?
      left_join(attr_df, by = "Specimen Name") %>%
      mutate(color = ifelse(group == "vehicle", veh_color, tx_color)) %>%
      mutate(color = factor(color, levels = c(veh_color, tx_color))) %>%
      mutate(group = factor(group, levels = c("vehicle", drug_name))) %>% # ensure factor in correct order
      mutate(`Specimen Name` = factor(`Specimen Name`, levels = unique(.$`Specimen Name`)))  %>%
      filter(!(`Specimen Name` %in% unique(dead_mice$`Specimen Name`)))
    data$`Specimen Name` <- droplevels(data$`Specimen Name`)
    
  } else {
    # merge data and meta
    data <- data_temp %>% 
      left_join(attr_df, by = "Specimen Name") %>%
      mutate(color = ifelse(group == "vehicle", veh_color, tx_color)) %>%
      mutate(color = factor(color, levels = c(veh_color, tx_color))) %>%
      mutate(group = factor(group, levels = c("vehicle", drug_name))) %>% # ensure factor in correct order
      mutate(`Specimen Name` = factor(`Specimen Name`, levels = unique(.$`Specimen Name`))) 
    meta_df_w_assign <- NULL
    summary_meta_df_w_assign <- NULL
  }
  
  message("Done loading data!")
  
  main_lst <- list("data_df" = data, "meta_data_df" = meta_df_w_assign, 
                   "meta_summary" = summary_meta_df_w_assign, "random_assignment_df" = rand_samp_assign)
  return(main_lst)
}



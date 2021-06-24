# check for packages that need to be installed
list_of_packages <- c("tidyverse", "rstatix", "ggthemes", "ggforce", "ggsci", "ggpubr", 
                      "readxl", "forcats", "knitr", "kableExtra", "gridExtra", "GetoptLong", "zoo", "randomizr")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


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


# set knitr chunk options
knitr::opts_chunk$set(echo = FALSE, message = FALSE, 
                      warning = FALSE, results = "asis", fig.width = 10, fig.height = 8)

# phases of experiment
fct_phases <- c("training", "baseline", "vehicle", "treatment", "treatment 2x")
tx_phases <- c("treatment", "treatment 2x")

# theme attributes
my_theme <- theme_stata() + 
  theme(plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(face = "italic", color = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")) 

my_cur_dir <- as.character(getwd())
data_dir <- file.path(my_cur_dir, "data")
dir.create(data_dir)
output_dir <- file.path(my_cur_dir, "output")
dir.create(output_dir)
cleaned_data_dir <- file.path(output_dir, "cleaned-data")
dir.create(cleaned_data_dir)

#' reads in mega excel file 
full_path_fn <- file.path(cleaned_data_dir, fn_name)
if(!file.exists(full_path_fn)) {
  stop(qq("@{full_path_fn} does not exist."))
}


#' other removed fn denotes other reasons why you might not want to include a mouse's data for a specific day
removed_dir <- file.path(output_dir, "removed")
other_removed_fn <- file.path(removed_dir, qq("other-removed_@{drug_name}_@{drug_dosage}_trial-@{trial_num}.csv"))
if (!file.exists(other_removed_fn)) {
  dir.create(removed_dir, recursive = T, showWarnings = F)
  message("Created output/removed directory. You can edit this in excel.")
  write_lines(x = c("Specimen Name",	"Date",	"group", "n",	"reason"), file = other_removed_fn, sep = ",")
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

meta_df_all <- read_excel(full_path_fn,
                               sheet = 2) %>%
  mutate(Date = as.Date(Date, "GMT"),
         DOB = as.Date(DOB, "GMT")) %>%
  arrange(Date, `Specimen Name`) %>%
  mutate(`Specimen Name` = factor(`Specimen Name`, levels = str_c("M", seq(1, 10, 1))),
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
                      meta_df_temp) %>%
  arrange(`Specimen Name`) %>%
  anti_join(dead_mice %>% distinct(`Specimen Name`))

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

rand_lst <- get_random_assign(vec = for_randomization_df, m_each_ = c(4,6))

rand_samp_assign <- bind_cols(meta_df %>% distinct(`Specimen Name`), 
                              group = rand_lst$rand_assignments)

meta_df_w_assign <- meta_df %>% 
  mutate(group = ifelse(group == 0, "vehicle", drug_name)) %>%
  mutate(group = factor(group, levels = c(drug_name, "vehicle"))) %>%
  arrange(`Specimen Name`, group) %>%
  dplyr::select(group, everything())

num_mice_per_group <- meta_df_w_assign %>% 
  group_by(group) %>%
  summarize(n = n())

num_mice_per_group_str <- num_mice_per_group %>% 
  group_by(group) %>%
  mutate(n_str = map2_chr(group, n, function(a,b) qq("N @{group} = @{n}"))) %>%
  pluck("n_str") %>% 
  str_c(collapse = " | ")


# merge data and meta
data <- data_temp %>% 
  right_join(meta_df_w_assign) %>% # inner join? right join?
  right_join(shape_id_df) %>%
  mutate(color = ifelse(group == "vehicle", "#1e81b0", "#ed9942")) %>%
  mutate(color = factor(color, levels = c("#1e81b0", "#ed9942"))) %>%
  mutate(group = factor(group, levels = c("vehicle", drug_name))) %>%
  mutate(`Specimen Name` = factor(`Specimen Name`, levels = unique(.$`Specimen Name`))) 


named_color_vec <- levels( data$color )
names(named_color_vec) <- levels(data$group)

shape_ids <- unique(data$shape_id)
names(shape_ids) <- levels(data$`Specimen Name`)

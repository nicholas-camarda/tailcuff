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




#' reads in mega excel file 
data_dir <- "/Users/ncamarda/OneDrive - Tufts/phd/projects/vegfri/sorafenib/mouse-studies/tail-cuff"
fn_name <- "Pilot 2 - sorafenib oral gavage - dosage 100 mgkgd - MASTER.xlsx"
full_path_fn <- file.path(data_dir, fn_name)

split_fn_name <- str_split(string = fn_name, pattern = " - ", simplify = TRUE)
trial_name <- split_fn_name[,1]
drug_name <- str_split(string = split_fn_name[,2], pattern = " ", simplify = TRUE)[,1]
drug_dosage <- str_split(string = split_fn_name[,3], pattern = " ", simplify = TRUE)[,2]


# Extract the trial num from fn_name, number next to "pilot"
trial_num <- str_split(string = fn_name, pattern = " ", simplify = T)[,2]

#' other removed fn denotes other reasons why you might not want to include a mouse's data for a specific day
other_removed_fn <- qq("./other_removed-@{drug_name}-@{trial_num}.csv")
if (!file.exists(other_removed_fn)) {
  file.create(other_removed_fn, showWarnings = FALSE)
}

shape_id_df <- tibble(`Specimen Name` = str_c("M",seq(1, 10, 1)), 
                      shape_id = c(0:6,10:12)) # c(0:6,10) 

## Read in the data
data_temp <- read_excel(full_path_fn, sheet = 1) %>% 
  dplyr::select(`Specimen Name`, Systolic, Mean, Rate, `Cycle #`, `Date`, Phase) %>%
  mutate(Date = as.Date(Date, "GMT")) %>%
  arrange(Date, `Specimen Name`, `Cycle #`) %>%
  mutate(`Specimen Name` = factor(`Specimen Name`, levels = str_c("M", seq(1, 10, 1)))) %>%
  # filter(Phase != "training", Phase != "tx1") %>%
  mutate(Phase = factor(Phase, levels = fct_phases)) %>%
  arrange(Phase, `Specimen Name`) 

meta_df_all_temp <- read_excel(full_path_fn,
                               sheet = 2) %>%
  mutate(Date = as.Date(Date, "GMT"),
         DOB = as.Date(DOB, "GMT")) %>%
  arrange(Date, `Specimen Name`) %>%
  mutate(`Specimen Name` = factor(`Specimen Name`, levels = str_c("M", seq(1, 10, 1))),
         `Date` = as.factor(Date),
         `Body weight (g)` = as.numeric(`Body weight (g)`)) %>%
  group_by(`Specimen Name`)


dead_mice <- meta_df_all_temp %>% 
  distinct(`Specimen Name`, `Date of death`) %>%
  na.omit() 

meta_df_temp <- meta_df_all_temp %>% 
  summarize(`Average body weight (g)` = mean(`Body weight (g)`, na.rm = T), .groups = "keep") 


meta_df_all <- meta_df_all_temp
meta_df <- inner_join(meta_df_all %>% 
                        dplyr::select(-`Body weight (g)`, -`Date`) %>% 
                        distinct(), 
                      meta_df_temp) %>%
  arrange(`Specimen Name`) %>%
  anti_join(dead_mice %>% distinct(`Specimen Name`))

# block random assignment by machine

# set.seed(3334) # pilot 1
set.seed(2223) # pilot 2

num_mice <- nrow(meta_df);
# https://cran.r-project.org/web/packages/randomizr/vignettes/randomizr_vignette.html


meta_df_w_assign <- meta_df %>% 
  bind_cols(tibble(group = complete_ra(N = num_mice, prob = 0.6))) %>%
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

library(tidyverse)
library(readxl)

df <- read_excel("/Users/ncamarda/OneDrive - Tufts/phd/projects/vegfri/sorafenib/mouse-studies/tail-cuff/Pilot 1 - vegfri oral gavage - old.xlsx")
sub <- df %>% filter(Date > "2021-05-11")
n_df <- df %>% filter(!(Date > "2021-05-11"))
new_sub <- sub %>% 
  mutate(`Specimen Name` = ifelse(`Specimen Name` == "M1", "M6-temp", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M2", "M7-temp", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M3", "M8-temp", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M6", "M1-temp", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M7", "M2-temp", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M8", "M3-temp", `Specimen Name`)) %>%
  mutate(`Specimen Name` = ifelse(`Specimen Name` == "M6-temp", "M6", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M7-temp", "M7", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M8-temp", "M8", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M1-temp", "M1", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M2-temp", "M2", `Specimen Name`),
         `Specimen Name` = ifelse(`Specimen Name` == "M3-temp", "M3", `Specimen Name`))  

final_df <- bind_rows(n_df, new_sub) %>% mutate(Date = as.Date(Date))
write_csv(final_df, "/Users/ncamarda/OneDrive - Tufts/phd/projects/vegfri/sorafenib/mouse-studies/tail-cuff/Pilot 1 - sorafenib oral gavage - copy fixed.csv" )

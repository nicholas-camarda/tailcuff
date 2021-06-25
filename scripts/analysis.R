# for saving Kables
if (!file.exists(webshot:::find_phantom())){
  webshot::install_phantomjs()
}


plots_and_analysis <- function(res_lst, my_project_dir){
  
  my_cur_dir <- getwd()
  data_dir <- file.path(my_cur_dir, "data", my_project_dir)
  output_dir <- file.path(my_cur_dir, "output", my_project_dir)
  cleaned_data_dir <- file.path(output_dir, "cleaned-data")
  
  # theme attributes
  my_theme <- theme_stata() + 
    theme(plot.title = element_text(face = "bold", size = 20),
          plot.subtitle = element_text(face = "italic", color = "black"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold")) 
  
  # load up data from main.R
  data <- res_lst$data_df
  meta_df_all <- res_lst$meta_data_df
  meta_df_w_assign <- res_lst$meta_summary
  
  num_mice_per_group <- meta_df_w_assign %>% 
    group_by(group) %>%
    summarize(n = n())
  
  num_mice_per_group_str <- num_mice_per_group %>% 
    group_by(group) %>%
    mutate(n_str = map2_chr(group, n, function(a,b) qq("N @{group} = @{n}"))) %>%
    pluck("n_str") %>% 
    str_c(collapse = " | ")
  
  named_color_vec <- levels( data$color )
  names(named_color_vec) <- levels(data$group)
  
  shape_ids <- unique(data$shape_id)
  names(shape_ids) <- levels(data$`Specimen Name`)
  
  meta_data_dir <- file.path(output_dir, "meta_data")
  dir.create(meta_data_dir, recursive = TRUE, showWarnings = FALSE)
  
  sun_color_vec_kbl <- which(meta_df_w_assign$group == drug_name)
  veh_color_vec_kbl <- which(meta_df_w_assign$group == "vehicle")
  
  meta_fn <- file.path(meta_data_dir, qq("@{my_project_dir}_meta.pdf"))
  # if (!(file.exists(meta_fn))) {
  message("Saving meta data table...")
  kable(meta_df_w_assign, caption = "Metadata", align = "c") %>%
    kable_styling("striped") %>%
    row_spec(sun_color_vec_kbl, bold = T, color = "white", background = "#ed9942") %>%
    row_spec(veh_color_vec_kbl, bold = T, color = "white", background = "#1e81b0") %>%
    save_kable(meta_fn)
  message("Done.")
  # }
  
  
  meta_df_weight <- full_join(meta_df_all, 
                              meta_df_w_assign %>% 
                                ungroup() %>% 
                                distinct(`Specimen Name`, group, `Average body weight (g)`), by = c("Specimen Name", "group")) %>%
    filter(Status == "Alive") %>% 
    distinct(`Specimen Name`, group,  Date, `Body weight (g)`, `Average body weight (g)`)
  
  specimen_weight_g <- ggplot(meta_df_weight, aes(fill = `group`, x = `Specimen Name`, y = `Body weight (g)`)) +
    geom_point() + 
    geom_boxplot(position = "dodge", alpha=0.8) +
    stat_summary(fun="mean", color = "cyan", show.legend = F) + # position = "dodge", stat = "identity"
    scale_fill_manual(name = "group", values = named_color_vec) +
    my_theme +
    scale_y_continuous(breaks=seq(20,30,2)) +
    ylab("Body weight (g)") +
    ggtitle("Average animal body weight (g)") + 
    geom_text(data = meta_df_w_assign, 
              aes(x = `Specimen Name`, y = `Average body weight (g)`, 
                  label = round(`Average body weight (g)`,1), vjust = 2.5), 
              hjust = 0.5, color = "black", size = 5, 
              inherit.aes = FALSE) 
  
  
  dates <- unique(meta_df_all$Date)
  reps <- length(unique(meta_df_all$`Specimen Name`))
  date_tbl <- tibble(Date = rep(dates, reps))
  
  weight_change_df <- meta_df_weight %>%
    arrange(Date) %>%
    group_by(`Specimen Name`) %>% 
    summarize(`Weight change (g)` = `Body weight (g)` - `Body weight (g)`[1L], .groups = "keep") %>%
    bind_cols(date_tbl) %>%
    mutate(Date = as.Date(Date)) %>%
    left_join(data %>% distinct(`Specimen Name`, group), by = "Specimen Name")
  
  min_weight_chg <- weight_change_df %>% na.omit() %>% .$`Weight change (g)` %>% min()
  max_weight_chg <- weight_change_df %>% na.omit() %>% .$`Weight change (g)` %>% max()
  weight_change_g <- ggplot(weight_change_df %>% na.omit(), aes(x = `Date`, y = `Weight change (g)`, color = `group`)) +
    geom_point(show.legend = F) + 
    geom_line() + 
    geom_smooth(method = "lm", color = "cyan", linetype = "dashed") +
    scale_color_manual(name = "group", values = named_color_vec) + 
    my_theme +
    scale_fill_manual(name = "Weight change (g)", values = named_color_vec) + 
    scale_y_continuous(breaks = seq(min_weight_chg, max_weight_chg)) +
    scale_x_date(breaks = unique(weight_change_df$Date)) +
    theme(axis.text.x = element_text(angle = 75, size = 6, vjust = 0.5),
          axis.text.y = element_text(hjust = 0.55),
          panel.spacing = unit(0.5, "lines")) +
    labs(title = "Weight change (g) since beginning of experiment\n") +
    facet_grid(cols = vars(`Specimen Name`))
  
  ggarrange(specimen_weight_g, weight_change_g, ncol = 2)
  
  remove_bad_days_df1 <- data %>%
    group_by(`Specimen Name`, Date, group, color) %>%
    summarize(n = n(), .groups = "keep") %>%
    arrange(Date, `Specimen Name`) 
  
  accepted_cycles_g <- ggplot(remove_bad_days_df1, aes(color = `group`, x = Date, y = n)) +
    geom_point(show.legend = F) + 
    geom_line() + 
    geom_smooth(method = "lm", color = "cyan", linetype = "dashed") +
    # geom_bar(position = "dodge", stat = "identity") +
    scale_color_manual(name = "group", values = named_color_vec) +
    scale_x_date(breaks = unique(remove_bad_days_df1$Date)) + 
    my_theme +
    scale_y_continuous(breaks=seq(0,35,5)) +
    theme(axis.text.x = element_text(angle = 60, hjust=1, size = 5)) +
    ylab("Number of Accepted Cycles") +
    ggtitle("Accepted cycles per mouse over time") + 
    facet_grid(cols = vars(`Specimen Name`))
  
  
  ggarrange(specimen_weight_g, accepted_cycles_g, ncol = 2)
  
  dates <- unique(meta_df_all$Date)
  reps <- length(unique(meta_df_all$`Specimen Name`))
  date_tbl <- tibble(Date = rep(dates, reps))
  
  
}

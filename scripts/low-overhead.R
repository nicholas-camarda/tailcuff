



run_low_overhead <- function(my_project_dir){
  
  my_cur_dir <- getwd()
  data_dir <- file.path(my_cur_dir, "data", my_project_dir)
  output_dir <- file.path(my_cur_dir, "output", my_project_dir)
  cleaned_data_dir <- file.path(output_dir, "../cleaned-data")
  results_dir <- file.path(output_dir, "results", "low-overhead")
  
  walk(list(data_dir, output_dir, cleaned_data_dir, results_dir), dir.create, showWarnings = FALSE, recursive = TRUE)

  full_path_fn <- file.path(cleaned_data_dir, fn_name)
  if(!file.exists(full_path_fn)) {
    stop(qq("@{full_path_fn} does not exist. Please see the README and/or 'examples/' for exampels of data and metadata structure.\nUpdate the 'output/<my_project_dir>/cleaned_data/my_data.xlsx file and try again."))
  }
  
  ## Read in the data
  data_temp_b <- read_excel(full_path_fn, sheet = 1) %>% 
    dplyr::select(`Specimen Name`, Systolic, Mean, Rate, `Cycle #`, `Date`, Phase) %>%
    mutate(Date = as.Date(Date, "GMT")) %>%
    arrange(Date, `Specimen Name`, `Cycle #`)
  
  if("Phase" %in% colnames(data_temp_b)){
    data_temp_b <- data_temp_b %>% select(-Phase)
  }
  
  num_mice <- nrow(data_temp_b %>% distinct(`Specimen Name`))
  
  specimen_levels <- str_c("M", seq(1, num_mice, 1))
                           
  data_temp <- data_temp_b %>%
    mutate(`Specimen Name` = factor(`Specimen Name`, levels = specimen_levels))
  
  sample_id_df <- data_temp %>% 
    distinct(`Specimen Name`)
  
  attr_df <- sample_id_df %>%
    mutate(shape_id = seq(0, num_mice-1, 1))
  
  
  cutoff_dates_df_temp <- data_temp %>%
    distinct(Date) %>%
    arrange(Date)
  
  # segment into groups based on measurement dates
  # sub_g becomes phase
  cutoff_dates_df_temp$Phase <- c(0,cumsum(as.numeric(with(cutoff_dates_df_temp, Date[2:length(Date)] - Date[1:(length(Date)-1)] > 1)))) + 1 
  
  cutoff_dates_df <- cutoff_dates_df_temp %>% 
    group_by(Phase) %>% 
    summarize(first = first(Date), last = last(Date), .groups = "keep") %>%
    mutate_all(.funs = as.character) %>%
    mutate(date_range = map2_chr(first, last, function(f,l){
      res <- str_c(c(f, l), collapse = " to ")
      return(res)
    })) %>%
    mutate(`Number of Days` = as.integer(as.Date(last) - as.Date(first)) + 1) %>%
    ungroup() %>%
    mutate(Phase = as.factor(Phase))
  
  # merge data and meta
  data <- data_temp %>% 
    left_join(attr_df, by = "Specimen Name") %>%
    left_join(cutoff_dates_df_temp, by = "Date") %>%
    mutate(`Specimen Name` = factor(`Specimen Name`, levels = specimen_levels)) %>%
    mutate(Phase = as.factor(Phase)) 
  
  
  fences_df <- data %>%
    ungroup() %>%
    dplyr::select(`Specimen Name`, Date, `Cycle #`, Systolic, Phase) %>%
    dplyr::group_by(`Specimen Name`, `Date`, Phase) %>%
    dplyr::summarize(median = median(Systolic),
                     iqr = IQR(Systolic),
                     min = min(Systolic),
                     max = max(Systolic),
                     first_quantile = quantile(Systolic, 0.25),
                     third_quantile = quantile(Systolic, 0.75), .groups = "keep") %>%
    mutate(lower_fence = first_quantile - iqr*2,
           upper_fence = third_quantile + iqr*2) %>%
    distinct(`Specimen Name`, Date, lower_fence, upper_fence)
  
  
  mark_outliers_df <- data %>%
    group_by(`Specimen Name`, `Date`, Phase) %>%
    left_join(fences_df, by = c("Specimen Name", "Date")) %>%
    mutate(is.outlier = Systolic > upper_fence | Systolic < lower_fence) %>%
    dplyr::select(is.outlier, everything())

  # remove all rows that are outliers
  # set factor 
  final_filtered_data <- mark_outliers_df %>%
    filter(!is.outlier) %>%
    group_by(`Specimen Name`, `Date`, Phase) %>%
    arrange(`Specimen Name`, Phase) %>%
    mutate(`Specimen Name` = factor(`Specimen Name`, levels = specimen_levels))
  
  
  #' @note analysis of outlier removal
  with_outliers_g <- ggplot(mark_outliers_df, aes(x = `Specimen Name`, y = Systolic)) + 
    geom_boxplot() + 
    geom_point(data = mark_outliers_df %>% filter(is.outlier), aes(x = `Specimen Name`, y = Systolic), color = "magenta", shape = 17, size = 3) + 
    my_theme + 
    ggtitle("Outliers marked") + 
    facet_grid(~ Date) +
    theme(axis.text.x = element_text(angle = 60, hjust=1, size = rel(0.75)),
          axis.text.y = element_text(hjust = 0.5))  + 
    facet_wrap(~ Date, ncol = 5) +
    labs(caption = "Pink triangles = where outliers are!")
  
  # just plot the watermark of where the outliers used to be...
  without_outliers_g <- ggplot(final_filtered_data, aes(x = `Specimen Name`, y = Systolic)) + 
    geom_boxplot() + 
    geom_point(data = mark_outliers_df %>% filter(is.outlier), 
               aes(x = `Specimen Name`, y = Systolic), color = "magenta", shape = 17, size = 3, alpha = 0.3) + 
    # scale_y_continuous(breaks = seq(50,220,20)) + 
    my_theme + 
    theme(axis.text.x = element_text(angle = 60, hjust=1, size = rel(0.75)),
          axis.text.y = element_text(hjust = 0.5)) +
    ggtitle("Outliers removed") + 
    facet_wrap(~ Date, ncol = 5) + 
    labs(caption = "Pink triangles = where outliers used to be!")
  
  outliers_lst_g <- ggarrange(with_outliers_g, without_outliers_g, nrow = 2)
  
  outliers_res_fn <- file.path(results_dir, "outliers.pdf")
  ggsave(outliers_lst_g, file = outliers_res_fn, width = 20, height = 20)
  ### analysis
  
  outlier_mice_df <- final_filtered_data %>% 
    dplyr::select(`Specimen Name`, Date, `Cycle #`, Systolic) %>%
    dplyr::group_by(`Specimen Name`, Date, Phase) %>% # across all dates
    dplyr::summarize(sem = calculate_sem(Systolic), .groups = "keep")
  
  
  #' @Note main df! this is specimen average
  specimen_avg_data_df <- final_filtered_data %>% 
    summarize(`specimen_mean_systolic` = mean(Systolic),
              specimen_systolic_sem = calculate_sem(Systolic),
              specimen_hr_mean = mean(Rate),
              specimen_hr_sem = calculate_sem(Rate),
              .groups = "keep") %>%
    # necessary?
    ungroup()
  
  three_day_df <- specimen_avg_data_df %>%
    group_by(`Specimen Name`, Phase) %>%
    mutate(n = n():1) %>%
    filter(n %in% c(1:3))
  
  all_sumarized <- three_day_df %>% 
    arrange(`Specimen Name`, Phase, Date) %>%
    group_by(`Specimen Name`, Phase) %>% 
    summarize(sys_mean = mean(specimen_mean_systolic),
              hr_mean = mean(specimen_hr_mean), .groups = "keep") %>%
    left_join(cutoff_dates_df, by = "Phase") %>%
    left_join(three_day_df %>% ungroup() %>% distinct(`Specimen Name`), by = "Specimen Name") %>%
    ungroup() %>%
    select(-`Number of Days`) 
    
  
  # calculations
  all_sumarized_diff <- all_sumarized %>%
    group_by(`Specimen Name`) %>% #
    arrange(`Specimen Name`, Phase) %>%
    mutate(sys_mean_diff = sys_mean - sys_mean[1L], # subtract from each mouse's baseline
              hr_mean_diff = hr_mean - hr_mean[1L]) %>%
    left_join(three_day_df %>% ungroup() %>% distinct(`Specimen Name`), by = "Specimen Name") 
    left_join(cutoff_dates_df, by = "Phase") %>%
    mutate(unique_id = make.unique(as.character(`Specimen Name`))) %>% 
    ungroup() %>%
    # mutate(Phase = factor(Phase, levels = fct_phases)) %>%
    filter(sys_mean_diff != 0)
  
  all_sumarized_diff$`Specimen Name` <- droplevels(all_sumarized_diff$`Specimen Name`)
  
  raw_sys1_g <- ggbarplot(all_sumarized, x = "date_range", y = "sys_mean", 
                          add = c("mean_se", "jitter"), scales = "free_x",
                          palette = named_color_vec,
                          ggtheme = theme_bw()) +
    scale_y_continuous(breaks = seq(round(min(all_sumarized$sys_mean),0),  round(max(all_sumarized$sys_mean),0), 10)) +
    ggtitle("Raw Systolic BP") +
    ylab("Mean systolic BP (mmHg)") + 
    xlab("Phase") +
    stat_compare_means(method = "t.test", 
                       comparisons = list(c(1,2), c(2,3), c(1,3)),
                       label = "p.format", 
    ); 
  
  raw_sys1_g <- ggpar(raw_sys1_g, ylim = c(min(all_sumarized$sys_mean), max(all_sumarized$sys_mean) + 5)); raw_sys1_g
  
  
  delta_sys1_g <- ggbarplot(all_sumarized_diff, x = "date_range", y = "sys_mean_diff", 
                            add = c("mean_se", "jitter"), 
                            palette = named_color_vec,
                            ggtheme = theme_bw()) +
    ggtitle("Difference from first set of measurements") +
    ylab("Mean systolic BP\ndifference from Phase (mmHg)") + 
    xlab("Phase")  +  
    stat_compare_means(method = "t.test", 
                       comparisons = list(c(1,2), c(2,3), c(1,3)),
                       label = "p.format", 
    ); delta_sys1_g


  sys_bp_g_lst <- ggarrange(raw_sys1_g, delta_sys1_g, ncol = 2,
                            nrow = 1, common.legend = TRUE) 
  
  sys_bp_g_lst_fn <- file.path(results_dir, "bp_change.pdf")
  ggsave(sys_bp_g_lst, file = sys_bp_g_lst_fn, width = 14, height = 6)
  
}
# for saving Kables
if (!file.exists(webshot:::find_phantom())){
  webshot::install_phantomjs()
}

#' @note helper function to calculate the standard error of the mean
#' @param x numeric vector on which to calculate sem
calculate_sem <- function(x) {
  return(sd(x, na.rm = TRUE) / sqrt(length(x)))
}



run_plots_and_analysis <- function(res_lst, my_project_dir){
  
  my_cur_dir <- getwd()
  data_dir <- file.path(my_cur_dir, "data", my_project_dir)
  output_dir <- file.path(my_cur_dir, "output", my_project_dir)
  cleaned_data_dir <- file.path(output_dir, "cleaned-data")
  
  results_dir <- file.path(output_dir, "results")
  dir.create(results_dir, recursive = T, showWarnings = FALSE)
  
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
  
  if(is.null(meta_df_all) | is.null(meta_df_w_assign)){
    stop("Please create a metadata sheet in the my_data.xlsx file according to the examples in the ./examples folder")
  }
  
  num_mice_per_group <- meta_df_w_assign %>% 
    group_by(group) %>%
    summarize(n = n())
  num_mice <- sum(num_mice_per_group$n)
  
  num_mice_per_group_str <- num_mice_per_group %>% 
    group_by(group) %>%
    mutate(n_str = map2_chr(group, n, function(a,b) qq("N @{group} = @{n}"))) %>%
    pluck("n_str") %>% 
    str_c(collapse = " | ")
  
  named_color_vec <- levels( data$color )
  names(named_color_vec) <- levels(data$group)
  
  shape_ids <- unique(data$shape_id)
  names(shape_ids) <- levels(data$`Specimen Name`)
  
  sun_color_vec_kbl <- which(meta_df_w_assign$group == drug_name)
  veh_color_vec_kbl <- which(meta_df_w_assign$group == "vehicle")
  
  meta_data_dir <- file.path(output_dir, "meta_data")
  dir.create(meta_data_dir, recursive = TRUE, showWarnings = FALSE)
  meta_fn <- file.path(meta_data_dir, qq("@{my_project_dir}_meta.pdf"))
  
  message("Saving meta data table...")
  kable(meta_df_w_assign, caption = "Metadata", align = "c") %>%
    kable_styling("striped") %>%
    row_spec(sun_color_vec_kbl, bold = T, color = "white", background = "#ed9942") %>%
    row_spec(veh_color_vec_kbl, bold = T, color = "white", background = "#1e81b0") %>%
    save_kable(meta_fn)
  message("Done.")
  
  
  ### meta data anlysis
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
              hjust = 0.5, color = "black", size = rel(0.75), 
              inherit.aes = FALSE) 
  
  date_tbl <- meta_df_all %>% 
    ungroup() %>%
    distinct(`Specimen Name`, Date) %>%
    select(Date)
  
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
    geom_smooth(method = "lm", color = "cyan", linetype = "dashed", se = FALSE) +
    scale_color_manual(name = "group", values = named_color_vec) + 
    my_theme +
    scale_fill_manual(name = "Weight change (g)", values = named_color_vec) + 
    scale_y_continuous(breaks = seq(min_weight_chg, max_weight_chg)) +
    scale_x_date(breaks = unique(weight_change_df$Date)) +
    theme(axis.text.x = element_text(angle = 75, size = rel(0.75), vjust = 0.5),
          axis.text.y = element_text(hjust = 0.55),
          panel.spacing = unit(0.5, "lines")) +
    labs(title = "Weight change (g) since beginning of experiment\n") +
    facet_grid(cols = vars(`Specimen Name`))
  
  
  # daily average weight loss
  daily_lost_weight_avg_df <- weight_change_df %>% 
    na.omit() %>% 
    group_by(Date) %>%
    summarize(weight_change_avg = mean(`Weight change (g)`))
  
  final_avg_weight <- daily_lost_weight_avg_df %>% slice(n()) 
  # take final avg weight change
  
  weight_change_avg_g <- ggplot(daily_lost_weight_avg_df, aes(x = `Date`, y = `weight_change_avg`)) +
    geom_point(show.legend = F) + 
    geom_line() + 
    geom_smooth(method = "lm") +
    my_theme +
    geom_label(data = final_avg_weight, 
               label = round(final_avg_weight$weight_change_avg, 2), 
               vjust = 1.5, color = "red") +
    ylab("Average daily weight change (g)") + 
    # scale_y_continuous(breaks = seq(-10,10,.2)) +
    scale_x_date(breaks = unique(weight_change_df$Date)) +
    theme(axis.text.x = element_text(angle = 75, size = rel(0.75), vjust = 0.5),
          axis.text.y = element_text(hjust = 0.55),
          panel.spacing = unit(1, "lines")) +
    labs(title = "Average weight change (g) across all mice\nsince beginning of experiment\n")  
  
  # plot arranged weight data
  weight_lst_g <- ggarrange(weight_change_avg_g, ncol = 1,
                            ggarrange(specimen_weight_g, weight_change_g, ncol = 2), 
                            nrow = 2, heights = c(1, 0.8, 0.8)) #
  
  weight_res_fn <- file.path(results_dir, "weight_change.pdf")
  ggsave(weight_lst_g, file = weight_res_fn, width = 16, height = 16)
  
  remove_bad_days_df1 <- data %>%
    group_by(`Specimen Name`, Date, group, color) %>%
    summarize(n = n(), .groups = "keep") %>%
    arrange(Date, `Specimen Name`)  %>% 
    mutate(Date = as.character(Date))
  
  accepted_cycles_g <- ggplot(remove_bad_days_df1, aes(color = `group`, group = `Specimen Name`, x = Date, y = n)) +
    geom_point(show.legend = F) + 
    geom_line() + 
    geom_smooth(method = "lm", color = "cyan", linetype = "dashed") +
    # geom_bar(position = "dodge", stat = "identity") +
    scale_color_manual(name = "group", values = named_color_vec) +
    # scale_x_date(breaks = unique(remove_bad_days_df1$Date)) + 
    my_theme +
    scale_y_continuous(breaks=seq(0,35,5)) +
    theme(axis.text.x = element_text(angle = 60, hjust=1, size = rel(0.75)),
          axis.text.y = element_text(hjust = 0.5)) +
    ylab("Number of Accepted Cycles") +
    ggtitle("Accepted cycles per mouse over time") + 
    facet_grid(cols = vars(`Specimen Name`))
  
  accepted_cycles_lst_g <- ggarrange(accepted_cycles_g, ncol = 1, nrow = 1, heights = c(0.4))
  
  cycles_res_fn <- file.path(results_dir, "accepted_cycles.pdf")
  ggsave(accepted_cycles_lst_g, file = cycles_res_fn, width = 12, height = 8)
  
  ### Blood pressure data analysis
  
  #' other removed fn denotes other reasons why you might not want to include a mouse's data for a specific day
  removed_dir <- file.path(output_dir, "removed")
  other_removed_fn <- file.path(removed_dir, qq("other-removed_@{drug_name}_@{drug_dosage}_trial-@{trial_num}.csv"))
  if (!file.exists(other_removed_fn)) {
    dir.create(removed_dir, recursive = T, showWarnings = F)
    message("Created output/removed directory. You can edit this in excel.")
    write_lines(x = c("Specimen Name",	"Date",	"group", "n",	"reason"), file = other_removed_fn, sep = ",")
  }
  
  
  removed_df_temp <- inner_join(remove_bad_days_df1 %>% ungroup() %>% filter(n < FILTER_OUT_LESS_THAN_CYCLES), 
                                remove_bad_days_df1 %>% ungroup(), by = c("Specimen Name", "Date", "group", "color", "n")) %>%
    dplyr::select(-color) %>%
    mutate(reason = "Low cycle count")
  
  other_df <- suppressMessages(suppressWarnings(read_csv(other_removed_fn) %>% select(-X6))) 
  removed_df <- bind_rows(removed_df_temp %>% mutate(Date = as.Date(Date),
                                                     n = as.character(n)), 
                          other_df %>% mutate(Date = as.Date(Date),
                                              n = as.character(n))) %>% arrange(Date)
  
  # print("Removed days/Specimens:")
  removed_fn_formatted_tbl <- file.path(meta_data_dir, qq("formated-removed-samples.pdf"))
  kable(removed_df %>% rename(`# cycles` = n),
        caption = qq("Removed days/Specimens with less than @{FILTER_OUT_LESS_THAN_CYCLES} cycles:")) %>%
    kable_styling("striped") %>%
    save_kable(removed_fn_formatted_tbl)
  
  # filtered df
  bad_days_removed_df <- anti_join(data, removed_df, by = c("Specimen Name", "Date", "group")) 
  
  ### Removing outliers
  # Detect outliers BP measurements using boxplot methods. Boxplots are a popular and an easy method for identifying 
  # outliers. There are two categories of outlier: (1) outliers and (2) extreme points. Values above Q3 + 2xIQR or 
  # below Q1 - 2xIQR are considered as outliers. Q1 and Q3 are the first and third quartile, respectively. 
  # IQR is the interquartile range (IQR = Q3 - Q1). This method is more robust than STDEV based outlier detection 
  # because outliers can skew the mean and STDEV of a sample. 
  # Here, outliers are nominated based on *daily* blood pressure recordings, so as to not throw out data on treatment 
  # days when the blood pressure is expected to rise above the average. 
  # Additionally, we remove mice that are too 'volatile' after trianing period has finished.
  
  fences_df <- bad_days_removed_df %>%
    ungroup() %>%
    dplyr::select(`Specimen Name`, Date, `Cycle #`, Systolic) %>%
    dplyr::group_by(`Specimen Name`, `Date`) %>%
    dplyr::summarize(median = median(Systolic),
                     iqr = IQR(Systolic),
                     min = min(Systolic),
                     max = max(Systolic),
                     first_quantile = quantile(Systolic, 0.25),
                     third_quantile = quantile(Systolic, 0.75), .groups = "keep") %>%
    mutate(lower_fence = first_quantile - iqr*2,
           upper_fence = third_quantile + iqr*2) %>%
    distinct(`Specimen Name`, Date, lower_fence, upper_fence)
  
  
  mark_outliers_df <- bad_days_removed_df %>%
    group_by(`Specimen Name`, `Date`) %>%
    left_join(fences_df, by = c("Specimen Name", "Date")) %>%
    mutate(is.outlier = Systolic > upper_fence | Systolic < lower_fence) %>%
    dplyr::select(is.outlier, everything())
  
  # count_outliers <- mark_outliers_df %>% 
  #   group_by(`Specimen Name`) %>%
  #   summarize(`# Outliers` = sum(is.outlier)) %>%
  #   filter(`# Outliers` != 0) %>%
  #   arrange(`Specimen Name`)
  
  # remove all rows that are outliers
  # set factor 
  final_filtered_data <- mark_outliers_df %>%
    filter(!is.outlier) %>%
    group_by(`Specimen Name`, `Date`, Phase, color, group) %>%
    mutate(Phase = factor(Phase, levels = fct_phases)) %>%
    arrange(Phase)
  
  
  #' @note analysis of outlier removal
  with_outliers_g <- ggplot(mark_outliers_df, aes(x = `Specimen Name`, y = Systolic, color = group)) + 
    geom_boxplot() + 
    scale_color_manual(name = "group", values = named_color_vec) +
    geom_point(data = mark_outliers_df %>% filter(is.outlier), aes(x = `Specimen Name`, y = Systolic), color = "magenta", shape = 17, size = 3) + 
    my_theme + 
    ggtitle("Outliers marked") + 
    facet_grid(~ Date) +
    theme(axis.text.x = element_text(angle = 60, hjust=1, size = rel(0.75)),
          axis.text.y = element_text(hjust = 0.5)) 
  
  
  # just plot the watermark of where the outliers used to be...
  without_outliers_g <- ggplot(final_filtered_data, aes(x = `Specimen Name`, y = Systolic, color = group)) + 
    geom_boxplot() + 
    scale_color_manual(name = "group", values = named_color_vec) +
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
    filter(Phase != "training") %>%
    dplyr::select(`Specimen Name`, Date, `Cycle #`, Systolic) %>%
    dplyr::group_by(`Specimen Name`, Phase) %>% # across all dates
    dplyr::summarize(sem = sd(Systolic)/sqrt(num_mice), .groups = "keep")
  
  
  #' @Note main df! this is specimen average
  specimen_avg_data_df <- final_filtered_data %>% 
    summarize(`specimen_mean_systolic` = mean(Systolic), 
              # `specimen_median_systolic` = median(Systolic),
              # specimen_sd_systolic = sd(Systolic),
              specimen_systolic_sem = calculate_sem(Systolic),
              specimen_hr_mean = mean(Rate),
              specimen_hr_sem = calculate_sem(Rate),
              .groups = "keep") %>%
    # necessary?
    ungroup() %>%
    mutate(Phase = factor(Phase, levels = fct_phases))
  
  title_ <- qq("\n## Trial @{trial_num} | @{drug_name} @{drug_dosage} @{freq}\n")
  cat(title_)
  
  cutoff_dates_df_temp <- specimen_avg_data_df %>%
    distinct(Date, Phase) %>%
    arrange(Date) %>%
    mutate(Phase = as.character(Phase))
  
  # segment into groups
  cutoff_dates_df_temp$sub_g <- c(0,cumsum(as.numeric(with(cutoff_dates_df_temp, Phase[1:(length(Phase)-1)] != Phase[2:length(Phase)])))); 
  
  cutoff_dates_df <- cutoff_dates_df_temp %>% 
    group_by(sub_g, Phase) %>% 
    summarize(first = first(Date), last = last(Date), .groups = "keep") %>%
    mutate_all(.funs = as.character) %>%
    mutate(date_range = map2_chr(first, last, function(f,l){
      res <- str_c(c(f, l), collapse = " to ")
      return(res)
    })) %>%
    mutate(`Number of Days` = as.integer(as.Date(last) - as.Date(first)) + 1) %>%
    ungroup() %>%
    select(-sub_g) 
  
  
  experiment_timeline_fn <- file.path(meta_data_dir, "experiment-timeline.pdf")
  kable(cutoff_dates_df, caption = "Dates", align = "c") %>%
    kable_styling("striped") %>%
    save_kable(experiment_timeline_fn)
  
  group_wise_bp_and_sem <- specimen_avg_data_df %>%
    group_by(`Specimen Name`, group, Phase) %>% 
    summarize(grp_systolic_mean = mean(specimen_mean_systolic),
              grp_systolic_sd = sd(specimen_mean_systolic),
              grp_systolic_sem = calculate_sem(specimen_mean_systolic),
              grp_hr_mean = mean(specimen_hr_mean), 
              grp_hr_sem = calculate_sem(specimen_hr_mean), .groups = "keep") 
  
  phase_wise_bp_and_sem <- specimen_avg_data_df %>%
    group_by(group, Phase) %>% 
    summarize(phase_systolic_mean = mean(specimen_mean_systolic),
              phase_systolic_sd = sd(specimen_mean_systolic),
              phase_systolic_sem = calculate_sem(specimen_mean_systolic),
              phase_hr_mean = mean(specimen_hr_mean), 
              phase_hr_sem = calculate_sem(specimen_hr_mean), .groups = "keep") 
  
  # this is the main df now!!
  three_day_df <- specimen_avg_data_df %>%
    group_by(`Specimen Name`, Phase) %>%
    mutate(n = n():1) %>%
    filter(n %in% c(1:3))
  
  three_day_group_df <- three_day_df %>%
    group_by(`Specimen Name`, group, Phase) %>% 
    summarize(mean_group = mean(specimen_mean_systolic),
              sd_group = sd(specimen_mean_systolic),
              sem_group = calculate_sem(specimen_mean_systolic), .groups = "keep") 
  
  three_day_phase_df <- three_day_df %>%
    group_by(group, Phase) %>% 
    summarize(mean_phase = mean(specimen_mean_systolic),
              sd_phase = sd(specimen_mean_systolic),
              sem_phase = calculate_sem(specimen_mean_systolic), .groups = "keep") 
  
  bp_avg_g <- ggplot(data = three_day_phase_df, 
                     aes(fill = `group`, x = group, y = mean_phase)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.5) +
    geom_point(three_day_group_df, 
               mapping = aes(x = group, y = mean_group, shape = `Specimen Name`), 
               show.legend = TRUE, inherit.aes = FALSE) + 
    scale_shape_manual(values = shape_ids) +
    # text by group and phase
    geom_label(data = three_day_phase_df,
               aes(x = `group`, y = max(mean_phase) + 35,
                   label = round(mean_phase,1)),
               hjust = 0.5, color = "black", size = 3,
               inherit.aes = FALSE, show.legend = F) +
    
    geom_errorbar(data = three_day_phase_df,
                  aes(x = group, ymin=`mean_phase`-sem_phase, 
                      ymax=`mean_phase`+sem_phase), width=.2,
                  position=position_dodge(0.05), show.legend = F, inherit.aes = FALSE) +
    # change fill colors
    scale_fill_manual(name = "group", values = named_color_vec) +
    my_theme +
    # scale_y_continuous(breaks=seq(40, 175, 10)) +
    theme(axis.text.x = element_text(angle = 60, hjust=1),
          axis.text.y = element_text(hjust = 0.5)) +
    ylab("Average Systolic BP") +
    ggtitle("Average SBP across Phases") +
    labs(caption = "Averages calculated using the last-most 2-3 days of each phase") + 
    facet_grid(cols = vars(Phase), scales = "free_x") 
  
  
  hr_avg_g <- ggplot(data = phase_wise_bp_and_sem, 
                     aes(fill = `group`, x = group, y = phase_hr_mean)) +
    geom_bar(position = "dodge", stat = "identity", width = 0.5) +
    
    geom_point(group_wise_bp_and_sem, 
               mapping = aes(x = group, y = grp_hr_mean, shape = `Specimen Name`), 
               show.legend = TRUE, inherit.aes = FALSE) + 
    scale_shape_manual(values = shape_ids) +
    
    # text by group and phase
    geom_label(data = phase_wise_bp_and_sem,
               aes(x = `group`, y = max(group_wise_bp_and_sem$grp_hr_mean) + 35,
                   label = round(phase_hr_mean,1)),
               hjust = 0.5, color = "black", size = 3,
               inherit.aes = FALSE, show.legend = F) +
    
    geom_errorbar(data = phase_wise_bp_and_sem,
                  aes(x = group, ymin=`phase_hr_mean`-phase_hr_sem,
                      ymax=`phase_hr_mean`+phase_hr_sem), width=.2,
                  position=position_dodge(0.05), show.legend = F, inherit.aes = FALSE) +
    # change fill colors
    scale_fill_manual(name = "group", values = named_color_vec) +
    my_theme +
    # scale_y_continuous(breaks=seq(90, 220, 10)) +
    theme(axis.text.x = element_text(angle = 60, hjust=1),
          axis.text.y = element_text(hjust = 0.5)) +
    ylab("Average Heart Rate") +
    ggtitle("Average heart rate across phases") +
    facet_grid(cols = vars(Phase), scales = "free_x") 
  
  avg_lst_g <- ggarrange(bp_avg_g, hr_avg_g, nrow = 2, common.legend = TRUE)
  
  avg_res_fn <- file.path(results_dir, "averages.pdf")
  ggsave(avg_lst_g, file = avg_res_fn, width = 12, height = 15)
  
  ## DIFFERENCE FROM BASELINE
  
  # vehicle_avg_3day <- three_day_df %>%
  #   filter(Phase == "baseline") %>%
  #   group_by(`Specimen Name`, group, color, Phase) %>%
  #   summarize(specimen_mean_systolic = mean(specimen_mean_systolic),
  #             specimen_systolic_sem  = calculate_sem(specimen_mean_systolic), 
  #             specimen_hr_mean = mean(specimen_hr_mean),
  #             specimen_hr_sem  = calculate_sem(specimen_hr_mean), 
  #             .groups = "keep") %>%
  #   mutate(Date = as.factor(cutoff_dates_df %>% filter(Phase == "baseline") %>% pluck("date_range")))
  
  # all_sumarized_diff <- three_day_df %>% 
  #   filter(Phase != "training") %>%
  #   group_by(Phase, group) %>%
  #   summarize(mean_systolic = mean(specimen_mean_systolic),
  #             systolic_sem  = calculate_sem(specimen_mean_systolic), 
  #             hr_mean = mean(specimen_hr_mean),
  #             hr_sem  = calculate_sem(specimen_hr_mean), 
  #             .groups = "keep") %>% 
  #   group_by(group) %>% 
  #   arrange(group) %>%
  #   mutate(sys_mean_diff = c(0, diff(mean_systolic)),
  #          hr_mean_diff = c(0, diff(hr_mean))) %>%
  #   left_join(cutoff_dates_df, by = "Phase")
  # 
  
  all_sumarized_diff <- three_day_df %>% 
    filter(Phase != "training") %>%
    arrange(group) %>%
    mutate(sys_mean_diff = c(0, diff(specimen_mean_systolic)),
           hr_mean_diff = c(0, diff(specimen_hr_mean))) %>%
    left_join(cutoff_dates_df, by = "Phase") %>%
    mutate(unique_id = make.unique(as.character(`Specimen Name`))) %>% 
    ungroup() %>%
    mutate(Phase = factor(Phase, levels = fct_phases))
  
  bt_sumarized_diff <- three_day_df %>% 
    filter(Phase != "training", Phase != "HSD + treatment") %>%
    arrange(group) %>%
    mutate(sys_mean_diff = c(0, diff(specimen_mean_systolic)),
           hr_mean_diff = c(0, diff(specimen_hr_mean))) %>%
    left_join(cutoff_dates_df, by = "Phase") %>%
    mutate(unique_id = make.unique(as.character(`Specimen Name`))) %>% 
    ungroup() %>%
    mutate(Phase = factor(Phase, levels = fct_phases))
  
  thsdt_sumarized_diff <- three_day_df %>% 
    filter(Phase != "training", Phase != "baseline") %>%
    arrange(group) %>%
    mutate(sys_mean_diff = c(0, diff(specimen_mean_systolic)),
           hr_mean_diff = c(0, diff(specimen_hr_mean))) %>%
    left_join(cutoff_dates_df, by = "Phase") %>%
    mutate(unique_id = make.unique(as.character(`Specimen Name`))) %>% 
    ungroup() %>%
    mutate(Phase = factor(Phase, levels = fct_phases))
  
  bhsdt_sumarized_diff <- three_day_df %>% 
    filter(Phase != "training", Phase != "treatment") %>%
    arrange(group) %>%
    mutate(sys_mean_diff = c(0, diff(specimen_mean_systolic)),
           hr_mean_diff = c(0, diff(specimen_hr_mean))) %>%
    left_join(cutoff_dates_df, by = "Phase") %>%
    mutate(unique_id = make.unique(as.character(`Specimen Name`))) %>% 
    ungroup() %>%
    mutate(Phase = factor(Phase, levels = fct_phases))
  
  
  my_comparisons <- list( c("baseline", "treatment"), c("baseline", "HSD + treatment"), c("treatment", "HSD + treatment") )
  
  # res.aov <- anova_test(data = all_sumarized_diff, 
  #                       dv = sys_mean_diff, 
  #                       wid = `unique_id`, within = Phase) %>% 
  #   get_anova_table() %>% 
  #   adjust_pvalue(method = "bonferroni"); res.aov
  # 
# 
#   p_vals <- all_sumarized_diff %>% pairwise_t_test(
#     sys_mean_diff ~ Phase, paired = FALSE,
#     p.adjust.method = "bonferroni"
#   ) %>% add_y_position() %>%
#    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  raw_sys1_g <- ggline(bt_sumarized_diff, x = "Phase", y = "specimen_mean_systolic", 
                       add = c("mean_se", "jitter"),
                       color = "group", palette = "jco", ggtheme = theme_bw(),
                       facet.by = "group") + 
    # stat_pvalue_manual(p_vals, label = "p.signif")  +
    ylab("Mean systolic BP (mmHg)") + 
    xlab("Phase") +
    ggtitle("Raw systolic BP - baseline to treatment") + 
    stat_compare_means(method = 't.test',
                       comparisons = my_comparisons[1], 
                       label = "p.format", paired = TRUE
    )
  
  delta_sys1_g <- ggline(bt_sumarized_diff, x = "Phase", y = "sys_mean_diff", 
                         add = c("mean_se", "jitter"),
                         color = "group", palette = "jco", ggtheme = theme_bw(),
                         facet.by = "group") +
    ylab("Mean systolic BP change\nfrom previous phase (mmHg)") + 
    xlab("Phase") +
    ggtitle("Change in systolic BP - baseline to treatment") + 
    stat_compare_means(method = 't.test',
                       comparisons = my_comparisons[1], 
                       label = "p.format", paired = TRUE
    )
  
  raw_sys2_g <- ggline(bhsdt_sumarized_diff, x = "Phase", y = "specimen_mean_systolic", 
                       add = c("mean_se", "jitter"),
                       color = "group", palette = "jco", ggtheme = theme_bw(),
                       facet.by = "group") + 
    # stat_pvalue_manual(p_vals, label = "p.signif")  +
    ylab("Mean systolic BP (mmHg)") + 
    xlab("Phase") +
    ggtitle("Raw systolic BP - baseline to HSD + treatment") + 
    stat_compare_means(method = 't.test',
                       comparisons = my_comparisons[2], 
                       label = "p.format", paired = TRUE
    )
  
  delta_sys2_g <- ggline(bhsdt_sumarized_diff, x = "Phase", y = "sys_mean_diff", 
                         add = c("mean_se", "jitter"),
                         color = "group", palette = "jco", ggtheme = theme_bw(),
                         facet.by = "group") +
    ylab("Mean systolic BP change\nfrom previous phase (mmHg)") + 
    xlab("Phase") +
    ggtitle("Change in systolic BP - baseline to HSD + treatment") + 
    stat_compare_means(method = 't.test',
                       comparisons = my_comparisons[2], 
                       label = "p.format", paired = TRUE
    )
  
  raw_sys3_g <- ggline(thsdt_sumarized_diff, x = "Phase", y = "specimen_mean_systolic", 
                       add = c("mean_se", "jitter"),
                       color = "group", palette = "jco", ggtheme = theme_bw(),
                       facet.by = "group") + 
    # stat_pvalue_manual(p_vals, label = "p.signif")  +
    ylab("Mean systolic BP (mmHg)") + 
    xlab("Phase") +
    ggtitle("Raw systolic BP - treatment to HSD + treatment") + 
    stat_compare_means(method = 't.test',
                       comparisons = my_comparisons[3], 
                       label = "p.format", paired = TRUE
    )
  
  delta_sys3_g <- ggline(thsdt_sumarized_diff, x = "Phase", y = "sys_mean_diff", 
                         add = c("mean_se", "jitter"),
                         color = "group", palette = "jco", ggtheme = theme_bw(),
                         facet.by = "group") +
    ylab("Mean systolic BP change\nfrom previous phase (mmHg)") + 
    xlab("Phase") +
    ggtitle("Change in systolic BP - treatment to HSD + treatment") + 
    stat_compare_means(method = 't.test',
                       comparisons = my_comparisons[3], 
                       label = "p.format", paired = TRUE
    )
  
  
  raw_sys4_g <- ggline(all_sumarized_diff, x = "Phase", y = "specimen_mean_systolic", 
         add = c("mean_se", "jitter"),
         color = "group", palette = "jco", ggtheme = theme_bw(),
         facet.by = "group") + 
    # stat_pvalue_manual(p_vals, label = "p.signif")  +
    ylab("Mean systolic BP (mmHg)") + 
    xlab("Phase") +
    ggtitle("Raw systolic BP by phase") + 
    stat_compare_means(method = 't.test',
                       comparisons = my_comparisons, 
                       label = "p.format", paired = TRUE
    )
  
  delta_sys4_g <- ggline(all_sumarized_diff, x = "Phase", y = "sys_mean_diff", 
         add = c("mean_se", "jitter"),
         color = "group", palette = "jco", ggtheme = theme_bw(),
         facet.by = "group") +
    ylab("Mean systolic BP change\nfrom previous phase (mmHg)") + 
    xlab("Phase") +
    ggtitle("Change in systolic BP by phase") + 
    stat_compare_means(method = 't.test',
      comparisons = my_comparisons, 
      label = "p.format", paired = TRUE
    )


  sys_bp_g_lst <- ggarrange(raw_sys1_g, delta_sys1_g, 
                            raw_sys2_g, delta_sys2_g, 
                            raw_sys3_g, delta_sys3_g, 
                            raw_sys4_g, delta_sys4_g, 
                            nrow = 4, ncol = 2, common.legend = TRUE)
  
  sys_bp_g_lst_fn <- file.path(results_dir, "bp_change.pdf")
  ggsave(sys_bp_g_lst, file = sys_bp_g_lst_fn, width = 12, height = 18)

  
  # time_series_avg_diff_by_mouse_temp1 <- specimen_avg_data_df %>%
  #   filter(!(Phase %in% c("baseline", "training", "vehicle"))) %>%
  #   mutate(Date = as.factor(Date))
  # 
  # diffs_df <- three_day_phase_df %>% 
  #   filter(Phase != "training") %>%
  #   group_by(group) %>% 
  #   mutate(mean_diff = c(0, diff(mean_phase)),
  #          sd_diff = c(0, diff(sd_phase)),
  #          sem_diff = c(0, diff(sem_phase)))
  
  # if (nrow(time_series_avg_diff_by_mouse_temp1) != 0) {
  #   new_levels <- c(levels(vehicle_avg_3day$Date[1]), levels(time_series_avg_diff_by_mouse_temp1$Date))
  #   
  #   time_series_diff <- bind_rows(vehicle_avg_3day, time_series_avg_diff_by_mouse_temp1) %>%
  #     mutate(Date = factor(Date, levels = new_levels)) %>%
  #     group_by(`Specimen Name`) %>%
  #     arrange(`Specimen Name`, Date) %>%
  #     mutate(systolic_diff = specimen_mean_systolic - specimen_mean_systolic[1L],
  #            hr_diff = specimen_hr_mean - specimen_hr_mean[1L]) %>%
  #     mutate(systolic_sem_diff = calculate_sem(systolic_diff),
  #            hr_sem_diff = calculate_sem(hr_diff)) %>%
  #     arrange(Phase) %>%
  #     dplyr::select(-c(specimen_mean_systolic, specimen_systolic_sem, specimen_hr_mean, specimen_hr_sem)) %>%
  #     mutate(Date = as.character(Date))
  #   
  #   bp_diff_g <- ggplot(time_series_diff,
  #                       aes(x = `Date`,
  #                           y = systolic_diff, group = `Specimen Name`, 
  #                           color = `group`, fill = `group`)) +
  #     geom_point(show.legend = T) +
  #     geom_line() +
  #     geom_smooth(aes(x = `Date`,
  #                     y = systolic_diff, group = `group`, 
  #                     color = `group`, fill = `group`),
  #                 se = TRUE, inherit.aes = FALSE, stat="smooth", method = "lm",
  #                 size = 1.3,
  #                 linetype ="dashed",
  #                 alpha = 0.3) + 
  #     
  #     # geom_bar(position = "dodge", stat = "identity", width = 0.5) + 
  #     my_theme +
  #     geom_errorbar(aes(ymin = systolic_diff - systolic_sem_diff,
  #                       ymax = systolic_diff + systolic_sem_diff), width=.2,
  #                   position=position_dodge(0.05)) +
  #     theme(axis.text.x = element_text(angle = 60, hjust=1),
  #           axis.text.y = element_text(hjust = 0.5)) +
  #     labs(title = "BP difference from baseline") +
  #     ylab("Systolic blood pressure (mmHg)") +
  #     scale_color_manual(name = "group", values = named_color_vec) +
  #     scale_fill_manual(name = "group", values = named_color_vec) ; bp_diff_g
  #   
  #   hr_diff_g <- ggplot(time_series_diff,
  #                       aes(x = `Date`,
  #                           y = hr_diff, group = `Specimen Name`, 
  #                           color = `group`, fill = `group`)) +
  #     geom_point(show.legend = T) +
  #     geom_line() +
  #     geom_smooth(aes(x = `Date`,
  #                     y = hr_diff, group = `group`, 
  #                     color = `group`, fill = `group`),
  #                 se = TRUE, inherit.aes = FALSE, stat="smooth", method = "lm",
  #                 size = 1.3,
  #                 linetype ="dashed",
  #                 alpha = 0.3) + 
  #     # geom_bar(position = "dodge", stat = "identity", width = 0.5) + 
  #     my_theme +
  #     geom_errorbar(aes(ymin = hr_diff - hr_sem_diff,
  #                       ymax = hr_diff + hr_sem_diff), width=.2,
  #                   position=position_dodge(0.05)) +
  #     theme(axis.text.x = element_text(angle = 60, hjust=1),
  #           axis.text.y = element_text(hjust = 0.5)) +
  #     labs(title = "Heart rate difference from baseline") +
  #     ylab("Heart rate (bpm)") +
  #     scale_color_manual(name = "group", values = named_color_vec) +
  #     scale_fill_manual(name = "group", values = named_color_vec) ; hr_diff_g
  #   
  #   
  #   avg_time_series_diff <- time_series_diff %>% 
  #     group_by(Phase, group) %>%
  #     summarize(grp_systolic_diff_mean = mean(systolic_diff, na.rm =TRUE), 
  #               # grp_systolic_diff_sem = calculate_sem(grp_systolic_diff_mean),
  #               grp_hr_diff_mean = mean(hr_diff, na.rm =TRUE),
  #               # grp_hr_diff_sem = calculate_sem(grp_hr_diff_mean), 
  #               .groups = "keep")
  #   
  #   
  #   by_spec <- time_series_diff %>% 
  #     group_by(`Specimen Name`,group, Phase) %>% 
  #     summarize(m = mean(systolic_diff),
  #               sem = calculate_sem(systolic_diff))
  #   
  #   bp_diff_avg_g <- ggplot(avg_time_series_diff,
  #                           aes(x = group,
  #                               y = grp_systolic_diff_mean, group = `Phase`, 
  #                               color = `Phase`, fill = `group`)) +
  # 
  #     geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  #     
  #     geom_point(by_spec,
  #                mapping = aes(x = group,
  #                              y = m, group = `Phase`), 
  #                position = position_dodge(width = 0.5), color = "black", shape = 21, show.legend = T) +
  #     my_theme +
  # 
  #     theme(axis.text.x = element_text(angle = 60, hjust=1),
  #           axis.text.y = element_text(hjust = 0.5)) +
  #     labs(title = "Avg BP difference from baseline") +
  #     ylab("Systolic blood pressure difference\nfrom baseline (mmHg)") +
  #     facet_grid(cols = vars(Phase),  scales = "free_x") +
  #   scale_color_manual(name = "group", values = named_color_vec) +
  #   scale_fill_manual(name = "group", values = named_color_vec) ; bp_diff_avg_g
  # 
  #   
  #   diff_lst_g <- ggarrange(bp_diff_avg_g, ncol = 1,
  #                           ggarrange(bp_diff_g, hr_diff_g, ncol = 2), 
  #                           nrow = 2, heights = c(1.5, 1.5, 1.5), common.legend = TRUE); diff_lst_g
  #   
  # } else {
  #   diff_lst_g <- ggplot()
  # }
  
  # diff_res_fn <- file.path(results_dir, "diff.pdf")
  # ggsave(diff_lst_g, file = diff_res_fn, width = 14, height = 16)
  
  # cutoff dates calculated early on
  names_chr <- cutoff_dates_df %>% filter(Phase != "training") %>% pluck("Phase")
  ranges_chr <- cutoff_dates_df  %>% filter(Phase != "training") %>% pluck("date_range")
  formatted_ranges <- str_c(names_chr, ranges_chr, sep = ": ", collapse = "\n")
  
  summary_boxplots_g <- ggplot(final_filtered_data  %>% filter(Phase != "training"), 
                               aes(x = `Specimen Name`, y = `Systolic`, group = `Specimen Name`, fill = `group`)) +
    geom_boxplot(position = "dodge", alpha=0.8) +
    stat_summary(fun.y="mean", color = "cyan", show.legend = F) +
    scale_y_continuous(breaks=seq(70,250,10)) +
    # scale_fill_jco() +
    scale_fill_manual(name = "group", values = named_color_vec) +
    labs(title = "Boxplots of Mice Systolic BP by Tail-cuff by Phase",
         subtitle = formatted_ranges) +
    ylab("Systolic blood pressure (mmHg)") +
    my_theme +
    theme(axis.title.y = element_text(hjust = 0.5),
          axis.text.y = element_text(hjust = 0.5)) +
    geom_label(data = group_wise_bp_and_sem  %>% filter(Phase != "training"),
               aes(label = round(grp_systolic_mean,1),
                   x = `Specimen Name`, hjust = 0.5, y = 200),
               inherit.aes = FALSE) +
    facet_grid(cols = vars(Phase))
  
  summary_res_fn <- file.path(results_dir, "summary.pdf")
  ggsave(summary_boxplots_g, file = summary_res_fn, width = 15, height = 10)
}


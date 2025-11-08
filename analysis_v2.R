library(archive)
library(jsonlite)
library(cli)
library(tidyverse)
library(future)
library(future.apply)
library(progressr)
library(furrr)
library(magrittr)
library(slider)
library(readxl)

rm(list=ls())
setwd("/Users/drew.cooper/AID_PSQI")

source("cleanup_data.R")
source("gv_metrics.R")
source("gateway_linkages.R")
source("psqi.R")

redcap_data_file <- "/Users/drew.cooper/REDCap/OPEN_DATA_2023-07-18_1202.csv"
gateway_linkages_file <- "/Users/drew.cooper/OPENonOH/Participants_FULL_BIGOPEN+OPENLight_2021.07.13.xlsx"
bg_readings_file <- "/Users/drew.cooper/OPENonOH/BgReadings.xlsx" #same file as outputExcel.xlsx from main.R
labels_file <- "/Users/drew.cooper/REDCap/psqi_5j_labels.xlsx" #this is updated from the scored codex

labels <- read_excel(labels_file)

gateway_links <- load_gateway_linkages(gateway_linkages_file) %>%
  filter(!is.null(project_member_id) & !is.null(survey_record_id))

redcap <- read.csv(redcap_data_file) %>%
  filter(psqi_complete == 2) %>%
  filter(fear_of_hypo_complete == 2) %>%
  filter(enrollment_type %in% c(0, 1)) %>%
  left_join(gateway_links, by = c("record_id" = "survey_record_id")) %>%
  mutate(
    enrollment_type = factor(
      x = enrollment_type,
      levels = c(0, 1),
      labels = c("Adult using DIYAPS", "Adult not using DIYAPS")
    ),
    type_of_diabetes = if_else(
      condition = enrollment_type == "Adult using DIYAPS",
      true = type_of_diabetes,
      false = diynot_diab_type
    ),
    type_of_diabetes = factor(
      x = type_of_diabetes,
      levels = c(1, 2, 4, 5, 7777),
      labels = c("Type 1", "Type 2", "LADA", "MODY", "Other")
    ),
    other_type_of_diabetes = if_else(
      condition = enrollment_type == "Adult using DIYAPS",
      true = other_types_of_diabetes,
      false = diynot_diab_type_ft
    ),
    gender = factor(
      gender,
      levels = c(1, 2),
      labels = c("Female", "Male")
    ),
    ethnicity = factor(
      ethnicity,
      levels = c(2, 3, 5, 7, 8, 77777, 99999),
      labels = c(
        "Asian",
        "Arab",
        "Hispanic/Latino",
        "White",
        "Mixed / Multiple ethnic groups",
        "Other",
        "I'd rather not say"
      )
    ),
    age = interval(
      make_date(year = year_of_birth, month = month_of_birth, day = 1),
      as_datetime(
        x = baseline_demographic_adults_diyaps_timestamp,
        tz = "Europe/Berlin"
      )
    ) / years(1),
    a1c_users = if_else(
      condition = hba1c_unit == 1,
      true = most_recent_hba1c,
      false = (most_recent_hba1c / 10.929) + 2.15
    ),
    a1c_non_users = if_else(
      condition = diynot_dia_management_hba1c == 1,
      true = diynot_dia_management_hba1c_dcct,
      false = (diynot_dia_management_hba1c_ifcc / 10.929) + 2.15
    ),
    a1c = if_else(
      condition = enrollment_type == "Adult using DIYAPS",
      true = a1c_users,
      false = a1c_non_users
    )
  ) %>%
  rowwise() %>%
  mutate(
    psqi_1 = ifelse(
      test = project_member_id %in% names(bedtime_override),
      yes = bedtime_override[[project_member_id]],
      no = psqi_1
    )
  ) %>%
  ungroup()

bg_readings <- read_excel(bg_readings_file)

psqi <- calculate_psqi_scores(redcap)

gv_metrics <- bg_readings %>%
  group_by(project_member_id) %>%
  calculate_gv_metrics()

study_data <- redcap %>%
  mutate(
    psqi_component_1 = psqi$component_1,
    psqi_component_2 = psqi$component_2,
    psqi_component_3 = psqi$component_3,
    psqi_component_4 = psqi$component_4,
    psqi_component_5 = psqi$component_5,
    psqi_component_6 = psqi$component_6,
    psqi_component_7 = psqi$component_7,
    psqi_global_score = psqi$global_score,
    hfs_b_6 = fear_of_hypo_1 - 1,
    hfs_b_8 = fear_of_hypo_2 - 1,
    hfs_b_11 = fear_of_hypo_3 - 1,
    hfs_b_13 = fear_of_hypo_4 - 1,
    hfs_b_14 = fear_of_hypo_5 - 1,
    hfs_w_1 = hypo_worry_1 - 1,
    hfs_w_3 = hypo_worry_2 - 1,
    hfs_w_9 = hypo_worry_3 - 1,
    hfs_w_16 = hypo_worry_4 - 1,
    hfs_w_17 = hypo_worry_5 - 1,
    hfs_w_18 = hypo_worry_6 - 1,
    hfs_b = hfs_b_6 +
      hfs_b_8 +
      hfs_b_11 +
      hfs_b_13 +
      hfs_b_14,
    hfs_w = hfs_w_1 +
      hfs_w_3 +
      hfs_w_9 +
      hfs_w_16 +
      hfs_w_17 +
      hfs_w_18,
    hfs = hfs_b + hfs_w
  ) %>%
  select(
    record_id,
    project_member_id,
    enrollment_type,
    type_of_diabetes,
    other_type_of_diabetes,
    gender,
    ethnicity,
    age,
    a1c,
    psqi_component_1,
    psqi_component_2,
    psqi_component_3,
    psqi_component_4,
    psqi_component_5,
    psqi_component_6,
    psqi_component_7,
    psqi_global_score,
    psqi_5other,
    hfs_b_6, # Limited my out of town travel
    hfs_b_8, # Avoided visiting friends
    hfs_b_11, # Made sure there were other people around.
    hfs_b_13, # Kept my blood sugar higher than usual in social situations
    hfs_b_14, # Kept my blood sugar higher than usual when doing important tasks.
    hfs_w_1, # Not recognizing/realizing I was having low blood sugar.
    hfs_w_3, # Passing out in public.
    hfs_w_9, # Having a hypoglycaemic episode while driving.
    hfs_w_16, # Low blood sugar interfering with important things I was doing.
    hfs_w_17, # Becoming hypoglycaemic during sleep.
    hfs_w_18, # Getting emotionally upset and difficult to deal with.
    hfs_b,
    hfs_w,
    hfs
  ) %>%
  filter(!is.na(psqi_global_score) & !is.na(hfs)) %>%
  left_join(gv_metrics, "project_member_id") %>%
  left_join(labels, "record_id")

###——————————————————————————————————————————————————————————————————————————###

# Some initial cleaning/renaming/restructuring to clean things up...
study_data <- study_data[ !duplicated(study_data$record_id), ] # remove duplicates

study_data <- study_data %>%
  mutate(
    diabetes_group = case_when(
      type_of_diabetes %in% c("Type 1", "LADA") ~ "Type 1 or LADA",
      type_of_diabetes %in% c("Type 2", "MODY") ~ "Type 2 or MODY",
      TRUE ~ NA_character_  # catches any unexpected values
    ),
    diabetes_group = factor(diabetes_group, levels = c("Type 1 or LADA", "Type 2 or MODY")),
    age = as.numeric(age)
  ) %>%
  relocate(diabetes_group, .after = type_of_diabetes)

levels(study_data$enrollment_type)[levels(study_data$enrollment_type) == "Adult using DIYAPS"] <- "User"
levels(study_data$enrollment_type)[levels(study_data$enrollment_type) == "Adult not using DIYAPS"] <- "Non-user"

levels(study_data$gender)[levels(study_data$gender) == "Female"] <- "Women"
levels(study_data$gender)[levels(study_data$gender) == "Male"] <- "Men"

study_data <- study_data %>%
  rename(A1c = a1c,
         TIR = percent_in_range,
         TING = percent_in_tight_range,
         TAR = percent_above_range,
         TAR1 = percent_above_range_level_1,
         TAR2 = percent_above_range_level_2,
         TBR = percent_below_range,
         TBR1 = percent_below_range_level_1,
         TBR2 = percent_below_range_level_2
  )

hist(study_data$age) # replace variable with whichever you want to visualise distribution; basic normality test

###—TABLE 1——————————————————————————————————————————————————————————————————###

library(dplyr)
library(tidyr)
library(openxlsx)

# Define format "fmt" function(s) to calculate n (%), mean ± sd, and median [IQR]
fmt_n_percent <- function(n, N, digits = 1) {
  pct <- 100 * n / N
  sprintf(
    paste0("%d (%.", digits, "f%%)"),
    n, pct
  )
}

fmt_mean_sd <- function(x, digits = 1) {
  sprintf(
    paste0("%.", digits, "f ± %.", digits, "f"),
    mean(x, na.rm=TRUE),
    sd(x, na.rm=TRUE)
  )
}

fmt_median_iqr <- function(x, digits = 1) {
  sprintf(
    paste0("%.", digits, "f [%.", digits, "f–%.", digits, "f]"),
    median(x, na.rm=TRUE),
    quantile(x, .25, na.rm=TRUE),
    quantile(x, .75, na.rm=TRUE)
  )
}

summary_tbl <- study_data # reset table if need be during troubleshooting
summary_tbl <- study_data %>%
  # make long on all grouping variables
  pivot_longer(
    cols = c(enrollment_type, type_of_diabetes, gender, ethnicity),
    names_to = "group_name",
    values_to = "group_value"
  ) %>%
  mutate(group = paste0(group_value)) %>%
  group_by(group_name, group) %>%
  summarise(
    n = fmt_n_percent(n(), nrow(study_data)),
    Age = fmt_mean_sd(age),
    across(
      c(A1c, TIR, TING, TAR, TAR1, TAR2, TBR, TBR1, TBR2),
      fmt_median_iqr), 
    .groups = "drop"
  ) %>%
  arrange(group)

overall_row <- study_data %>%
  summarise(
    group_name = "0_overall", 
    group = "Overall",
    n = fmt_n_percent(n(), nrow(study_data)),
    Age = fmt_mean_sd(age),
    across(
      c(A1c, TIR, TING, TAR, TAR1, TAR2, TBR, TBR1, TBR2),
      fmt_median_iqr)
  )
summary_tbl <- bind_rows(overall_row, summary_tbl)

summary_tbl <- summary_tbl %>% # factorising the various group_names and groups for ordering purposes...
  mutate(
    group_name = factor(group_name,
                        levels = c("0_overall", "enrollment_type", "type_of_diabetes", "gender", "ethnicity")),
    group = case_when(
      group_name == "enrollment_type" & group == "User" ~ factor(group, levels = c("User", "Non-user")),
      group_name == "gender" & group == "Women" ~ factor(group, levels = c("Women", "Men")),
      group_name == "type_of_diabetes" & group == "Type 1" ~ factor(group, levels = c("Type 1", "LADA", "Type 2", "MODY")),
      group_name == "ethnicity" & group == "White" ~ factor(group, levels = c("White", "Mixed / Multiple ethnic groups", "Hispanic/Latino", "Arab", "Asian", "Other", "I'd rather not say")),
      TRUE ~ factor(group)
    )
  )
summary_tbl <- summary_tbl %>%
  arrange(group_name, group)

summary_tbl
write.xlsx(summary_tbl, "/Users/drew.cooper/REDCap/table1.xlsx")

###——————————————————————————————————————————————————————————————————————————###

# Creating the generalised data frame skeleton for Wilcoxon testing
compare_groups <- function(data, group_var, outcome_vars) {
  results <- lapply(outcome_vars, function(var) {
    df <- data %>% select(all_of(c(group_var, var))) %>% na.omit()
    
    # Force numeric conversion
    df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
    
    # Skip if variable not numeric or has no variance
    if (!is.numeric(df[[var]]) || length(unique(df[[var]])) < 2) {
      return(data.frame(
        variable = var,
        test_used = "Wilcoxon rank-sum",
        statistic = NA,
        group1 = NA,
        group2 = NA,
        median_group1 = NA,
        iqr_lower_group1 = NA,
        iqr_upper_group1 = NA,
        median_group2 = NA,
        iqr_lower_group2 = NA,
        iqr_upper_group2 = NA,
        p_value = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    # Run Wilcoxon test
    test <- wilcox.test(df[[var]] ~ df[[group_var]])
    
    # Extract group names
    group_names <- unique(df[[group_var]])
    
    # Compute medians and IQRs
    median1 <- median(df[[var]][df[[group_var]] == group_names[1]], na.rm = TRUE)
    q1_1 <- quantile(df[[var]][df[[group_var]] == group_names[1]], 0.25, na.rm = TRUE)
    q3_1 <- quantile(df[[var]][df[[group_var]] == group_names[1]], 0.75, na.rm = TRUE)
    
    median2 <- median(df[[var]][df[[group_var]] == group_names[2]], na.rm = TRUE)
    q1_2 <- quantile(df[[var]][df[[group_var]] == group_names[2]], 0.25, na.rm = TRUE)
    q3_2 <- quantile(df[[var]][df[[group_var]] == group_names[2]], 0.75, na.rm = TRUE)
    
    data.frame(
      variable = var,
      group1 = group_names[1],
      group2 = group_names[2],
      median_group1 = median1,
      iqr_lower_group1 = q1_1,
      iqr_upper_group1 = q3_1,
      median_group2 = median2,
      iqr_lower_group2 = q1_2,
      iqr_upper_group2 = q3_2,
      test_used = "Wilcoxon rank-sum",
      statistic = unname(test$statistic),
      p_value = test$p.value,
      stringsAsFactors = FALSE
    )
  })
  
  results_df <- do.call(rbind, results)
  results_df$adj_p <- p.adjust(results_df$p_value, method = "holm")
  results_df
}

#Example usage
psqi_vars <- grep("^psqi_(?!.*other)", names(study_data), value = TRUE, perl = TRUE)
hfs_vars <- grep("^hfs", names(study_data), value = TRUE)
glycemic_vars <- c("a1c", "percent_in_range", "percent_in_tight_range",
                   "percent_above_range", "percent_above_range_level_1", "percent_above_range_level_2",
                   "percent_below_range", "percent_below_range_level_1", "percent_below_range_level_2",
                   "mean", "sd", "cv")

# PSQI Wilcoxon results (swap in "enrollment_type", "gender", or "diabetes_group")
psqi_results <- compare_groups(study_data, "diabetes", psqi_vars)
write.csv(psqi_results, "/Users/drew.cooper/REDCap/psqi_results_diabetes.csv", row.names = FALSE)

# HFS Wilcoxon results
hfs_results <- compare_groups(study_data, "diabetes", hfs_vars)
write.csv(hfs_results, "/Users/drew.cooper/REDCap/hfs_results_diabetes.csv", row.names = FALSE)

# Glycemic measures Wilcoxon results
glycemic_results <- compare_groups(study_data, "diabetes", glycemic_vars)
write.csv(glycemic_results, "/Users/drew.cooper/REDCap/glycemic_results_diabetes.csv", row.names = FALSE)

###——————————————————————————————————————————————————————————————————————————###

library(ggplot2)
library(dplyr)
library(tidyr)

long_df <- study_data %>%
  pivot_longer(cols = c(enrollment_type, gender, diabetes),
               names_to = "group_type",
               values_to = "group_value")

# Define custom palette
custom_colors <- c(
  # Enrollment type (users vs non-users) → shades of orange
  "User" = "#FFA500",       # orange
  "Non-user" = "#cc5c00",   # dark orange
  
  # Gender → pale pink and deep burgundy
  "Women" = "#FFC0CB",     # pale pink
  "Men" = "#b2334d",       # burgundy
  
  # Diabetes → light and dark periwinkles
  "Type 1 or LADA" = "#CCCCFF",  # light periwinkle
  "Type 2 or MODY" = "#6666CC"   # dark periwinkle
)

# swap in different y-values from long_df/study_data to assess different outcome measures
p <- ggplot(long_df %>% filter(!is.na(group_value)),
       aes(x = group_value, y = hfs, fill = group_value)) +
  geom_violin(trim = FALSE, color = "white") +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - qt(0.975, length(x)-1)*sd(x)/sqrt(length(x)),
               fun.max = function(x) mean(x) + qt(0.975, length(x)-1)*sd(x)/sqrt(length(x)),
               geom = "pointrange", color = "#dc267f", position = position_nudge(x = 0.15)) +
  facet_wrap(~group_type, scales = "free_x") +
  scale_fill_manual(values = custom_colors) +  # apply custom colors
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    strip.text = element_blank(),
    legend.position = "none"
  ) +
  labs(y = "HFS-II Score")

ggsave("/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/ISPAD 2025/hfs-median-iqr-violin-plot.png",
       plot = p, width = 20, height = 10, units = "cm", dpi = 300, bg = "transparent")

###——————————————————————————————————————————————————————————————————————————###

psqi_questions <- study_data %>%
  select(enrollment_type, psqi_component_1:psqi_component_7) %>%
  pivot_longer(
    cols = -enrollment_type,
    names_to = "question",
    values_to = "score",
  ) %>%
  group_by(enrollment_type, question) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    se = sd(score, na.rm = TRUE) /sqrt(n()),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = enrollment_type,
    values_from = c(mean, se)
  ) %>%
  mutate(
    diff = `mean_Adult not using DIYAPS` - `mean_Adult using DIYAPS`,
    pooled_se = sqrt(`se_Adult not using DIYAPS`^2 + `se_Adult using DIYAPS`^2),
    question_label = case_when(
      question == "psqi_component_1" ~ "Subjective Sleep Quality",
      question == "psqi_component_2" ~ "Sleep Latency",
      question == "psqi_component_3" ~ "Sleep Duration",
      question == "psqi_component_4" ~ "Sleep Efficiency",
      question == "psqi_component_5" ~ "Sleep Disturbance",
      question == "psqi_component_6" ~ "Use of Sleep Medication",
      question == "psqi_component_7" ~ "Daytime Dyfunction"
    )
  )

plot <- ggplot(psqi_questions, aes(y = reorder(question_label, diff))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(
    aes(xmin = diff - 1.96 * pooled_se,
        xmax = diff + 1.96 * pooled_se),
    height = 0.3,
    color = "black"
  ) +
  geom_point(aes(x = diff), size = 3, color = "black") +
  xlim(-0.1, 1.0) +
  #scale_x_discrete(-0.2, 1.0) +
  labs(
    x = "Mean PSQI Score Difference w/ 95% CIs (Non-Users vs. Users)",
    y = "",
    title = ""
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill = "transparent"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    legend.box = "horizontal"
  )

ggsave("psqi_differences.png", width = 25, height = 10, units = "cm", dpi = 300, bg = "transparent")

###——————————————————————————————————————————————————————————————————————————###

hfs_questions <- study_data %>%
  select(enrollment_type, hfs_b_6:hfs_w_18) %>%
  pivot_longer(
    cols = -enrollment_type,
    names_to = "question",
    values_to = "score"
  ) %>%
  group_by(enrollment_type, question) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    se = sd(score, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = enrollment_type,
    values_from = c(mean, se)
  ) %>%
  mutate(
    diff = `mean_Adult not using DIYAPS` - `mean_Adult using DIYAPS`,
    pooled_se = sqrt(`se_Adult not using DIYAPS`^2 + `se_Adult using DIYAPS`^2),
    subscale = if_else(str_detect(question, "hfs_b"), "Behavior", "Worry"),
    question_label = case_when(
      question == "hfs_w_1" ~ "Not recognizing low blood sugar (W1)",
      question == "hfs_w_3" ~ "Passing out in public (W3)",
      question == "hfs_w_9" ~ "Hypoglycemia while driving (W9)",
      question == "hfs_w_16" ~ "Low blood sugar interfering with tasks (W16)",
      question == "hfs_w_17" ~ "Hypoglycemia during sleep (W17)",
      question == "hfs_w_18" ~ "Getting emotionally upset (W18)",
      question == "hfs_b_6" ~ "Limited out of town travel (B6)",
      question == "hfs_b_8" ~ "Avoided visiting friends (B8)",
      question == "hfs_b_11" ~ "Made sure others were around (B11)",
      question == "hfs_b_13" ~ "Higher blood sugar in social situations (B13)",
      question == "hfs_b_14" ~ "Higher blood sugar for tasks (B14)"
    )
  )

plot <- ggplot(hfs_questions, aes(y = reorder(question_label, diff))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(
    aes(xmin = diff - 1.96 * pooled_se,
        xmax = diff + 1.96 * pooled_se,
        linetype = subscale),
    height = 0.3,
    color = "black"
  ) +
  geom_point(aes(x = diff, shape = subscale), size = 3, color = "black") +
  scale_shape_manual(values = c("Worry" = 16, "Behavior" = 1)) +
  scale_linetype_manual(values = c("Worry" = "solid", "Behavior" = "dashed")) +
  labs(
    x = "Mean HFS-II Score Difference w/ 95% CIs (Non-Users vs. Users)",
    y = "",
    title = "",
    shape = "Subscale",
    linetype = "Subscale"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.border = element_rect(fill = "transparent"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    legend.box = "horizontal"
  ) +
  guides(shape = guide_legend(title = "Subscale"),
         linetype = "none")

ggsave("hfs_differences.png", width = 25, height = 10, units = "cm", dpi = 300)

###——————————————————————————————————————————————————————————————————————————###

cgm_data_subanalysis <- study_data %>%
  filter(enrollment_type == "User" & days_of_data >= 25)

nrow(cgm_data_subanalysis)
by(cgm_data_subanalysis$age, cgm_data_subanalysis$enrollment_type, summary)
sd(cgm_data_subanalysis %>% pull(age), na.rm = TRUE)

by(cgm_data_subanalysis$gender, cgm_data_subanalysis$enrollment_type, summary)
by(cgm_data_subanalysis$type_of_diabetes, cgm_data_subanalysis$enrollment_type, summary)
by(cgm_data_subanalysis$ethnicity, cgm_data_subanalysis$enrollment_type, summary)

metrics_to_correlate <- c(
  "mean",
  "sd",
  "cv",
  "percent_in_range",
  "percent_in_tight_range",
  "percent_out_of_range",
  "percent_above_range",
  "percent_above_range_level_1",
  "percent_above_range_level_2",
  "percent_below_range",
  "percent_below_range_level_1",
  "percent_below_range_level_2"
)

results <- data.frame(
  Metric = metrics_to_correlate,
  Correlation = numeric(length(metrics_to_correlate)),
  P_Value = numeric(length(metrics_to_correlate))
)

for (i in seq_along(metrics_to_correlate)) {
  test_result <- cor.test(
    cgm_data_subanalysis$psqi_global_score,
    cgm_data_subanalysis[[metrics_to_correlate[i]]],
    method = "spearman"
  )
  results$Correlation[i] <- test_result$estimate
  results$P_Value[i] <- test_result$p.value
}

for (i in 1:nrow(results)) {
  cat(sprintf("%.4f\t%.4f\n",
              results$Correlation[i],
              results$P_Value[i]))
}


for (i in seq_along(metrics_to_correlate)) {
  test_result <- cor.test(
    cgm_data_subanalysis$hfs,
    cgm_data_subanalysis[[metrics_to_correlate[i]]],
    method = "spearman"
  )
  results$Correlation[i] <- test_result$estimate
  results$P_Value[i] <- test_result$p.value
}

for (i in 1:nrow(results)) {
  cat(sprintf("%.4f\t%.4f\n",
              results$Correlation[i],
              results$P_Value[i]))
}


for (i in seq_along(metrics_to_correlate)) {
  test_result <- cor.test(
    cgm_data_subanalysis$age,
    cgm_data_subanalysis[[metrics_to_correlate[i]]],
    method = "spearman"
  )
  results$Correlation[i] <- test_result$estimate
  results$P_Value[i] <- test_result$p.value
}

for (i in 1:nrow(results)) {
  cat(sprintf("%.4f\t%.4f\n",
              results$Correlation[i],
              results$P_Value[i]))
}

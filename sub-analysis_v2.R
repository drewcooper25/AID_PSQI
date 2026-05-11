###——————————————————————Tables, Stats and Figures———————————————————————————###
###——————————————————————————————————————————————————————————————————————————###
# Here we're going to import study_data.xlsx (now that the data is a bit more cleaned up) and generate some nice tables, do some statistical analyses, and generate some pretty figures. Let's go!

###——————————————————————————————————————————————————————————————————————————###
# Load libraries and clear environment before reading in the excel file
library(readxl)
library(writexl)
library(tidyverse)
library(glue)

rm(list=ls())

study_data <- read_excel("/Users/drew.cooper/AID_PSQI/study_data.xlsx")

###——————————————————————————————————————————————————————————————————————————###
# Creating Table 1

# ===========================
# 0) Group reformatting
# ===========================
# OS-AID reformatting (more complex bc multiple strings per cell)
study_data <- study_data %>%
  mutate(
    diabetes_mgmt = str_replace_all(
      diabetes_mgmt, "\\b(AndroidAPS|Loop|OpenAPS)\\b", "OS-AID"
    ),
    # Remove duplicates after replacement
    diabetes_mgmt = map_chr(diabetes_mgmt, ~ {
      .x %>%
        str_split(",\\s*") %>%          # split into vector
        unlist() %>%
        unique() %>%                    # remove duplicates
        paste(collapse = ", ")          # reconstruct
    })
  )

# Inhaled insuli, SGLT2-Inhibitors and Other reformatting
study_data <- study_data %>%
  mutate(
    diabetes_mgmt = str_replace_all(
      diabetes_mgmt, "\\b(Inhaled insulin|SGLT2-Inhibitors|Other)\\b", "IhI/SGLT2-I/Other"
    ),
    diabetes_mgmt = map_chr(diabetes_mgmt, ~ {
      .x %>%
        str_split(",\\s*") %>%          # split into vector
        unlist() %>%
        unique() %>%                    # remove duplicates
        paste(collapse = ", ")          # reconstruct
    })
  )


# Education reformatting
study_data <- study_data %>%
  mutate(
    education = case_when(
      education %in% c("Master's degree or equivalent level (e.g. MA, MS, MEd)",
                       "Professional degree or equivalent level (e.g. MD, DDS, DVM)",
                       "Doctorate (e.g. PhD, EdD)") ~ 
        "Post-graduate Degree (MSc, MD, PhD, etc.",
      education %in% c("Associate degree (e.g. AA, AS)",
                       "Bachelor's degree or equivalent level (e.g. BA, BS)") ~
        "Undergraduate Degree",
      TRUE ~ education
    )
  )

# Creating the demographics table...
# 1) Define groups
groups <- list(
  Overall   = study_data,
  Users     = filter(study_data, enrollment_type == "User"),
  Non_users = filter(study_data, enrollment_type == "Non-user"),
  Subsample = filter(study_data, enrollment_type == "User", days_of_data > 25)
)

numericals <- c("age", "A1c", "psqi_global_score", "hfs")
categoricals <- c("gender", "type_of_diabetes", "diabetes_mgmt", "ethnicity", "education")

# 2) Summary helper functions
# Numerical variables: mean ± sd
summ_num_mean <- function(df, var) {
  tibble(
    level = "mean ± sd",
    value = sprintf("%.1f ± %.1f",
                    mean(df[[var]], na.rm = TRUE),
                    sd(df[[var]], na.rm = TRUE))
  )
}

summ_num_med <- function(df, var) {
  vals <- df[[var]]
  q <- quantile(vals, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  tibble(
    level = "median [Q1–Q3]",
    value = sprintf("%.1f [%.1f–%.1f]", q[2], q[1], q[3])
  )
}

# Categorical variables: n (%)
summ_cat <- function(df, var) {

  # list of variables that are multi-select and should be split
  multi_vars <- c("diabetes_mgmt")
  
  # number of individuals in this specific column
  denom <- nrow(df)

  df_proc <- df %>%
    mutate(tmp = as.character(.data[[var]]))

  # only split multi-select variables
  if (var %in% multi_vars) {
    df_proc <- df_proc %>%
      mutate(tmp = strsplit(tmp, "\\s*,\\s*")) %>%
      tidyr::unnest(tmp)
  }

  df_proc %>%
    rename(level = tmp) %>%
    count(level, sort = FALSE) %>%
    mutate(value = paste0(n, " (", round(100 * n / denom, 1), "%)")) %>%
    select(level, value)
}

num_methods <- list(
  age  = summ_num_mean,
  A1c  = summ_num_med,
  psqi_global_score = summ_num_med,
  hfs = summ_num_med
)

# Build summary for a variable across all groups
build_var <- function(var) {
  tmp <- map_df(names(groups), function(g) {
    gdf <- groups[[g]]
    
    # Determine correct summary function
    if (is.numeric(gdf[[var]])) {
      # If var is in num_methods, use that function; otherwise default
      fun <- num_methods[[var]] %||% summ_num_med
      out <- fun(gdf, var)
    } else {
      out <- summ_cat(gdf, var)
    }
    
    out %>% mutate(group = g)
  })
  
  tmp %>%
    pivot_wider(
      names_from = group,
      values_from = value,
      values_fill = "0 (0%)"
    ) %>%
    mutate(variable = var, .before = 1)
}

# 3) Count row (automatic)
count_row <- {
  total_n <- nrow(groups$Overall)
  
  tibble(
    variable = "Count",
    level = "n (%)",
    !!!setNames(
      map(names(groups), function(g) {
        n <- nrow(groups[[g]])
        pct <- round(100 * n / total_n, 1)
        paste0(n, " (", pct, "%)")
      }),
      names(groups)
    )
  )
}

# 4) Build Demographics Table
table_1 <- bind_rows(
  count_row,
  map_df(c(numericals, categoricals), build_var)
)

# specify variable order:
variable_order <- c(
  "Count",
  "age",
  "gender",
  "type_of_diabetes",
  "diabetes_mgmt",
  "ethnicity",
  "education",
  "annual_income",
  "psqi_global_score",
  "hfs",
  "A1c"
)

table_1 <- table_1 %>%
  # enforce variable order
  mutate(variable = factor(variable, levels = variable_order, ordered = TRUE)) %>%
  # enforce level order within variable
  rowwise() %>% ungroup() %>%
  arrange(variable)

# 6) Output
table_1
#write_xlsx(table_1, "/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/table_1.xlsx")

###——————————————————————————————————————————————————————————————————————————###
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

# --- Subsample ---
Subsample <- study_data %>%
  filter(enrollment_type == "User",
         days_of_data > 25)

# Variables to summarize
gly_vars <- c("A1c", "TIR", "TITR", "TAR", "TAR1",
              "TAR2", "TBR", "TBR1", "TBR2", "mean", "sd", "cv")

# Helper to summarize one group
summarize_group <- function(df, label, N_total) {
  tibble(
    group = label,
    count = fmt_n_percent(nrow(df), N_total),
    !!!setNames(
      map(df[gly_vars], fmt_median_iqr),
      gly_vars
    )
  )
}

# Total N for percentages
N_total <- nrow(Subsample)

# Build summary
summary_subset <-
  bind_rows(
    summarize_group(Subsample, "Overall", N_total)
  )

# Flip axes
summary_subset_flipped <- summary_subset %>%
  ungroup() %>%   # <-- important
  pivot_longer(
    cols = -group,
    names_to = "variable",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = group,
    values_from = value
  ) %>%
  arrange(variable)

summary_subset_flipped <- as.data.frame(t(summary_subset))
names(summary_subset_flipped) <- summary_subset$group
summary_subset_flipped <- tibble::rownames_to_column(summary_subset_flipped, "variable")

#write_xlsx(summary_subset_flipped, "/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/Subsample_glycemic_outcomes.xlsx")

###——————————————————————————————————————————————————————————————————————————###

# Spearman correlations between primary outcomes (PSQI, HFS-II, A1c) and demographic features (Age, Diabetes Duration and Gender)
cor.test(
  study_data$diabetes_duration_years,
  study_data$psqi_global_score,
  method = "spearman",
  use = "complete.obs"
)

# Variables
x_vars <- c("age", "diabetes_duration_years")
y_vars <- c("psqi_global_score", "hfs", "A1c")

# Function to run Spearman correlation
run_spearman <- function(x, y, data) {
  test <- cor.test(
    data[[x]],
    data[[y]],
    method = "spearman",
    use = "complete.obs"
  )
  
  tibble(
    independent_var = x,
    dependent_var = y,
    spearman_rho = unname(test$estimate),
    p_value = test$p.value # still need to add Holm-Bonf test...
  )
}

# Run all combinations
correlation_table <- expand.grid(
  x = x_vars,
  y = y_vars,
  stringsAsFactors = FALSE
) %>%
  pmap_dfr(~ run_spearman(..1, ..2, study_data))

correlation_table

# Subsample correlations
correlation_table_subsample <- expand.grid(
  x = x_vars,
  y = y_vars,
  stringsAsFactors = FALSE
) %>%
  pmap_dfr(~ run_spearman(..1, ..2, study_data %>% filter(enrollment_type == "User" & days_of_data >= 25)))

correlation_table_subsample

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

# Example usage
psqi_vars <- grep("^psqi_(?!.*other)", names(study_data), value = TRUE, perl = TRUE)
hfs_vars <- grep("^hfs", names(study_data), value = TRUE)
glycemic_vars <- c("A1c", "TIR", "TITR", "TAR", "TAR1", "TAR2", "TBR", "TBR1", "TBR2", "mean", "sd", "cv")

# PSQI Wilcoxon results (swap in "enrollment_type", "gender", "diabetes_group", or "AID_type")
psqi_results <- compare_groups(study_data, "enrollment_type", psqi_vars)
#write.csv(psqi_results, "/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/Wilcoxon-results/psqi_results_AID.csv", row.names = FALSE)

# HFS Wilcoxon results
hfs_results <- compare_groups(study_data, "enrollment_type", hfs_vars)
#write.csv(hfs_results, "/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/Wilcoxon-results/hfs_results_AID.csv", row.names = FALSE)

# Glycemic measures Wilcoxon results
glycemic_results <- compare_groups(study_data, "AID_type", "A1c") # glycemic_vars or "A1c" for "AID_type" 
#write.csv(glycemic_results, "/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/Wilcoxon-results/glycemic_results_AID.csv", row.names = FALSE)

# Glycemic measures Wilcoxon; n=60 subsample gendered analyse
subsample <- study_data %>%
  filter(enrollment_type == "User", days_of_data > 25, !is.na(gender))
subsample_results <- compare_groups(subsample, "gender", glycemic_vars)

# write.csv(subsample_results, "/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/Wilcoxon-results/glycemic_results_subsample-gender.csv", row.names = FALSE)

###——————————————————————————————————————————————————————————————————————————###
# Questionnaire Wilcoxon testing by subgroup
wilcox.test(psqi_global_score ~ diabetes_group, study_data) # T1D/LADA vs. T2D/MODY
wilcox.test(psqi_global_score ~ diabetes_group, study_data %>% filter(enrollment_type == "User"))
wilcox.test(psqi_global_score ~ AID_type, study_data) # OS-AID vs. C-AID
wilcox.test(hfs ~ AID_type, study_data) 

###——————————————————————————————————————————————————————————————————————————###
# Questionnaire (PSQI vs. HFS-II) Spearman correlation
cor.test(
  study_data$psqi_global_score,
  study_data$hfs,
  method = "spearman",
  use = "complete.obs"
)

# Spearman correlations between questionnaires and glycemic outcomes
questionnaire_vars <- c("psqi_global_score", "hfs")

corr_table <- function(data, row_vars, col_vars) {
  
  p_values <- c() # store p values so Holm is applied correctly
  results  <- list()
  
  for (r in row_vars) {
    for (c in col_vars) {
      
      cor_test <- suppressWarnings(
        cor.test(data[[r]], data[[c]], method = "spearman")
      )
      
      rho <- cor_test$estimate
      p   <- cor_test$p.value
      
      p_values <- c(p_values, p)
      results[[paste(r, c, sep="|")]] <- list(rho = rho, p = p)
    }
  }
  
  # Holm adjustment
  p_adj_all <- p.adjust(p_values, method = "holm")
  
  # Reinsert adjusted p-values
  k <- 1
  for (nm in names(results)) {
    results[[nm]]$p_adj <- p_adj_all[k]
    k <- k + 1
  }
  
  # Build final table
  out <- matrix("", nrow = length(row_vars), ncol = length(col_vars),
                dimnames = list(row_vars, col_vars))
  
  for (r in row_vars) {
    for (c in col_vars) {
      nm <- paste(r, c, sep="|")
      rho   <- round(results[[nm]]$rho, 3)
      p     <- signif(results[[nm]]$p, 3)
      p_adj <- signif(results[[nm]]$p_adj, 3)
      
      out[r, c] <- paste0(rho, " (", p, "; ", p_adj, ")")
    }
  }
  
  as.data.frame(out)
}

cgm_vars <- c("mean", "sd", "cv", "TIR", "TITR", "TAR", "TAR1", "TAR2", "TBR", "TBR1", "TBR2")

questionnaire_glycemia_corr <- corr_table(
  study_data %>% filter(enrollment_type == "User" & days_of_data >= 25),
  cgm_vars, questionnaire_vars)
questionnaire_glycemia_corr <- rownames_to_column(
  questionnaire_glycemia_corr, var = "glycemic_var"
)
#write_xlsx(questionnaire_glycemia_corr, "/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/questionnaire_glycemia_corr.xlsx")

# A1c analysis (swap between ~ psqi_global_score || $hfs)
cor.test(~ hfs + A1c, study_data, method = "spearman")

cor.test(~ hfs + A1c, study_data %>% filter(enrollment_type == "User"), method = "spearman")
cor.test(~ hfs + A1c, study_data %>% filter(enrollment_type == "Non-user"), method = "spearman")

###——————————————————————————————————————————————————————————————————————————###

# # Subsample Spearman analysis
# cgm_data_subanalysis <- study_data %>%
#   filter(enrollment_type == "User" & days_of_data >= 25)
# 
# metrics_to_correlate_1 <- c(
#   "mean","sd","cv","TIR","TOR","TAR","TBR","TITR","TAR1","TAR2","TBR1","TBR2"
# )
# 
# metrics_to_correlate_2 <- c(
#   "hfs", "psqi_global_score"
# )
# 
# results <- expand_grid(metrics_to_correlate_1, metrics_to_correlate_2) %>%
#   rowwise() %>%
#   pmap(\(metrics_to_correlate_1, metrics_to_correlate_2) {
#     test_result <- cor.test(
#       cgm_data_subanalysis[[metrics_to_correlate_1]],
#       cgm_data_subanalysis[[metrics_to_correlate_2]],
#       method = "spearman"
#     )
#     tibble(
#       metric = glue("{metrics_to_correlate_1} ~ {metrics_to_correlate_2}"),
#       rho_value = test_result$estimate,
#       p_value = test_result$p.value
#     )
#   }) %>%
#   list_rbind() %>%
#   mutate
# 
# # Save
# #write_xlsx(results, "/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/cgm_subsample_spearman_Tebbe.xlsx")
# # these outcomes match up with my calculations; lovely!

###——————————————————————————————————————————————————————————————————————————###

# Histogram for visualisaing variable distributions
hist(study_data$psqi_global_score,
     breaks = 20, col = "purple", border = "pink",
     xlim = c(0, 20), ylim = c(0, 80))

# Cohen's d; so we can see what the outcome is for assumed normality
library(effsize)
cohen.d(d = study_data$hfs,
        f = na.omit(study_data$enrollment_type),
        paired = FALSE,
        conf.level = 0.8)

# Impact Effect Size (nonparametric effect size)
library(ImpactEffectsize)
library(pwr)

Impact(Data = study_data$psqi_global_score,
       Cls = na.omit(study_data$enrollment_type),
       PlotIt = TRUE,
       pde = TRUE,
       col = c("red", "blue"),
       medianLines = TRUE)

# Let's try storing outputs
iES_psqi <- Impact(Data = study_data$psqi_global_score,
                   Cls = na.omit(study_data$enrollment_type),
                   PlotIt = TRUE,
                   pde = TRUE,
                   col = c("purple", "orange"),
                   medianLines = TRUE)$Impact

iES_hfs <- Impact(Data = study_data$hfs,
                  Cls = na.omit(study_data$enrollment_type),
                  PlotIt = TRUE,
                  pde = TRUE,
                  col = c("purple", "orange"),
                  medianLines = TRUE)$Impact

pwr.t.test(d = impact_effect_size, sig.level = 0.05, power = 0.8, type = "two.sample", alternative = "two.sided")$n

###——————————————————————————————————————————————————————————————————————————###
# Graphing PSQI and HFS outcomes
long_df <- study_data %>%
  pivot_longer(cols = c(enrollment_type, gender, diabetes_group, AID_type),
               names_to = "group_type",
               values_to = "group_value") %>%
  bind_rows(
    study_data %>%
      transmute(
        group_type  = "Overall",
        group_value = "Overall",
        psqi_global_score,
        hfs,
        record_id
      )
  )

# Forcing orders of facet plots, and making them one row
long_df <- long_df %>%
  mutate(group_type = factor(
    group_type,
    levels = c("Overall", "gender", "diabetes_group", "AID_type", "enrollment_type"),
    labels = c("Overall", "Gender", "Diabetes Group", "AID Type", "Enrollment Type")
  ))

x_order <- c("Overall",
             "Women", "Men",
             "Type 1/LADA", "Type 2/MODY",
             "OS-AID", "C-AID",
             "User", "Non-user")
long_df <- long_df %>%
  mutate(group_value = factor(group_value, levels = x_order))

# Define custom palette
custom_colors <- c(
  # Overall
  "Overall" = "grey",
  # Enrollment type:
  "User" = "#FFA500", "Non-user" = "#cc5c00",
  # Gender:
  "Women" = "#FFC0CB", "Men" = "#b2334d",
  # Diabetes:
  "Type 1/LADA" = "#CCCCFF", "Type 2/MODY" = "#6666CC",
  # AID type:
  "OS-AID" = "#ef73b2", "C-AID" = "#a61d62"
)

# swap in different y-values from long_df/study_data to assess different outcome measures
p <- ggplot(long_df %>% filter(!is.na(group_value)),
            aes(x = group_value, y = psqi_global_score, fill = group_value)) +
  ylim(0,20) +
  geom_violin(trim = TRUE, color = "grey") +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") +
  geom_hline(yintercept = 5, linetype = "longdash", color = "#dc267f", size = 1) +
  facet_wrap(~group_type, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey"),
    panel.grid.minor = element_line(color = "grey"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    #axis.text.x = element_blank(),
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    strip.text = element_blank(),
    legend.position = "none"
  ) +
  labs(y = "PSQI Score")
p

# ggsave("/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/Figure1.tif",
#        plot = p, width = 20, height = 10, units = "cm", dpi = 300, bg = "white")

###——————————————————————————————————————————————————————————————————————————###
# 100% stacked bar chart of Subsample
Subsample = filter(study_data, enrollment_type == "User", days_of_data > 25)

# Compute TIR_truncated = TIR - TITR
Subsample <- Subsample %>%
  mutate(
    TIR_truncated = TIR - TITR
  )

# Pivot to long format
plot_data <- Subsample %>%
  pivot_longer(
    cols = c(TBR2, TBR1, TITR, TIR_truncated, TAR1, TAR2),
    names_to = "Range",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))  # remove missing values if any

# Factor levels determine stacking order
plot_data$Range <- factor(
  plot_data$Range,
  levels = rev(c("TBR2", "TBR1", "TITR", "TIR_truncated", "TAR1", "TAR2"))
)

# Colour-coding and legend labelling
custom_colors <- c(
  "TBR2" = "#673AB7",
  "TBR1" = "#3F51B5",
  "TIR_truncated" = "#fee08b",
  "TITR" = "#fff7bc",
  "TAR1" = "#d73027",
  "TAR2" = "#a50026"
)

legend_labels <- c(
  "TBR2" = "TBR Level 2",
  "TBR1" = "TBR Level 1",
  "TIR_truncated" = "TIR",
  "TITR" = "TITR",
  "TAR1" = "TAR Level 1",
  "TAR2" = "TAR Level 2"
)

# Plot: each bar = one participant
h <- ggplot(plot_data, aes(x = factor(record_id), y = Value, fill = Range)) +
  geom_col() +
  scale_fill_manual(values = custom_colors, labels = legend_labels) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    x = "",
    y = "Percent Time",
    fill = "Glucose Range"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
h

# ggsave("/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/subset_heatmap.png",
#        plot = h, width = 20, height = 10, units = "cm", dpi = 300, bg = "white")

# for when we want to make things beautiful...
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r

###——————————————————————————————————————————————————————————————————————————###
# Testing for revised submission...

# Some additional brief calculations
num <- nrow(filter(study_data, enrollment_type == "User", psqi_global_score > 5))
denom <- nrow(filter(study_data, enrollment_type == "User", !is.na(psqi_global_score)))

psqi_over5 <- num/denom

# Questionnaire binary calculations
study_data$psqi_bool <- as.numeric(study_data$psqi_global_score >= 5)

cor.test(
  study_data$psqi_bool,
  study_data$hfs,
  method = "spearman",
  use = "complete.obs"
)

cor.test(~ hfs + psqi_bool,
         study_data %>% filter(enrollment_type == "Non-user"),
         method = "spearman",
         use = "complete.obs")

###——————————————————————Tables, Stats and Figures———————————————————————————###
###——————————————————————————————————————————————————————————————————————————###
# Here we're going to import study_data.xlsx (now that the data is a bit more cleaned up) and generate some nice tables, do some statistical analyses, and generate some pretty figures. Let's go!

###——————————————————————————————————————————————————————————————————————————###
# Load libraries and clear environment before reading in the excel file
library(readxl)
library(writexl)
library(tidyverse)
library(glue)
library(ImpactEffectsize)
library(pwr)

rm(list=ls())
source("config.R")

study_data <- read_excel("output/study_data.xlsx")

###——————————————————————————————————————————————————————————————————————————###
# Creating Table 2

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

# Inhaled insulin, SGLT2-Inhibitors and Other reformatting
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
        "Post-graduate Degree (MSc, MD, PhD, etc.)",
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
table_2 <- bind_rows(
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

table_2 <- table_2 %>%
  # enforce variable order
  mutate(variable = factor(variable, levels = variable_order, ordered = TRUE)) %>%
  # enforce level order within variable
  rowwise() %>% ungroup() %>%
  arrange(variable)

# 6) Output
table_2
write.csv(table_2, "output/table_2.csv")

# Impact Effect sizes for PSQI and HFS-II
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

write.csv(summary_subset_flipped, "output/subsample_supp_table_s2.csv")

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
      med1 = round(median1, 3),
      q1_1 = round(q1_1, 3),
      q3_1 = round(q3_1, 3),
      med2 = round(median2, 3),
      q1_2 = round(q1_2, 3),
      q3_2 = round(q3_2, 3),
      #test_used = "Wilcoxon rank-sum",
      #statistic = unname(test$statistic),
      p_value = test$p.value,
      stringsAsFactors = FALSE
    )
  })
  
  results_df <- do.call(rbind, results)
  results_df$adj_p <- p.adjust(results_df$p_value, method = "holm")
  results_df
}

# Get questionnaire / data variable names
psqi_vars <- grep("^psqi_(?!.*other)", names(study_data), value = TRUE, perl = TRUE)
hfs_vars <- grep("^hfs", names(study_data), value = TRUE)
glycemic_vars <- c("A1c", "TIR", "TITR", "TAR", "TAR1", "TAR2", "TBR", "TBR1", "TBR2", "mean", "sd", "cv")

# Generate the Wilcoxon .csv results files
outcomes = list(
  psqi = psqi_vars,
  hfs = hfs_vars,
  glycemic = list(
    enrollment_type = glycemic_vars,
    gender = glycemic_vars,
    diabetes_group = glycemic_vars,
    AID_type = "A1c"
  )
)

groups <- c("enrollment_type", "gender", "diabetes_group", "AID_type")

for (group in groups) {
  for (outcome in c("psqi", "hfs", "glycemic")) {

    vars <- if (outcome == "glycemic") {
      outcomes$glycemic[[group]]
    } else {
      outcomes[[outcome]]
    }

    results <- compare_groups(study_data, group, vars)

    write.csv(
      results,
      sprintf("output/%s_wilcoxon_%s.csv", outcome, group),
      row.names = FALSE
    )
  }
}

###——————————————————————————————————————————————————————————————————————————###
# n=60 subsample analyses
subsample <- study_data %>%
  filter(enrollment_type == "User", days_of_data > 25, !is.na(gender))

subsample_gender_psqi <- compare_groups(subsample, "gender", psqi_vars)
write.csv(subsample_gender_psqi, "output/psqi_wilcoxon_subsample-gender.csv", row.names = FALSE)

subsample_diagroup_psqi <- compare_groups(subsample, "diabetes_group", psqi_vars)
write.csv(subsample_diagroup_psqi, "output/psqi_wilcoxon_subsample-diabetes_group.csv", row.names = FALSE)

subsample_gender_gly <- compare_groups(subsample, "gender", glycemic_vars)
write.csv(subsample_gender_gly, "output/glycemic_wilcoxon_subsample-gender.csv", row.names = FALSE)

###——————————————————————————————————————————————————————————————————————————###
# Questionnaire (PSQI vs. HFS-II) Spearman correlation
cor.test(~ hfs + psqi_global_score, study_data, method = "spearman")
cor.test(~ hfs + psqi_global_score, study_data %>% filter(enrollment_type == "User"), method = "spearman")
cor.test(~ hfs + psqi_global_score, study_data %>% filter(enrollment_type == "Non-user"), method = "spearman")

# A1c-questionnaire analyses
cor.test(~ psqi_global_score + A1c, study_data, method = "spearman")
cor.test(~ psqi_global_score + A1c, study_data %>% filter(enrollment_type == "User"), method = "spearman")
cor.test(~ psqi_global_score + A1c, study_data %>% filter(enrollment_type == "Non-user"), method = "spearman")

cor.test(~ hfs + A1c, study_data, method = "spearman")
cor.test(~ hfs + A1c, study_data %>% filter(enrollment_type == "User"), method = "spearman")
cor.test(~ hfs + A1c, study_data %>% filter(enrollment_type == "Non-user"), method = "spearman")

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
  cgm_vars, questionnaire_vars
)
questionnaire_glycemia_corr <- rownames_to_column(
  questionnaire_glycemia_corr, var = "glycemic_var"
)
write.csv(questionnaire_glycemia_corr, "output/questionnaire_glycemia_corr.csv", row.names = FALSE)

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
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    strip.text = element_blank(),
    legend.position = "none"
  ) +
  labs(y = "PSQI Score")
p
ggsave("figures/Figure1.tif", plot = p, width = 20, height = 10, units = "cm", dpi = 300, bg = "white")

h <- ggplot(long_df %>% filter(!is.na(group_value)),
            aes(x = group_value, y = hfs, fill = group_value)) +
  ylim(0,45) +
  geom_violin(trim = TRUE, color = "grey") +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "white") +
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
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    strip.text = element_blank(),
    legend.position = "none"
  ) +
  labs(y = "PSQI Score")
h
ggsave("figures/Figure2.tif", plot = h, width = 20, height = 10, units = "cm", dpi = 300, bg = "white")

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
b <- ggplot(plot_data, aes(x = factor(record_id), y = Value, fill = Range)) +
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
b

ggsave("figures/subset_heatmap.tif", plot = b, width = 20, height = 10, units = "cm", dpi = 300, bg = "white")

###——————————————————————————————————————————————————————————————————————————###
###——————————————————————————————————————————————————————————————————————————###
### Testing for revised submission...

# Some additional brief calculations
num <- nrow(filter(study_data, enrollment_type == "User", psqi_global_score > 5))
denom <- nrow(filter(study_data, enrollment_type == "User", !is.na(psqi_global_score)))

psqi_over5 <- num/denom

# Questionnaire binary calculations
study_data$psqi_bool <- as.numeric(study_data$psqi_global_score >= 5)

cor.test(~ hfs + psqi_bool, study_data, method = "spearman")
cor.test(~ hfs + psqi_bool, study_data %>% filter(enrollment_type == "User"), method = "spearman")
cor.test(~ hfs + psqi_bool, study_data %>% filter(enrollment_type == "Non-user"), method = "spearman")

###——————————————————————————————————————————————————————————————————————————###
# Spearman correlations between primary outcomes (PSQI, HFS-II, A1c) and demographic features (Age, Diabetes Duration and Gender); full dataset and AID subsample
run_correlations <- function(data) {
  
  x_vars <- c("age", "diabetes_duration_years")
  y_vars <- c("psqi_global_score", "hfs", "A1c")
  
  var_pairs <- expand.grid(
    independent_var = x_vars,
    dependent_var = y_vars,
    stringsAsFactors = FALSE
  )
  
  results <- do.call(
    rbind,
    lapply(seq_len(nrow(var_pairs)), function(i) {
      
      x <- var_pairs$independent_var[i]
      y <- var_pairs$dependent_var[i]
      
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
        p_value = test$p.value
      )
    })
  ) %>%
    mutate(
      p_holm = p.adjust(p_value, method = "holm")
    )
  
  return(results)
}

# Define datasets
datasets <- list(
  full_sample = study_data,
  
  subsample = study_data %>%
    filter(
      enrollment_type == "User",
      days_of_data >= 25
    )
)

# Run analyses + export CSVs
results_list <- lapply(names(datasets), function(name) {
  results <- run_correlations(datasets[[name]])
  
  write.csv(results, sprintf("output/%s_%s.csv", name, "age_diabetes_duration_corr"), row.names = FALSE)

})

###——————————————————————————————————————————————————————————————————————————###
# AID users vs non-users, HFS-II, controlled for age and diabetes duration and gender
model <- lm(
  hfs ~ AID_type + age + diabetes_duration_years + gender,
  data = study_data
)

summary(model)
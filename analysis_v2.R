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

source("cleanup_data.R")
source("gv_metrics.R")
source("gateway_linkages.R")
source("psqi.R")

redcap_data_file <- "C:/Users/Tebbe/Desktop/SIESTA/OPEN_DATA_2023-07-18_1202.csv"
gateway_linkages_file <- "C:/Users/Tebbe/Desktop/SIESTA/Participants_FULL_BIGOPEN+OPENLight_2021.07.13.xlsx"
bg_readings_file <- "C:/Users/Tebbe/Desktop/SIESTA/BgReadings.xlsx"
labels_file <- "C:/Users/Tebbe/Desktop/SIESTA/psqi_other_labels.xlsx"

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
    psqi_component_1,
    psqi_component_2,
    psqi_component_3,
    psqi_component_4,
    psqi_component_5,
    psqi_component_6,
    psqi_component_7,
    psqi_global_score,
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
    hfs,
    psqi_5other,
    a1c
  ) %>%
  filter(!is.na(psqi_global_score) & !is.na(hfs)) %>%
  left_join(gv_metrics, "project_member_id") %>%
  left_join(labels, "record_id")


by(study_data$a1c, study_data$enrollment_type, summary)
sd(study_data %>%
     filter(enrollment_type == "Adult using DIYAPS") %>%
     pull(a1c), na.rm = TRUE) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult using DIYAPS" & !is.na(a1c))))
sd(study_data %>%
     filter(enrollment_type == "Adult not using DIYAPS") %>%
     pull(a1c), na.rm = TRUE) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult not using DIYAPS" & !is.na(a1c))))
wilcox.test(a1c ~ enrollment_type, data = study_data)

cor.test(study_data$a1c, study_data$psqi_global_score, method = "spearman")
cor.test(
  study_data %>% filter(enrollment_type == "Adult using DIYAPS") %>% pull(a1c),
  study_data %>% filter(enrollment_type == "Adult using DIYAPS") %>% pull(psqi_global_score),
  method = "spearman"
)
cor.test(
  study_data %>% filter(enrollment_type == "Adult not using DIYAPS") %>% pull(a1c),
  study_data %>% filter(enrollment_type == "Adult not using DIYAPS") %>% pull(psqi_global_score),
  method = "spearman"
)

cor.test(study_data$a1c, study_data$hfs, method = "spearman")
cor.test(
  study_data %>% filter(enrollment_type == "Adult using DIYAPS") %>% pull(a1c),
  study_data %>% filter(enrollment_type == "Adult using DIYAPS") %>% pull(hfs),
  method = "spearman"
)
cor.test(
  study_data %>% filter(enrollment_type == "Adult not using DIYAPS") %>% pull(a1c),
  study_data %>% filter(enrollment_type == "Adult not using DIYAPS") %>% pull(hfs),
  method = "spearman"
)

cor.test(study_data$psqi_global_score, study_data$hfs, method = "spearman")
cor.test(
  study_data %>% filter(enrollment_type == "Adult using DIYAPS") %>% pull(psqi_global_score),
  study_data %>% filter(enrollment_type == "Adult using DIYAPS") %>% pull(hfs),
  method = "spearman"
)
cor.test(
  study_data %>% filter(enrollment_type == "Adult not using DIYAPS") %>% pull(psqi_global_score),
  study_data %>% filter(enrollment_type == "Adult not using DIYAPS") %>% pull(hfs),
  method = "spearman"
)

by(study_data$age, study_data$enrollment_type, summary)
sd(study_data %>%
     filter(enrollment_type == "Adult using DIYAPS") %>%
     pull(age), na.rm = TRUE)
sd(study_data %>%
     filter(enrollment_type == "Adult not using DIYAPS") %>%
     pull(age), na.rm = TRUE)

by(study_data$gender, study_data$enrollment_type, summary)
by(study_data$type_of_diabetes, study_data$enrollment_type, summary)
by(study_data$ethnicity, study_data$enrollment_type, summary)

by(study_data$psqi_global_score, study_data$enrollment_type, summary)
sd(study_data %>%
     filter(enrollment_type == "Adult using DIYAPS") %>%
     pull(psqi_global_score)) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult using DIYAPS")))
sd(study_data %>%
     filter(enrollment_type == "Adult not using DIYAPS") %>%
     pull(psqi_global_score)) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult not using DIYAPS")))
wilcox.test(psqi_global_score ~ enrollment_type, data = study_data)

by(study_data$psqi_global_score, study_data$gender, summary)
sd(study_data %>%
     filter(gender == "Female") %>%
     pull(psqi_global_score)) /
  sqrt(nrow(study_data %>% filter(gender == "Female")))
sd(study_data %>%
     filter(gender == "Male") %>%
     pull(psqi_global_score)) /
  sqrt(nrow(study_data %>% filter(gender == "Male")))
wilcox.test(psqi_global_score ~ gender, data = study_data)

by(study_data$hfs, study_data$enrollment_type, summary)
sd(study_data %>%
     filter(enrollment_type == "Adult using DIYAPS") %>%
     pull(hfs)) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult using DIYAPS")))
sd(study_data %>%
     filter(enrollment_type == "Adult not using DIYAPS") %>%
     pull(hfs)) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult not using DIYAPS")))
wilcox.test(hfs ~ enrollment_type, data = study_data)

by(study_data$hfs, study_data$gender, summary)
sd(study_data %>%
     filter(gender == "Female") %>%
     pull(hfs)) /
  sqrt(nrow(study_data %>% filter(gender == "Female")))
sd(study_data %>%
     filter(gender == "Male") %>%
     pull(hfs)) /
  sqrt(nrow(study_data %>% filter(gender == "Male")))
wilcox.test(hfs ~ gender, data = study_data)

by(study_data$hfs_b, study_data$enrollment_type, summary)
sd(study_data %>%
     filter(enrollment_type == "Adult using DIYAPS") %>%
     pull(hfs_b)) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult using DIYAPS")))
sd(study_data %>%
     filter(enrollment_type == "Adult not using DIYAPS") %>%
     pull(hfs_b)) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult not using DIYAPS")))
wilcox.test(hfs_b ~ enrollment_type, data = study_data)

by(study_data$hfs_w, study_data$enrollment_type, summary)
sd(study_data %>%
     filter(enrollment_type == "Adult using DIYAPS") %>%
     pull(hfs_w)) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult using DIYAPS")))
sd(study_data %>%
     filter(enrollment_type == "Adult not using DIYAPS") %>%
     pull(hfs_w)) /
  sqrt(nrow(study_data %>% filter(enrollment_type == "Adult not using DIYAPS")))
wilcox.test(hfs_w ~ enrollment_type, data = study_data)

poor_sleep <- study_data %>% filter(psqi_global_score > 5)
poor_sleep_contingency_table <- matrix(c(
  nrow(poor_sleep %>% filter(enrollment_type == "Adult using DIYAPS")),
  nrow(study_data %>% filter(enrollment_type == "Adult using DIYAPS")) -
    nrow(poor_sleep %>% filter(enrollment_type == "Adult using DIYAPS")),

  nrow(poor_sleep %>% filter(enrollment_type == "Adult not using DIYAPS")),
  nrow(study_data %>% filter(enrollment_type == "Adult not using DIYAPS")) -
    nrow(poor_sleep %>% filter(enrollment_type == "Adult not using DIYAPS"))
), nrow = 2)
rownames(poor_sleep_contingency_table) <- c("PSQI > 5", "PSQI <= 5")
colnames(poor_sleep_contingency_table) <- c("Users", "Non-Users")

fisher.test(poor_sleep_contingency_table)
chisq.test(poor_sleep_contingency_table)


diabetes_mentioned <- study_data %>% filter(str_detect(labels, "Diabetes"))
diabetes_mentioned_contingency_table <- matrix(c(
  nrow(diabetes_mentioned %>% filter(enrollment_type == "Adult using DIYAPS")),
  nrow(study_data %>% filter(enrollment_type == "Adult using DIYAPS" & psqi_5other != "")) -
    nrow(diabetes_mentioned %>% filter(enrollment_type == "Adult using DIYAPS")),

  nrow(diabetes_mentioned %>% filter(enrollment_type == "Adult not using DIYAPS")),
  nrow(study_data %>% filter(enrollment_type == "Adult not using DIYAPS" & psqi_5other != "")) -
    nrow(diabetes_mentioned %>% filter(enrollment_type == "Adult not using DIYAPS"))
), nrow = 2)
rownames(diabetes_mentioned_contingency_table) <- c("Diabetes Mentioned", "Diabetes Not Mentioned")
colnames(diabetes_mentioned_contingency_table) <- c("Users", "Non-Users")

fisher.test(diabetes_mentioned_contingency_table)
chisq.test(diabetes_mentioned_contingency_table)


childcare_mentioned <- study_data %>% filter(str_detect(labels, "Children"))
childcare_mentioned_contingency_table <- matrix(c(
  nrow(childcare_mentioned %>% filter(enrollment_type == "Adult using DIYAPS")),
  nrow(study_data %>% filter(enrollment_type == "Adult using DIYAPS" & psqi_5other != "")) -
    nrow(childcare_mentioned %>% filter(enrollment_type == "Adult using DIYAPS")),

  nrow(childcare_mentioned %>% filter(enrollment_type == "Adult not using DIYAPS")),
  nrow(study_data %>% filter(enrollment_type == "Adult not using DIYAPS" & psqi_5other != "")) -
    nrow(childcare_mentioned %>% filter(enrollment_type == "Adult not using DIYAPS"))
), nrow = 2)
rownames(childcare_mentioned_contingency_table) <- c("Childcare Mentioned", "Childcare Not Mentioned")
colnames(childcare_mentioned_contingency_table) <- c("Users", "Non-Users")

fisher.test(childcare_mentioned_contingency_table)
chisq.test(childcare_mentioned_contingency_table)



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
      question == "hfs_w_1" ~ "Not recognizing low BG (W1)",
      question == "hfs_w_3" ~ "Passing out in public (W3)",
      question == "hfs_w_9" ~ "Hypo while driving (W9)",
      question == "hfs_w_16" ~ "Interfering with important tasks (W16)",
      question == "hfs_w_17" ~ "Hypo during sleep (W17)",
      question == "hfs_w_18" ~ "Getting emotionally upset (W18)",
      question == "hfs_b_6" ~ "Limited out of town travel (B6)",
      question == "hfs_b_8" ~ "Avoided visiting friends (B8)",
      question == "hfs_b_11" ~ "Made sure others were around (B11)",
      question == "hfs_b_13" ~ "Higher BG in social situations (B13)",
      question == "hfs_b_14" ~ "Higher BG for important tasks (B14)"
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
    x = "Mean Difference (Non-Users Minus Users)",
    y = "HFS-II Question",
    title = "Mean Differences in HFS-II Questions Between Groups",
    subtitle = "Error bars represent 95% confidence intervals",
    shape = "Subscale",
    linetype = "Subscale"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  guides(shape = guide_legend(title = "Subscale"),
         linetype = "none")

ggsave("hfs_differences.png", width = 7, height = 4)

cgm_data_subanalysis <- study_data %>%
  filter(enrollment_type == "Adult using DIYAPS" & days_of_data >= 25)

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

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

setwd("/Users/drew.cooper/AID_PSQI")
getwd()

source("gateway_linkages.R")
source("psqi.R")

source("cleanup_data.R")
redcap_data_file <- "/Users/drew.cooper/REDCap/OPEN_DATA_2023-07-18_1202.csv"
gateway_linkages_file <- "/Users/drew.cooper/OPENonOH/Participants_FULL_BIGOPEN+OPENLight_2021.07.13.xlsx"

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
    ),
    group = if_else(
      condition = enrollment_type == "Adult using DIYAPS" | diynot_dia_management___1 == 1,
      true = "User",
      false = "Non-user"
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

psqi <- calculate_psqi_scores(redcap)

redcap %<>%
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
  ) %<>%
  filter(!is.na(psqi_global_score) & !is.na(hfs))

redcap <- redcap[!duplicated(redcap$record_id),]

redcap %<>%
  mutate(
    # 1. Convert the baseline timestamp to a Date object
    baseline_date = as.Date(baseline_demographic_adults_diyaps_timestamp),

    # 2. Extract components to build the start date
    # We prioritize Month + Year combinations first
    start_date = case_when(
      # Priority 1: DIYNOT specific month/year columns
      !is.na(diynot_diagnosis_date_yyyy) & !is.na(diynot_diagnosis_date_mm) ~
        make_date(year = diynot_diagnosis_date_yyyy, month = diynot_diagnosis_date_mm, day = 1),

      # Priority 2: Standard month/year columns
      !is.na(year_of_diagnosis) & !is.na(month_of_diagnosis) ~
        make_date(year = year_of_diagnosis, month = month_of_diagnosis, day = 1),

      # Priority 3: Year only (using July 1st as a mid-year estimate)
      # We check all three possible year columns provided
      !is.na(diynot_diagnosis_date_yyyy) ~ make_date(year = diynot_diagnosis_date_yyyy, month = 7, day = 1),
      !is.na(diynot_diagnosis_date_yyyy_2) ~ make_date(year = diynot_diagnosis_date_yyyy_2, month = 7, day = 1),
      !is.na(year_of_diagnosis) ~ make_date(year = year_of_diagnosis, month = 7, day = 1),
      !is.na(year_of_diagnosis_2) ~ make_date(year = year_of_diagnosis_2, month = 7, day = 1),

      TRUE ~ as.Date(NA)
    ),

    # 3. Calculate Duration
    # Using time_length ensures leap years and varying month lengths are handled correctly
    diabetes_duration_years = time_length(interval(start_date, baseline_date), "years")
  )

# 1. Histogram with a Density Curve
ggplot(redcap, aes(x = diabetes_duration_years)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "white") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Diabetes Duration", x = "Years", y = "Density")

# 2. Q-Q Plot (Quantile-Quantile)
# If the points follow the diagonal line, the data is likely normal.
ggplot(redcap, aes(sample = diabetes_duration_years)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Diabetes Duration")

# Shapiro-Wilk Test
# If p < 0.05, the data is significantly different from a normal distribution.
shapiro.test(redcap$diabetes_duration_years)

# 1. Calculate the 'Users' and 'Non-users' breakdown
group_stats <- redcap %>%
  group_by(group) %>%
  summarise(
    mean_duration = mean(diabetes_duration_years, na.rm = TRUE),
    sd_duration = sd(diabetes_duration_years, na.rm = TRUE),
    n = n()
  )

# 2. Calculate the 'All' total
all_stats <- redcap %>%
  summarise(
    group = "Overall",
    mean_duration = mean(diabetes_duration_years, na.rm = TRUE),
    sd_duration = sd(diabetes_duration_years, na.rm = TRUE),
    n = n()
  )

# 3. Combine them into one clean table
final_summary <- bind_rows(all_stats, group_stats)

# Display the results
print(final_summary)

# Helper to fix 2-digit years
fix_year <- function(y) {
  # Convert to numeric just in case they are strings
  y <- as.numeric(y)
  case_when(
    is.na(y) ~ as.numeric(NA),
    y < 100 & y > 26 ~ y + 1900,
    y <= 26 ~ y + 2000,
    TRUE ~ y
  )
}

df_hba1c <- redcap %>%
  # STEP 1: Fix all year columns first
  mutate(
    across(
      .cols = matches("year|yyyy"),
      .fns = ~fix_year(.x),
      .names = "fix_{.col}"
    )
  ) %>%
  # STEP 2: Now that 'fix_...' columns exist, build the date
  mutate(
    # 1. Standardize PSQI Date
    psqi_date_only = as.Date(psqi_timestamp),

    # 2. Build HbA1c Date with normalized years
    hba1c_date = case_when(
      # Month + Year: IFCC logic
      !is.na(fix_year_latest_hba1c_ifcc) & !is.na(month_latest_hba1c_ifcc) ~
        make_date(year = fix_year_latest_hba1c_ifcc, month = month_latest_hba1c_ifcc, day = 1),

      # Month + Year: DIYNOT DCCT
      !is.na(fix_diynot_dia_management_hba1c_dcct_date_yyyy) & !is.na(diynot_dia_management_hba1c_dcct_date_mm) ~
        make_date(year = fix_diynot_dia_management_hba1c_dcct_date_yyyy, month = diynot_dia_management_hba1c_dcct_date_mm, day = 1),

      # Month + Year: DIYNOT IFCC
      !is.na(fix_diynot_dia_management_hba1c_ifcc_date_yyyy) & !is.na(diynot_dia_management_hba1c_ifcc_date_mm) ~
        make_date(year = fix_diynot_dia_management_hba1c_ifcc_date_yyyy, month = diynot_dia_management_hba1c_ifcc_date_mm, day = 1),

      # Month + Year: Generic
      !is.na(fix_year_latest_hba1c) & !is.na(month_latest_hba1c) ~
        make_date(year = fix_year_latest_hba1c, month = month_latest_hba1c, day = 1),

      # Year Only Fallbacks (Impute July 1st) using fixed years
      !is.na(fix_year_latest_hba1c_ifcc) ~ make_date(year = fix_year_latest_hba1c_ifcc, month = 7, day = 1),
      !is.na(fix_year_latest_hba1c_ifcc_2) ~ make_date(year = fix_year_latest_hba1c_ifcc_2, month = 7, day = 1),
      !is.na(fix_year_latest_hba1c) ~ make_date(year = fix_year_latest_hba1c, month = 7, day = 1),
      !is.na(fix_year_latest_hba1c_2) ~ make_date(year = fix_year_latest_hba1c_2, month = 7, day = 1),
      !is.na(fix_diynot_dia_management_hba1c_dcct_date_yyyy) ~ make_date(year = fix_diynot_dia_management_hba1c_dcct_date_yyyy, month = 7, day = 1),
      !is.na(fix_diynot_dia_management_hba1c_dcct_date_yyyy_2) ~ make_date(year = fix_diynot_dia_management_hba1c_dcct_date_yyyy_2, month = 7, day = 1),

      TRUE ~ as.Date(NA)
    ),

    # 3. Gap Calculation
    hba1c_psqi_gap_days = as.numeric(psqi_date_only - hba1c_date),
    hba1c_psqi_gap_days = if_else(condition = hba1c_psqi_gap_days < 0, true = NA, false = hba1c_psqi_gap_days)
  )

hba1c_gap_table <- df_hba1c %>% select(psqi_date_only, hba1c_date, hba1c_psqi_gap_days, month_latest_hba1c, year_latest_hba1c, year_latest_hba1c_2, month_latest_hba1c_ifcc, year_latest_hba1c_ifcc, year_latest_hba1c_ifcc_2, diynot_dia_management_hba1c_dcct_date, diynot_dia_management_hba1c_dcct_date_mm, diynot_dia_management_hba1c_dcct_date_yyyy, diynot_dia_management_hba1c_dcct_date_yyyy_2, diynot_dia_management_hba1c_ifcc, diynot_dia_management_hba1c_ifcc_date, diynot_dia_management_hba1c_ifcc_date_mm, diynot_dia_management_hba1c_ifcc_date_yyyy, diynot_dia_management_hba1c_ifcc_date_yyyy_2)

# 4. Reporting the results
hba1c_report <- df_hba1c %>%
  summarise(
    n_valid_dates = sum(!is.na(hba1c_psqi_gap_days)),
    n_missing = sum(is.na(hba1c_psqi_gap_days)),
    mean_gap_days = mean(hba1c_psqi_gap_days, na.rm = TRUE),
    min_gap_days = min(hba1c_psqi_gap_days, na.rm = TRUE),
    max_gap_days = max(hba1c_psqi_gap_days, na.rm = TRUE),
    sd_gap_days = sd(hba1c_psqi_gap_days, na.rm = TRUE)
  )

print(hba1c_report)

df_bmi <- redcap %>%
  mutate(
    # --- HARMONIZE WEIGHT (KG) ---
    weight_kg = case_when(
      # Branch 1: Primary Fields
      weight_dimension_unit_v2 == 1 ~ as.numeric(body_weight_kg_v2),
      weight_dimension_unit_v2 == 2 ~ as.numeric(body_weight_pounds_v2) * 0.453592,

      # Branch 2: DIYNOT Fallback Fields
      diynot_dia_weight_h_l == 1 ~ as.numeric(diynot_dia_weight_kg_h_l),
      diynot_dia_weight_h_l == 2 ~ as.numeric(diynot_dia_weight_lbs_h_l) * 0.453592,

      TRUE ~ NA_real_
    ),

    # --- HARMONIZE HEIGHT (METERS) ---
    height_m = case_when(
      # Branch 1: Primary Fields
      dimension_unit_height_v2 == 1 ~ as.numeric(height_cm_v2) / 100,
      dimension_unit_height_v2 == 2 ~ (as.numeric(height_inches_v2) * 12 + coalesce(as.numeric(height_inches_2_v2), 0)) * 0.0254,

      # Branch 2: DIYNOT Fallback Fields
      diynot_dia_high_h_l == 1 ~ as.numeric(diynot_dia_high_cm_h_l) / 100,
      diynot_dia_high_h_l == 2 ~ (as.numeric(diynot_dia_high_inches_h_l) * 12 + coalesce(as.numeric(diynot_dia_high_inches_2_h_l), 0)) * 0.0254,

      TRUE ~ NA_real_
    ),

    # --- CALCULATE BMI ---
    bmi = weight_kg / (height_m^2)
  )

bmi_table <- df_bmi %>% select(weight_kg, height_m, bmi,
                               height_cm_v2,
                               height_inches_v2,
                               height_inches_2_v2,
                               body_weight_kg_v2,
                               body_weight_pounds_v2,
                               diynot_dia_high_cm_h_l,
                               diynot_dia_high_inches_h_l,
                               diynot_dia_high_inches_2_h_l,
                               diynot_dia_weight_kg_h_l,
                               diynot_dia_weight_lbs_h_l)

# 1. Calculate the 'Users' and 'Non-users' breakdown for BMI
group_bmi_stats <- df_bmi %>%
  group_by(group) %>%
  summarise(
    n             = sum(!is.na(bmi)),
    mean_bmi      = mean(bmi, na.rm = TRUE),
    sd_bmi        = sd(bmi, na.rm = TRUE),
    min_bmi       = min(bmi, na.rm = TRUE),
    max_bmi       = max(bmi, na.rm = TRUE)
  )

# 2. Calculate the 'All' total for BMI
all_bmi_stats <- df_bmi %>%
  summarise(
    group         = "All",
    n             = sum(!is.na(bmi)),
    mean_bmi      = mean(bmi, na.rm = TRUE),
    sd_bmi        = sd(bmi, na.rm = TRUE),
    min_bmi       = min(bmi, na.rm = TRUE),
    max_bmi       = max(bmi, na.rm = TRUE)
  )

# 3. Combine and Format for easy reading
group_bmi_stats <- df_bmi %>%
  group_by(group) %>%
  summarise(
    n             = sum(!is.na(bmi)),
    mean_bmi      = mean(bmi, na.rm = TRUE),
    sd_bmi        = sd(bmi, na.rm = TRUE),
    median_bmi    = median(bmi, na.rm = TRUE),
    iqr_25        = quantile(bmi, 0.25, na.rm = TRUE),
    iqr_75        = quantile(bmi, 0.75, na.rm = TRUE)
  )

# 2. Calculate stats for 'All'
all_bmi_stats <- df_bmi %>%
  summarise(
    group         = "All",
    n             = sum(!is.na(bmi)),
    mean_bmi      = mean(bmi, na.rm = TRUE),
    sd_bmi        = sd(bmi, na.rm = TRUE),
    median_bmi    = median(bmi, na.rm = TRUE),
    iqr_25        = quantile(bmi, 0.25, na.rm = TRUE),
    iqr_75        = quantile(bmi, 0.75, na.rm = TRUE)
  )

# 3. Combine and Format for Publication
final_bmi_report <- bind_rows(all_bmi_stats, group_bmi_stats) %>%
  mutate(
    # Parametric (for reference)
    mean_sd = paste0(round(mean_bmi, 1), " (±", round(sd_bmi, 1), ")"),
    # Non-Parametric (Recommended given your Shapiro-Wilk result)
    median_iqr = paste0(round(median_bmi, 1), " [", round(iqr_25, 1), " - ", round(iqr_75, 1), "]")
  ) %>%
  select(group, n, median_iqr, mean_sd)

print(final_bmi_report)

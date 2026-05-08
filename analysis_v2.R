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
library(writexl)
library(openxlsx)

rm(list=ls())
setwd("/Users/drew.cooper/AID_PSQI")

source("cleanup_data.R")
source("gv_metrics.R")
source("gateway_linkages.R")
source("psqi.R")

redcap_data_file <- "/Users/drew.cooper/REDCap/OPEN_DATA_2023-07-18_1202.csv"
gateway_linkages_file <- "/Users/drew.cooper/OPENonOH/Participants_FULL_BIGOPEN+OPENLight_2021.07.13.xlsx"
bg_readings_file <- "/Users/drew.cooper/AID_PSQI/BgReadings.xlsx"
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
      levels = c(2, 3, 4, 5, 7, 8, 77777, 99999),
      labels = c(
        "Asian",
        "Arab",
        "Black or African American",
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


# creating new diyaps variable to unify labels and reduce NAs
map <- c(
  "1"="AndroidAPS", "2"="Loop", "3"="OpenAPS",
  "77777"="Other", "88888"="I don't know",
  "99999"="I'd rather not say"
)

redcap <- redcap %>%
  mutate(across(starts_with("type_of_diyaps___"),
                ~ ifelse(.x == 1, map[sub(".*___", "", cur_column())], NA))) %>%
  unite(type_of_diyaps, starts_with("type_of_diyaps___"),
        sep = ", ", na.rm = TRUE)

map2 <- c(
  "1"     = "C-AID",
  "2"     = "SAPT",
  "3"     = "rtCGM/isCGM",
  "4"     = "SMBG",
  "5"     = "Insulin pump",
  "6"     = "MDI",
  "7"     = "Inhaled insulin",
  "8"     = "SGLT2-Inhibitors",
  "77777" = "Other",
  "88888" = "I don't know",
  "99999" = "I'd rather not say"
)

redcap <- redcap %>%
  mutate(across(starts_with("diynot_dia_management___"),
                ~ ifelse(.x == 1,
                         map2[sub(".*___", "", cur_column())],
                         NA))) %>%
  unite(diynot_dia_management,
        starts_with("diynot_dia_management___"),
        sep = ", ",
        na.rm = TRUE)

redcap <- redcap %>%
  mutate(
    diabetes_mgmt = case_when(
      !is.na(type_of_diyaps) & type_of_diyaps != "" &
        !is.na(diynot_dia_management) & diynot_dia_management != "" ~
        paste(type_of_diyaps, diynot_dia_management, sep = ", "),
      
      !is.na(type_of_diyaps) & type_of_diyaps != "" ~ type_of_diyaps,
      !is.na(diynot_dia_management) & diynot_dia_management != "" ~ diynot_dia_management,
      
      TRUE ~ NA_character_
    )
  )

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
    diabetes_mgmt,
    type_of_diabetes,
    other_type_of_diabetes,
    gender,
    ethnicity,
    country_of_origin,
    education,
    occpational_status,
    work_field,
    annual_income,
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
  left_join(labels, "record_id") %>%
  relocate(labels, .after = psqi_5other)

###——————————————————————————————————————————————————————————————————————————###

# Some final cleaning/renaming/restructuring:
study_data <- study_data[ !duplicated(study_data$record_id), ] # remove duplicates

# Combine "Mixed / Multiple ethnic groups", "Other" ethnicity terms
study_data <- study_data %>%
  mutate(
    ethnicity = case_when(
      ethnicity %in% c("Mixed / Multiple ethnic groups", "Other") ~ 
        "Mixed/Multiple ethnic groups/Other",
      ethnicity %in% c("Black or African American") ~ "Black/African American",
      TRUE ~ ethnicity
    ),
    ethnicity = factor(
      ethnicity,
      levels = c("Asian", "Arab", "Black/African American", "Hispanic/Latino", "White", "Mixed/Multiple ethnic groups/Other", "I'd rather not say")
    )
  )

###
# Rename NAs in type_of_diabetes as "Other" (we only have two and they both indicated nonNA in other_type_of_diabetes)
study_data <- study_data %>%
  mutate(
    type_of_diabetes = fct_na_value_to_level(type_of_diabetes, level = "Other")
  )

# Create diabetes_group for easier analysis later
study_data <- study_data %>%
  mutate(
    diabetes_group = case_when(
      type_of_diabetes %in% c("Type 1", "LADA") ~ "Type 1/LADA",
      type_of_diabetes %in% c("Type 2", "MODY") ~ "Type 2/MODY"
      #TRUE ~ NA_character_  # catches any unexpected values
    ),
    diabetes_group = factor(diabetes_group, levels = c("Type 1/LADA", "Type 2/MODY"))#,
    #age = as.numeric(age)
  ) %>%
  relocate(diabetes_group, .after = type_of_diabetes)
###

# Simplifying AID-use names
levels(study_data$enrollment_type)[levels(study_data$enrollment_type) == "Adult using DIYAPS"] <- "User"
levels(study_data$enrollment_type)[levels(study_data$enrollment_type) == "Adult not using DIYAPS"] <- "Non-user"

# Correction from sex-specific terms to gender-specific
levels(study_data$gender)[levels(study_data$gender) == "Female"] <- "Women"
levels(study_data$gender)[levels(study_data$gender) == "Male"] <- "Men"

study_data <- study_data %>%
  mutate(education = recode(education,
                            `1` = "Less than a high school diploma",
                            `2` = "High school degree or equivalent",
                            `3` = "Some college, no degree",
                            `4` = "Associate degree (e.g. AA, AS)",
                            `5` = "Bachelor's degree or equivalent level (e.g. BA, BS)",
                            `6` = "Master's degree or equivalent level (e.g. MA, MS, MEd)",
                            `7` = "Professional degree or equivalent level (e.g. MD, DDS, DVM)",
                            `8` = "Doctorate (e.g. PhD, EdD)",
                            `9` = "None of the above",
                            `99999` = "I'd rather not say"))


study_data <- study_data %>%
  rename(A1c = a1c,
         TIR = percent_in_range,
         TOR = percent_out_of_range,
         TITR = percent_in_tight_range,
         TAR = percent_above_range,
         TAR1 = percent_above_range_level_1,
         TAR2 = percent_above_range_level_2,
         TBR = percent_below_range,
         TBR1 = percent_below_range_level_1,
         TBR2 = percent_below_range_level_2
  )

# Reassign Commercial AID people to User group
idx <- study_data$enrollment_type == "Non-user" &
  grepl("C-AID", study_data$diabetes_mgmt)
study_data$enrollment_type[idx] <- "User"

# Create new AID_type column for easier comparisons later
study_data <- study_data %>%
  mutate(
    AID_type = case_when(
      str_detect(diabetes_mgmt, regex("AndroidAPS|Loop|OpenAPS", ignore_case = TRUE)) ~ "OS-AID",
      str_detect(diabetes_mgmt, regex("C-AID", ignore_case = TRUE)) ~ "C-AID",
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(AID_type, .after = diabetes_mgmt)

# Explicitly re-code record_id 834 as "OS-AID" $diabetes_mgmt and $AID_type
study_data[study_data$record_id == 834, c("diabetes_mgmt", "AID_type")] <- "OS-AID"

#write_xlsx(study_data, path = "study_data.xlsx")

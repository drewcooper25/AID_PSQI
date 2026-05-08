###—PSQI 5J Processing—###

###——————————————————————————————————————————————————————————————————————————###
# Setup: Load libraries and REDCap data 
library(tidyverse)
library(readxl)
library(writexl)
library(glue)

rm(list = ls()) # clear the environment

study_data <- read_excel("/Users/drew.cooper/AID_PSQI/study_data.xlsx") %>% select(
  record_id,
  gender,
  enrollment_type
)
codes <- read_excel("/Users/drew.cooper/REDCap/psqi_codex_v2_results.xlsx") %>% left_join(study_data, "record_id")

#Define column labels to check; defining psqi 5j outcome labels
cols_to_check <- c(#"0a_missing", "0b_none",
                   "1a_diabetes_related", "1b_diabetes_technology",
                   "2_childcare_related", "3_stress_mental_health",
                   "4_physical_health", "5_environment", "6_other")

# Scoring function: is TRUE if string has 2 or 3 "x's" (defined by agreement criteria)
is_true <- function(x) {
  n_x <- stringr::str_count(x, "x")
  n_x %in% c(2, 3)
}

# Make logical TRUE/FALSE columns
codes_tf <- codes %>%
  mutate(across(all_of(cols_to_check), is_true))

# Function to build a single 2x2 table for one question
make_table <- function(question) {
  tab <- table(
    Group = factor(codes_tf$enrollment_type, levels = c("User", "Non-user")),
    Response = factor(codes_tf[[question]], levels = c(TRUE, FALSE))
  )
  return(tab)
}

# Build named list of 2×2 tables
twoXtwo <- setNames(lapply(cols_to_check, make_table), cols_to_check)

# Compute OR and 95% CI manually
compute_or <- function(tab) {
  # Extract cells
  a <- tab["User","TRUE"]
  b <- tab["User","FALSE"]
  c <- tab["Non-user","TRUE"]
  d <- tab["Non-user","FALSE"]
  
  # If any cell is zero, add 0.5 (Haldane-Anscombe correction)
  if (any(c(a,b,c,d) == 0)) {
    a <- a + 0.5; b <- b + 0.5; c <- c + 0.5; d <- d + 0.5
  }
  
  # Odds ratio
  OR <- (a*d) / (b*c)
  
  # Standard error of log(OR)
  SE_logOR <- sqrt(1/a + 1/b + 1/c + 1/d)
  
  # 95% CI
  lower <- exp(log(OR) - 1.96*SE_logOR)
  upper <- exp(log(OR) + 1.96*SE_logOR)
  
  sprintf("%.2f [%.2f, %.2f]", OR, lower, upper)
}

# add in loop to gather expected values as well
# Function to run chi-squared or Fisher depending on cell counts
run_test <- function(tab) {
  
  # If any expected cell count < 5 → Fisher
  chi_attempt <- tryCatch(chisq.test(tab), error = function(e) NULL)
  
  if (is.null(chi_attempt)) {
    # Fallback if chi-squared fails (e.g., zero margin)
    use_fisher <- TRUE
  } else {
    # Proportion of expected counts < 5
    prop_low <- mean(chi_attempt$expected < 5)
    
    # Rule: chi-squared if ≤ 20% expected < 5, else Fisher
    use_fisher <- prop_low > 0.2
  }
  
  if (use_fisher) {
    test <- fisher.test(tab)
    test_name <- "Fisher"
    stat <- NA  # Fisher has no chi-square statistic
    # Standard residuals are not meaningful for Fisher; use NA
    resid <- c(User = NA, Non_user = NA)
  } else {
    test <- chi_attempt
    test_name <- "Chi-squared"
    stat <- unname(test$statistic)
    # Standardized residuals: extract the two rows corresponding to User/Non-user
    resid <- rowSums(test$stdres, na.rm = TRUE)
  }
  
  # Compute OR if 2x2
  if (all(dim(tab) == c(2,2))) {
    OR <- compute_or(tab)
  } else {
    OR <- NA
  }
  
  list(
    test_used  = test_name,
    test_stat  = stat,
    OR         = OR,
    resid_User = resid[1],
    resid_Non  = resid[2],
    p_value    = test$p.value
  )
}

# Run the test for each 2x2 table
results_list <- lapply(twoXtwo, run_test)

# Convert to clean data frame
chi_results <- tibble(
  question = cols_to_check,
  n_User      = sapply(twoXtwo, function(t) t["User", "TRUE"]),
  n_Non_user  = sapply(twoXtwo, function(t) t["Non-user", "TRUE"]),
  test_used   = sapply(results_list, `[[`, "test_used"),
  test_stat   = sapply(results_list, `[[`, "test_stat"),
  OR          = sapply(results_list, `[[`, "OR"),
  resid_User  = sapply(results_list, `[[`, "resid_User"),
  resid_Non   = sapply(results_list, `[[`, "resid_Non"),
  p_value     = sapply(results_list, `[[`, "p_value")
) %>%
  mutate(adj_p_value = p.adjust(p_value, method = "holm"))

chi_results

total_n <- sum(chi_results$n_User, chi_results$n_Non_user)
total_User     <- sum(chi_results$n_User)
total_Non_user <- sum(chi_results$n_Non_user)

#write.csv(chi_results, "/Users/drew.cooper/Documents/HDS_PhD/ISPAD-JDRF/chi_fish_5j_results.csv", row.names = FALSE)

###——————————————————————————————————————————————————————————————————————————###

# Generating an updated psqi_other_labels file; this block/output is CRITICAL for analysis_v2.R
# This block works independently, so it can be copy-pasted into main.R LATERRRRR

redcap <- read.csv("/Users/drew.cooper/REDCap/OPEN_DATA_2023-07-18_1202.csv") %>% select(
  record_id,
  gender,
  enrollment_type,
  year_of_birth
)
codes <- read_excel("/Users/drew.cooper/REDCap/psqi_codex_v2_results.xlsx") %>% left_join(redcap, "record_id")

#Define column labels to check; defining psqi 5j outcome labels
cols_to_check <- c("0a_missing", "0b_none",
                   "1a_diabetes_related", "1b_diabetes_technology",
                   "2_childcare_related", "3_stress_mental_health",
                   "4_physical_health", "5_environment", "6_other")

# Scoring function: is TRUE if string has 2 or 3 "x's" (defined by agreement criteria)
is_true <- function(x) {
  n_x <- stringr::str_count(x, "x")
  n_x %in% c(2, 3)
}

# Make logical TRUE/FALSE columns
codes_tf <- codes %>%
  mutate(across(all_of(cols_to_check), is_true))

# Get numbered column names
num_cols <- grep("^[0-9]|0[a-b]|1[a-b]", names(codes_tf), value = TRUE)

# Build bool_codes with labels
bool_codes <- within(codes_tf, {
  labels <- apply(codes_tf[num_cols], 1, function(x)
    paste(names(x)[x], collapse = ", "))
})

# Drop numbered columns and columns we "don't need"
bool_codes <- bool_codes[ , !(names(bool_codes) %in% num_cols)]
bool_codes <- subset(bool_codes, select = -c(psqi_5other, gender, enrollment_type, year_of_birth))

# Print and save
print(bool_codes)
#write_xlsx(bool_codes, "/Users/drew.cooper/REDCap/psqi_5j_labels.xlsx")

###——————————————————————————————————————————————————————————————————————————###

# ID disagreements in coding...
# Set-up for filtering
disagreements <- codes %>%
  # 1. ID agreement columns for later processing...
  filter(if_any(all_of(cols_to_check), ~ !(.x %in% c("xxx", "ooo")))) %>%
  
  # 2. Replace agreement values ("xxx" or "ooo") with NA, keep disagreements visible
  mutate(across(all_of(cols_to_check),
                ~ ifelse(.x %in% c("xxx", "ooo"), NA, .x))) %>%
  
  # 3. Keep only relevant columns for review
  select(record_id, psqi_5other, all_of(cols_to_check), enrollment_type, gender, year_of_birth)

# Print and save
print(disagreements)
#write.csv(disagreements, "/Users/drew.cooper/REDCap/psqi_disagreements.csv", row.names = FALSE)
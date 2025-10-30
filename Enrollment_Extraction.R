###—EXTRACTING ENROLLMENT TYPES FOR PSQI CODEX—###

###——————————————————————————————————————————————————————————————————————————###

# Setup...
#Load libraries and REDCap data 
library(tidyverse)
library(readxl)
library(writexl)

redcap <- read.csv("/Users/drew.cooper/REDCap/OPEN_DATA_2023-07-18_1202.csv") %>% select(
  record_id,
  gender,
  enrollment_type,
  year_of_birth
  )
codes <- read_excel("/Users/drew.cooper/REDCap/psqi_codex_v2_results.xlsx") %>% left_join(redcap, "record_id")

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

# Count TRUEs per enrollment_type and column (i.e. how many users/non-users per psqi 5j outcome grouping)
result <- codes_tf %>%
  group_by(enrollment_type) %>%
  summarise(across(all_of(cols_to_check), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# Print results and save
print(result)
write.csv(result, "/Users/drew.cooper/REDCap/enrollment-extraction-counts.csv", row.names = FALSE)

###——————————————————————————————————————————————————————————————————————————###

# Generating an updated psqi_other_labels file...
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
write_xlsx(bool_codes, "/Users/drew.cooper/REDCap/psqi_5j_labels.xlsx")

###——————————————————————————————————————————————————————————————————————————###

# Set up chisq test for significant differences between enrollment types across 5j responses
results <- lapply(cols_to_check, function(col) {
  tab <- table(codes_tf$enrollment_type, codes_tf[[col]])
  test <- chisq.test(tab)
  data.frame(
    column = col,
    p_value = test$p.value,
    stringsAsFactors = FALSE
  )
})

results_df <- do.call(rbind, results)
results_df$adj_p <- p.adjust(results_df$p_value, method = "holm")

# Print and save chisq results
print(results_df)
write.csv(results_df, "/Users/drew.cooper/REDCap/enrollment-psqi5j-chisq-results.csv", row.names = FALSE)

###——————————————————————————————————————————————————————————————————————————###

#Chisq and fisher combined
chi_results <- lapply(cols_to_check, function(col) {
  tab <- table(codes_tf$enrollment_type, codes_tf[[col]])
  
  # Run chi-squared first (for expected counts)
  chi_test <- suppressWarnings(chisq.test(tab))
  expected <- chi_test$expected
  
  # Decide which test to use
  if (any(tab < 5)) { #updated from "expected < 5" to "tab < 5"; stricter criteria for choosing between chisq vs fisher
    test_used <- "Fisher"
    test <- fisher.test(tab)
    chisq_val <- NA  # Fisher’s test doesn’t return a chi-squared statistic
  } else {
    test_used <- "Chi-squared"
    test <- chi_test
    chisq_val <- unname(test$statistic)
  }
  
  # Extract standardized residuals (only available for chi-squared)
  resid <- if (!is.null(chi_test$stdres)) chi_test$stdres else matrix(NA, nrow = 2, ncol = 2)
  
  # Observed counts
  user_count <- tab["0", "TRUE"]
  nonuser_count <- tab["1", "TRUE"]
  
  data.frame(
    category = col,
    test_used = test_used,
    chisq = chisq_val,
    p_value = test$p.value,
    adj_p = NA,  # placeholder for Holm correction
    user_count = user_count,
    nonuser_count = nonuser_count,
    user_residual = resid["1", "TRUE"],
    nonuser_residual = resid["0", "TRUE"],
    stringsAsFactors = FALSE
  )
})

# Combine all results
chi_results_df <- do.call(rbind, chi_results)

# Holm–Bonferroni correction
chi_results_df$adj_p <- p.adjust(chi_results_df$p_value, method = "holm")

# Arrange columns
chi_results_df <- chi_results_df %>%
  select(category, test_used, user_count, nonuser_count, chisq, p_value, adj_p,
         user_residual, nonuser_residual)

# View and/or export
print(chi_results_df)
write.csv(chi_results_df, "/Users/drew.cooper/REDCap/chi_fish_results.csv", row.names = FALSE)

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
write.csv(disagreements, "/Users/drew.cooper/REDCap/psqi_disagreements.csv", row.names = FALSE)
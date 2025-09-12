library(glue)
library(tidyverse)

calculate_psqi_scores <- function(redCap) {
  # Component 1: Subjective sleep quality
  # Numerical representation in redCap corresponds to score
  component_1 <- redCap$psqi_6

  # Component 2: Sleep latency
  # Time to fall asleep
  question_2_score <- case_when(
    redCap$psqi_2 <= 15 ~ 0,
    redCap$psqi_2 <= 30 ~ 1,
    redCap$psqi_2 <= 60 ~ 2,
    redCap$psqi_2 > 60 ~ 3,
    .default = NA_integer_
  )

  # Occurrences of "Cannot get to sleep within 30 minutes"
  # Numerical representation in redCap corresponds to score
  question_5a_score <- as.integer(redCap$psqi_5a)

  # Use integer division for mapping to final component score
  component_2 <- as.integer((question_2_score + question_5a_score + 1) %/% 2)

  # Component 3: Sleep duration
  component_3 <- case_when(
    redCap$psqi_4 > 7 ~ 0,
    # TODO: Verify interval boundaries, 6 -> 1 v.s 6 -> 2
    redCap$psqi_4 >= 6 ~ 1,
    redCap$psqi_4 >= 5 ~ 2,
    redCap$psqi_4 >= 0 ~ 3,
    .default = NA_integer_,
    .ptype = integer()
  )

  # Component 4: Habitual sleep effiency
  getting_up_time <- hm(redCap$psqi_3)
  bedtime <- hm(redCap$psqi_1)
  num_of_hours_in_bed <- as.numeric(getting_up_time - bedtime, units = "hours")
  num_of_hours_in_bed <- if_else(num_of_hours_in_bed <= 0.0, num_of_hours_in_bed + 24.0, num_of_hours_in_bed)
  habitual_sleep_effiency <- redCap$psqi_4 / num_of_hours_in_bed * 100
  component_4 <- case_when(
    # TODO: Verify interval boundaries
    habitual_sleep_effiency > 85 ~ 0,
    habitual_sleep_effiency >= 75 ~ 1,
    habitual_sleep_effiency >= 65 ~ 2,
    habitual_sleep_effiency >= 0 ~ 3,
    .default = NA_integer_,
    .ptype = integer()
  )

  # Component 5: Sleep disturbances
  # TODO: Verify how to handle missing responses of question 5j (other)
  questions_5b_to_5j_scores <- redCap$psqi_5b +
    redCap$psqi_5c +
    redCap$psqi_5d +
    redCap$psqi_5e +
    redCap$psqi_5f +
    redCap$psqi_5g +
    redCap$psqi_5h +
    redCap$psqi_5i +
    ifelse(is.na(redCap$psqi_5othera), 0, redCap$psqi_5othera)
  # Use integer division for mapping to final component score
  component_5 <- as.integer((questions_5b_to_5j_scores + 8) %/% 9)

  # Component 6: Use of sleep medication
  component_6 <- as.integer(redCap$psqi_7)

  # Component 7: Daytime dysfunction
  component_7 <- as.integer((redCap$psqi_8 + redCap$psqi_9 + 1) %/% 2)

  global_score <- as.integer(component_1 +
                            component_2 +
                            component_3 +
                            component_4 +
                            component_5 +
                            component_6 +
                            component_7)
  tibble(
    component_1 = component_1,
    component_2 = component_2,
    component_3 = component_3,
    component_4 = component_4,
    component_5 = component_5,
    component_6 = component_6,
    component_7 = component_7,
    global_score = global_score,
    bedtime = bedtime,
    getting_up_time = getting_up_time,
    psqi_date = as_datetime(redCap$psqi_timestamp, tz = "Europe/Berlin")
  )
}
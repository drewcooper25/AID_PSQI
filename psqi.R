library(glue)
library(tidyverse)

calculatePSQIScores <- function(redCap) {
  # Component 1: Subjective sleep quality
  # Numerical representation in redCap corresponds to score
  component1 <- redCap$psqi_6

  # Component 2: Sleep latency
  # Time to fall asleep
  question2Score <- case_when(
    redCap$psqi_2 <= 15 ~ 0,
    redCap$psqi_2 <= 30 ~ 1,
    redCap$psqi_2 <= 60 ~ 2,
    redCap$psqi_2 > 60 ~ 3,
    .default = NA_integer_
  )

  # Occurrences of "Cannot get to sleep within 30 minutes"
  # Numerical representation in redCap corresponds to score
  question5aScore <- as.integer(redCap$psqi_5a)

  # Use integer division for mapping to final component score
  component2 <- as.integer((question2Score + question5aScore + 1) %/% 2)

  # Component 3: Sleep duration
  component3 <- case_when(
    redCap$psqi_4 > 7 ~ 0,
    # TODO: Verify interval boundaries, 6 -> 1 v.s 6 -> 2
    redCap$psqi_4 >= 6 ~ 1,
    redCap$psqi_4 >= 5 ~ 2,
    redCap$psqi_4 >= 0 ~ 3,
    .default = NA_integer_,
    .ptype = integer()
  )

  # Component 4: Habitual sleep effiency
  gettingUpTime <- hm(redCap$psqi_3)
  bedtime <- hm(redCap$psqi_1)
  numOfHoursInBed <- as.numeric(gettingUpTime - bedtime, units = "hours")
  numOfHoursInBed <- if_else(numOfHoursInBed <= 0.0, numOfHoursInBed + 24.0, numOfHoursInBed)
  habitualSleepEffiency = redCap$psqi_4 / numOfHoursInBed * 100
  component4 <- case_when(
    # TODO: Verify interval boundaries
    habitualSleepEffiency > 85 ~ 0,
    habitualSleepEffiency >= 75 ~ 1,
    habitualSleepEffiency >= 65 ~ 2,
    habitualSleepEffiency >= 0 ~ 3,
    .default = NA_integer_,
    .ptype = integer()
  )

  # Component 5: Sleep disturbances
  # TODO: Verify how to handle missing responses of question 5j (other)
  questions5bTo5jScores <- redCap$psqi_5b +
    redCap$psqi_5c +
    redCap$psqi_5d +
    redCap$psqi_5e +
    redCap$psqi_5f +
    redCap$psqi_5g +
    redCap$psqi_5h +
    redCap$psqi_5i +
    ifelse(is.na(redCap$psqi_5othera), 0, redCap$psqi_5othera)
  # Use integer division for mapping to final component score
  component5 <- as.integer((questions5bTo5jScores + 8) %/% 9)

  # Component 6: Use of sleep medication
  component6 <- as.integer(redCap$psqi_7)

  # Component 7: Daytime dysfunction
  component7 <- as.integer((redCap$psqi_8 + redCap$psqi_9 + 1) %/% 2)

  globalScore <- as.integer(component1 +
                            component2 +
                            component3 +
                            component4 +
                            component5 +
                            component6 +
                            component7)
  tibble(
    component1 = component1,
    component2 = component2,
    component3 = component3,
    component4 = component4,
    component5 = component5,
    component6 = component6,
    component7 = component7,
    globalScore = globalScore,
    bedtime = bedtime,
    gettingUpTime = gettingUpTime,
    psqiDate = as_datetime(redCap$psqi_timestamp, tz = "Europe/Berlin")
  )
}
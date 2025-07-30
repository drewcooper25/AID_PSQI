library("tidyverse")

TIR_UPPER <- 180
TIR_LOWER <- 70
TITR_UPPER <- 140
TITR_LOWER <- 80

summarizeTimeDiffs <- function(bgReadings, filterExpr) {
  filterExpr <- enquo(filterExpr)
  bgReadings %>%
    arrange(date) %>%
    mutate(timeDiff = as.numeric(difftime(date, lag(date), units = "secs"))) %>%
    filter(!!filterExpr) %>%
    # Ignore time ranges where the gap is too large
    filter(timeDiff <= 10800) %>%
    summarise(totalTime = sum(timeDiff, na.rm = TRUE)) %>%
    pull(totalTime)
}

calculateGVMetrics <- function(bgReadings) {
  bgReadings %>%
    summarise(
      totalTime = summarizeTimeDiffs(pick(everything()), TRUE),
      timeInRange = summarizeTimeDiffs(pick(everything()), value >= TIR_LOWER & value <= TIR_UPPER),
      timeOutOfRange = summarizeTimeDiffs(pick(everything()), value < TIR_LOWER | value > TIR_UPPER),
      timeAboveRange = summarizeTimeDiffs(pick(everything()), value > TIR_UPPER),
      timeBelowRange = summarizeTimeDiffs(pick(everything()), value < TIR_LOWER),
      timeInTightRange = summarizeTimeDiffs(pick(everything()), value >= TITR_LOWER & value <= TITR_UPPER),
      mean = mean(value),
      sd = sd(value),
      cv = sd(value) / mean(value) * 100
    ) %>%
    mutate(,
      percentInRange = timeInRange / totalTime,
      percentAboveRange = timeAboveRange / totalTime,
      percentBelowRange = timeBelowRange / totalTime,
      percentOutOfRange = timeOutOfRange / totalTime,
      percentInTightRange = timeInTightRange / totalTime
    )
}
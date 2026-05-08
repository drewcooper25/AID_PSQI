library("tidyverse")

TIR_UPPER <- 180
TIR_LOWER <- 70
TITR_UPPER <- 140
TITR_LOWER <- 70 # changed... was 80.

TAR_LEVEL <- 250
TBR_LEVEL <- 54

summarize_time_diffs <- function(bg_readings, filter_expr) {
  filter_expr <- enquo(filter_expr)
  bg_readings %>%
    arrange(date) %>%
    mutate(timeDiff = as.numeric(difftime(date, lag(date), units = "secs"))) %>%
    filter(!!filter_expr) %>%
    # Ignore time ranges where the gap is too large
    filter(timeDiff <= 10800) %>%
    summarise(totalTime = sum(timeDiff, na.rm = TRUE)) %>%
    pull(totalTime)
}

calculate_gv_metrics <- function(bg_readings) {
  bg_readings %>%
    summarise(
      total_time = summarize_time_diffs(pick(everything()), TRUE),
      percent_in_range = summarize_time_diffs(pick(everything()), value >= TIR_LOWER & value <= TIR_UPPER) / total_time * 100,
      percent_out_of_range = summarize_time_diffs(pick(everything()), value < TIR_LOWER | value > TIR_UPPER) / total_time * 100,
      percent_above_range = summarize_time_diffs(pick(everything()), value > TIR_UPPER) / total_time * 100,
      percent_below_range = summarize_time_diffs(pick(everything()), value < TIR_LOWER) / total_time * 100,
      percent_in_tight_range = summarize_time_diffs(pick(everything()), value >= TITR_LOWER & value <= TITR_UPPER) / total_time * 100,
      percent_above_range_level_1 = summarize_time_diffs(pick(everything()), value > TIR_UPPER & value <= TAR_LEVEL) / total_time * 100,
      percent_above_range_level_2 = summarize_time_diffs(pick(everything()), value > TAR_LEVEL) / total_time * 100,
      percent_below_range_level_1 = summarize_time_diffs(pick(everything()), value < TIR_LOWER & value >= TBR_LEVEL) / total_time * 100,
      percent_below_range_level_2 = summarize_time_diffs(pick(everything()), value < TBR_LEVEL) / total_time * 100,
      mean = mean(value),
      sd = sd(value),
      cv = sd(value) / mean(value) * 100
    ) %>%
    mutate(
      days_of_data = seconds(total_time) / days(1)
    ) %>% select(!total_time)
}
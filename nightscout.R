library(jsonlite)
library(tidyverse)

parseNightscoutFiles <- function(files) {
  entries <- files %>%
    filter(collection == "entries") %>%
    pull(path) %>%
    as.list() %>%
    map(~fromJSON(gzfile(.x), flatten = TRUE)) %>%
    keep(is.data.frame) %>% # Ignore empty collections (returned as list() by fromJSON)
    map(~.x %>%
      filter(if ("type" %in% names(.)) type == "sgv" else TRUE) %>%
      select(date, sgv) %>%
      mutate(date = as_datetime(date / 1000))
    ) %>%
    list_rbind() %>%
    distinct()

  treatments <- files %>%
    filter(collection == "treatments") %>%
    pull(path) %>%
    as.list() %>%
    map(~fromJSON(gzfile(.x), flatten = TRUE)) %>%
    keep(is.data.frame) %>% # Ignore empty collections (returned as list() by fromJSON)
    map(~.x %>%
      filter((insulin > 0.0 | carbs > 0.0) & (is.na(duration) | duration == 0.0)) %>%
      mutate(isSMB = if ("isSMB" %in% names(.)) case_when(
        isSMB == TRUE ~ TRUE,
        isSMB == FALSE ~ FALSE,
        isSMB == "true" ~ TRUE,
        isSMB == "false" ~ FALSE,
        TRUE ~ FALSE
      ) else FALSE) %>%
      select(created_at, insulin, carbs, isSMB) %>%
      mutate(created_at = parse_date_time(created_at, orders = c("ymd_HMSz", "ymd_HMz"), tz = "UTC"))
    ) %>%
    list_rbind() %>%
    distinct()

  list(
    entries = entries,
    treatments = treatments
  )
}
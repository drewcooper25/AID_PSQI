# Must be executed after main.R

computeTzCoverage <- function(timezone, offsets) {
  matches <- 0
  for (i in 1:nrow(offsets)) {
    date <- offsets$date[i]
    offset <- offsets$utcOffset[i]
    # Apply timezone to UTC+0 timestamp and then "wrongly" interpret it as UTC+0 again to determine offset
    offsetForTz <- difftime(force_tz(with_tz(date, timezone), "UTC"), date, units = "mins")
    if (offsetForTz == offset) {
      matches <- matches + 1
    }
  }
  matches / nrow(offsets) * 100
}

estimateTimezones <- function(utcOffsets) {
  # Only consider IANA / tzdata timezones, which safely implement DST on all systems
  timezones <- enframe(grep("/", OlsonNames(), value = TRUE), name = NULL, value = "timezone")

  counter <- new.env()
  counter$num <- 1
  utcOffsets %>%
    group_by(projectMemberId) %>%
    group_map(\(offsets, projectMemberId) {
      print(counter$num)
      counter$num <- counter$num + 1
      timezones %>% mutate(
        coverage = map_dbl(timezone, ~computeTzCoverage(.x, offsets)),
        projectMemberId = as.character(projectMemberId[1, 1]),
        dataPoints = nrow(offsets))
    }) %>%
    list_rbind() %>%
    filter(coverage > 0)
}

utcOffsets <- lapply(treatmentData, \(p) { p$androidAps$utcOffsets %||% tibble() %>% mutate(projectMemberId = p$projectMemberId) }) %>% list_rbind()
timezoneEstimates <- estimateTimezones(utcOffsets)

utcOffsetCount <- utcOffsets %>%
  count(projectMemberId, utcOffset) %>%
  arrange(projectMemberId, desc(n)) %>%
  mutate(offset_str = str_glue("{utcOffset} ({n})")) %>%
  group_by(projectMemberId) %>%
  summarise(offset_summary = str_c("AAPS UTC offsets: ", str_c(offset_str, collapse = ", ")))
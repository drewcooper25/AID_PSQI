

manualSleepInteractions <- treatments %>%
  #filter(isSMB == FALSE & (carbs >= 5 | insulin > 0)) %>%
  filter(carbs > 5) %>%
  left_join(psqi %>% select(projectMemberId, bedtime, gettingUpTime), by = "projectMemberId") %>%
  left_join(timezones %>% select(projectMemberId, timezone), by = "projectMemberId") %>%
  mutate(
    localDate = with_tz(date, tzone = timezone),
    timeOfDay = hours(hour(localDate)) +
      minutes(minute(localDate)) +
      seconds(second(localDate))
  ) %>%
  filter(
    (bedtime < gettingUpTime &
      timeOfDay >= bedtime &
      timeOfDay < gettingUpTime) |
      (bedtime > gettingUpTime & (timeOfDay >= bedtime | timeOfDay < gettingUpTime))
  ) %>%
  arrange(projectMemberId, date) %>%
  group_by(projectMemberId) %>%
  mutate(
    timeDiff = as.numeric(difftime(date, lag(date), units = "mins")),
    newCluster = if_else(is.na(timeDiff) | timeDiff > 7, 1, 0),
    clusterId = cumsum(newCluster)
  ) %>%
  ungroup() %>%
  group_by(projectMemberId, clusterId) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(projectMemberId) %>%
  summarise(nSleepInteractions = n(), .groups = "drop") %>%
  inner_join(redCap %>% select(project_member_id, enrollment_type),
             by = c("projectMemberId" = "project_member_id")) %>%
  inner_join(dataQuantity %>% select(projectMemberId, bgReadingsCount), "projectMemberId") %>%
  mutate(nSleepInteractionsScaled = nSleepInteractions / (bgReadingsCount / 8064)) %>%
  filter(!projectMemberId %in% excluded)

data <- inner_join(psqi, manualSleepInteractions, by = "projectMemberId") %>%
  inner_join(gvMetrics, by = "projectMemberId") %>%
  inner_join(redCap %>% select(project_member_id, gender),
             by = c("projectMemberId" = "project_member_id")) %>%
  filter(enrollment_type == 0) %>%
  filter(!projectMemberId %in% excluded) %>%
  drop_na(nSleepInteractions, globalScore)

print(cor.test(data$nSleepInteractions, data$globalScore, method = "spearman"))
print(cor.test(data$nSleepInteractions, data$globalScore, method = "kendall"))



  sleepOffset <- if_else(gettingUpTime < bedtime, days(1), days(0))


  sleepWindows <- tibble(
    date = seq(startDate, endDate - days(1), by = "1 day")
  ) %>%
    mutate(
      sleepStart = date + bedtime,
      sleepEnd = date + sleepOffset + gettingUpTime,
      periodStart = ifelse(date < periodSplit, "Days 1-14", "Days 15-28"),
      periodEnd = ifelse(sleepEnd < periodSplit, "Days 1-14", "Days 15-28")
    ) %>%
    rowwise() %>%
    mutate(
      periodList = list(unique(c(periodStart, periodEnd)))
    ) %>%
    unnest(periodList) %>%
    rename(period = periodList) %>%
    ungroup() %>%
    mutate(
      periodStartDate = case_when(
        period == "Days 1-14" ~ startDate,
        period == "Days 15-28" ~ periodSplit
      ),
      periodEndDate = case_when(
        period == "Days 1-14" ~ periodSplit,
        period == "Days 15-28" ~ psqiDate
      ),
      sleepStartCapped = pmax(sleepStart, periodStartDate),
      sleepEndCapped = pmin(sleepEnd, periodEndDate)
    )


    geom_rect(
      data = sleepWindows,
      aes(xmin = sleepStartCapped, xmax = sleepEndCapped, ymin = -Inf, ymax = Inf),
      fill = "gray95"
    ) +
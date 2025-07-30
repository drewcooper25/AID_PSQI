visualizeMember <- function(projectMemberId, psqiDate, psqiScore, bedtime, gettingUpTime, bgReadings, treatments, timezone, gender, age) {

  periodDefinitions <- list(
    week1 = list(periodStart = psqiDate - hours(28 * 24), periodEnd = psqiDate - hours(21 * 24)),
    week2 = list(periodStart = psqiDate - hours(21 * 24), periodEnd = psqiDate - hours(14 * 24)),
    week3 = list(periodStart = psqiDate - hours(14 * 24), periodEnd = psqiDate - hours(7 * 24)),
    week4 = list(periodStart = psqiDate - hours(7 * 24), periodEnd = psqiDate)
  )

  getPeriod <- \(dates) { map_chr(dates, \(date) { names(keep(periodDefinitions, ~date >= .$periodStart && date < .$periodEnd))[1] }) }

  endDate <- floor_date(with_tz(psqiDate, tzone = timezone), unit = "day")
  startDate <- endDate - days(28)

  bgWithTz <- bgReadings %>% mutate(localTime = with_tz(date, timezone), period = getPeriod(localTime))
  treatmentsWithTz <- treatments %>% mutate(localTime = with_tz(date, timezone), period = getPeriod(localTime))

  dailyLines <- startDate + days(0:28)
  weeklyLines <- dailyLines[wday(dailyLines, week_start = 1) == 1]
  hourLines <- map(startDate + days(0:29), ~. + hours(c(0, 6, 12, 18))) %>%
    flatten_dbl() %>%
    as.POSIXct(tz = timezone)
  hourLines <- hourLines[hourLines <= psqiDate]

  sleepOffset <- if_else(gettingUpTime < bedtime, days(1), days(0))
  sleepWindows <- tibble(date = dailyLines) %>%
    mutate(
      sleepStart = date + bedtime,
      sleepEnd = date + sleepOffset + gettingUpTime
    ) %>%
    # A single sleep window might fall into two different visual periods.
    # We duplicate the window for each applicable sleep window and cap to the period bounds.
    rowwise() %>%
    mutate(periodList = list(unique(c(getPeriod(sleepStart), getPeriod(sleepEnd))))) %>%
    unnest(periodList) %>%
    rename(period = periodList) %>%
    filter(!is.na(period)) %>%
    rowwise() %>%
    mutate(
      periodStartDate = periodDefinitions[[period]]$periodStart,
      periodEndDate = periodDefinitions[[period]]$periodEnd,
      sleepStart = pmax(sleepStart, periodStartDate),
      sleepEnd = pmin(sleepEnd, periodEndDate)
    )

  nBgReadings <- nrow(bgReadings)
  nCarbEntries <- nrow(treatments %>% filter(carbs > 0))
  nManualBoluses <- nrow(treatments %>% filter(insulin > 0 & !isSMB))
  nSMBs <- nrow(treatments %>% filter(insulin > 0 & isSMB))

  # To ensure even scaling across all facets
  blank <- tibble(
    period = names(periodDefinitions),
    periodStart = map_vec(periodDefinitions, "periodStart", .ptype = as.POSIXct(NA)),
    periodEnd = map_vec(periodDefinitions, "periodEnd", .ptype = as.POSIXct(NA))
  ) %>%
    pivot_longer(cols = c(periodStart, periodEnd), names_to = "type", values_to = "x") %>%
    mutate(y = 0)

  p <- ggplot() +
    geom_blank(data = blank, aes(x = x, y = y, group = period)) +
    geom_rect(
      data = sleepWindows,
      aes(xmin = sleepStart, xmax = sleepEnd, ymin = -Inf, ymax = Inf),
      fill = "gray95"
    ) +
    annotate(
      geom = "rect",
      xmin = as.POSIXct(-Inf), xmax = as.POSIXct(Inf), ymin = 70, ymax = 180,
      fill = "palegreen"
    ) +
    geom_vline(xintercept = hourLines, color = "black", alpha = 0.1, size = 0.3) +
    geom_vline(xintercept = dailyLines, color = "black", alpha = 0.3, size = 0.3) +
    geom_vline(xintercept = weeklyLines, color = "black", alpha = 0.8, size = 0.3) +
    geom_hline(yintercept = seq(50, 350, by = 50), color = "black", alpha = 0.1, size = 0.3) +
    geom_point(data = bgWithTz,
               aes(x = localTime, y = value),
               color = "black", alpha = 1, size = 0.5) +
    geom_text(
      data = treatmentsWithTz %>% filter(carbs > 0),
      aes(x = localTime, y = 340, label = paste0(carbs, " g")),
      color = "darkorange", fontface = "bold", angle = 90, hjust = 1, vjust = 0.5, size = 3
    ) +
    geom_text(
      data = treatmentsWithTz %>% filter(insulin > 0 & !isSMB),
      aes(x = localTime, y = 400, label = paste0(insulin, " U")),
      color = "blue", fontface = "bold", angle = 90, hjust = 1, vjust = 0.5, size = 3
    ) +
    geom_point(data = treatmentsWithTz %>% filter(insulin > 0 & isSMB),
               aes(x = localTime, y = 20),
               color = "blue", alpha = 0.5, size = 1) +
    facet_wrap(~period, ncol = 1, scales = "free_x", strip.position = "right") +
    scale_x_datetime(
      expand = c(0, 0),
      breaks = hourLines,
      labels = function(x) format(with_tz(x, tzone = timezone), "%H"),
      sec.axis = sec_axis(
        ~.,
        breaks = dailyLines + hours(12),
        labels = function(x) {
          dt <- with_tz(x, tzone = timezone)
          weekday <- wday(dt, label = TRUE, abbr = TRUE, week_start = 1, locale = "en_US.UTF-8")
          date <- format(dt, "%Y-%m-%d")
          paste0(weekday, "\n", date)
        }
      )
    ) +
    labs(
      title = glue("Project Member ID: {projectMemberId}    {startDate} to {endDate}    Timezone: {timezone}    Gender: {gender}    Age: {age}    PSQI: {psqiScore}    BG Readings: {nBgReadings}    Carb Entries: {nCarbEntries}    Manual Boluses: {nManualBoluses}    SMBs: {nSMBs}"),
      x = NULL,
      y = NULL
    ) +
    theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  return(p)
}

outputDir <- "C:/Users/Tebbe/Desktop/plots"

for (currentMember in unique(bgReadings$projectMemberId)) {
  cli_text("Saving {currentMember}...")
  mPsqi <- psqi %>% filter(projectMemberId == currentMember)
  mRedCap <- redCap %>% filter(project_member_id == currentMember)
  gender <- mRedCap %>%
    pull(gender) %>%
  { case_when(
    . == 1 ~ "F",
    . == 2 ~ "M",
    TRUE ~ NA_character_
  ) }
  psqiDate <- mPsqi %>% pull(psqiDate)
  age <- {
    birthData <- mRedCap %>% select(month_of_birth, year_of_birth)
    birthData$month_of_birth <- ifelse(birthData$month_of_birth == 0, 1, birthData$month_of_birth)

    if (is.na(birthData$month_of_birth) || is.na(birthData$year_of_birth)) {
      NA_real_
    } else {
      birthDate <- as.Date(paste(birthData$year_of_birth, birthData$month_of_birth, "1", sep = "-"))
      round(as.numeric(difftime(psqiDate, birthDate, units = "days")) / 365.25)
    }
  }
  bedtime <- mPsqi %>% pull(bedtime)
  gettingUpTime <- mPsqi %>% pull(gettingUpTime)
  if (is.na(bedtime) || is.na(gettingUpTime))next
  p <- visualizeMember(
    projectMemberId = currentMember,
    psqiDate = psqiDate,
    psqiScore = mPsqi %>% pull(globalScore),
    bedtime = bedtime,
    gettingUpTime = gettingUpTime,
    bgReadings = bgReadings %>% filter(projectMemberId == currentMember),
    treatments = treatments %>% filter(projectMemberId == currentMember),
    timezone = timezones %>%
      filter(projectMemberId == currentMember) %>%
      pull(timezone),
    gender = gender,
    age = age
  )

  dpi <- 300

  ggsave(
    filename = file.path(outputDir, glue("{currentMember}.png")),
    create.dir = TRUE,
    plot = p,
    width = 7680 / dpi,
    height = 4320 / dpi,
    dpi = dpi
  )
}
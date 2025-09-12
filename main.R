library(archive)
library(jsonlite)
library(cli)
library(tidyverse)
library(future)
library(future.apply)
library(progressr)
library(furrr)
library(magrittr)
library(slider)
library(writexl)

source("nightscout.R")
source("androidaps.R")
source("file_indexing.R")
source("gateway_linkages.R")
source("psqi.R")
source("gv_metrics.R")
source("cleanup_data.R")

plan(multisession, workers = availableCores())

openHumansZip <- "C:/Users/Tebbe/Desktop/SIESTA/n=147_OPENonOH_23.5.2023.zip"
extractDir <- "C:/Users/Tebbe/Desktop/extract"
redCapDataFile <- "C:/Users/Tebbe/Desktop/SIESTA/OPEN_DATA_2023-07-18_1202.csv"
gatewayLinkagesFile <- "C:/Users/Tebbe/Desktop/SIESTA/Participants_FULL_BIGOPEN+OPENLight_2021.07.13.xlsx"
timezonesFile <- "C:/Users/Tebbe/Desktop/SIESTA/timezones.xlsx"
outputExcel <- "C:/Users/Tebbe/Desktop/SIESTA/BgReadings.xlsx"


cli_h1("Loading REDCap dataset and Gateway linkages")

gatewayLinks = load_gateway_linkages(gatewayLinkagesFile) %>%
  filter(!is.null(project_member_id) & !is.null(survey_record_id))

redCap <- read.csv(redCapDataFile) %>%
  inner_join(gatewayLinks, by = c("participant_id" = "id")) %>%
  filter(!is.na(project_member_id)) %>%
  # Ignore participants without PSQI scales
  filter(psqi_complete == 2) %>%
  # Ignore excluded participants
  # filter(!(project_member_id) %in% excluded) %>%
  # Adult and Adult Non Users only
  filter(enrollment_type %in% c(0, 1))

cli_alert_success("Found {.strong {nrow(redCap)}} applicable survey responses.")

timezones <- read_excel(timezonesFile)

cli_h1("Loading OpenHumans dataset")

cli_text(format(Sys.time(), "%X"))
cli_text("Extracting archive...")
if (!file.exists(extractDir)) {
  archive_extract(openHumansZip, extractDir)
  cli_alert_success("Successfully extracted archive")
} else {
  cli_alert_warning("Directory already exists. To extract archive again, please delete: {.file {extractDir}}")
}

cli_text(format(Sys.time(), "%X"))
cli_text("Indexing dataset...")
validEntries <- list.files(extractDir, recursive = TRUE, full.names = TRUE)

files <- validEntries %>%
  map(function(path) {
    result <- parseAAPS(path) %||% parseNS(path)
    if (is.null(result) && !str_ends(path, "/")) {
      cli_alert_warning("Unknown file: {.file {path}}")
    }
    return(result)
  }) %>%
  list_rbind()

filesByMember <- files %>%
  group_by(projectMemberId) %>%
  group_split()

cli_alert_success("Detected {.strong {nrow(files)}} relevant files by {.strong {length(filesByMember)}} members in total")

cli_text(format(Sys.time(), "%X"))
cli_text("Loading dataset...")
treatmentData <- with_progress({
  p <- progressor(along = filesByMember)
  future_map(filesByMember, \(files) {
    #map(filesByMember, \(files) {
    projectMemberId <- unique(files$projectMemberId)

    aapsFiles <- files %>% filter(type == "aaps")
    aapsOutput <- NULL
    if (nrow(aapsFiles) > 0) {
      aapsOutput <- parseAndroidAPSFiles(aapsFiles)
    }

    nsFiles <- files %>% filter(type == "ns")
    nsOutput <- NULL

    if (nrow(nsFiles) > 0) {
      nsOutput <- parseNightscoutFiles(nsFiles)
    }

    p()
    list(
      projectMemberId = projectMemberId,
      androidAps = aapsOutput,
      nightscout = nsOutput
    )
    #})
  }, .options = furrr_options(seed = TRUE))
})

# Kill worker processes and free-up memory
plan(sequential)

cli_text(format(Sys.time(), "%X"))
cli_alert_success("Successfully loaded OpenHumans dataset")

rawStudyData <- lapply(treatmentData, \(participant) {
  redCap <- redCap %>%
    filter(project_member_id == participant$projectMemberId) %>%
    as.list()
  if (length(redCap$psqi_timestamp) == 0) return(NULL)
  # REDCap instance was hosted by Charite in Berlin
  psqiTimestamp <- as_datetime(redCap$psqi_timestamp, tz = "Europe/Berlin")

  androidApsBgReadings <- tibble(date = as.POSIXct(character(), tz = "UTC"), value = integer())
  androidApsTreatments <- tibble(date = as.POSIXct(character(), tz = "UTC"), insulin = double(), carbs = integer(), isSMB = logical())

  if (!is.null(participant$androidAps)) {
    androidApsBgReadings <- participant$androidAps$version1$bgReadings %>%
      filter(between(date, psqiTimestamp - days(28), psqiTimestamp))
    androidApsTreatments <- participant$androidAps$version1$treatments %>%
      filter(between(date, psqiTimestamp - days(28), psqiTimestamp))

    # None of the applicable participants have AAPS v2 data for the PSQI time range, so I won't spend time on handling that.
    # Putting stop marks here in case that ever changes.
    participant$androidAps$version2$glucoseValues %>%
      filter(between(timestamp, psqiTimestamp - days(28), psqiTimestamp)) %>%
    { stopifnot("Found AAPS v2 data (glucose values)" = nrow(.) == 0) }
    participant$androidAps$version2$boluses %>%
      filter(between(timestamp, psqiTimestamp - days(28), psqiTimestamp)) %>%
    { stopifnot("Found AAPS v2 data (boluses)" = nrow(.) == 0) }
    participant$androidAps$version2$carbs %>%
      filter(between(timestamp, psqiTimestamp - days(28), psqiTimestamp)) %>%
    { stopifnot("Found AAPS v2 data (carbs)" = nrow(.) == 0) }

  }

  nightscoutBgReadings <- tibble(date = as.POSIXct(character(), tz = "UTC"), value = integer())
  nightscoutTreatments <- tibble(date = as.POSIXct(character(), tz = "UTC"), insulin = double(), carbs = integer(), isSMB = logical())
  if (!is.null(participant$nightscout) &&
    nrow(participant$nightscout$entries) > 0 &&
    nrow(participant$nightscout$treatments) > 0) {
    nightscoutBgReadings <- participant$nightscout$entries %>%
      filter(between(date, psqiTimestamp - days(28), psqiTimestamp)) %>%
      rename(value = sgv)
    nightscoutTreatments <- participant$nightscout$treatments %>%
      filter(between(created_at, psqiTimestamp - days(28), psqiTimestamp)) %>%
      rename(date = created_at)
  }

  bgReadings <- NULL
  treatments <- NULL

  # Use whatever data source has more bg readings
  if (nrow(nightscoutBgReadings) >= nrow(androidApsBgReadings)) {
    bgReadings = nightscoutBgReadings
    treatments = nightscoutTreatments
  } else {
    bgReadings = androidApsBgReadings
    treatments = androidApsTreatments
  }

  # Remove duplicated bg readings with minimal change in time
  bgReadings %<>%
    filter(value >= 30) %<>% # Remove "flat tires" (sensor errors / failures)
    arrange(date) %<>%
    mutate(timeDiff = as.numeric(difftime(date, lag(date), units = "secs"))) %<>%
    filter(is.na(timeDiff) | timeDiff > 60) %<>%
    select(-timeDiff)

  smbThreshold = smb_thresholds[[participant$projectMemberId]]
  if (!is.null(smbThreshold)) {
    treatments %<>% mutate(isSMB = isSMB | ((is.na(carbs) | carbs == 0.0) &
      insulin > 0.0 &
      insulin <= smbThreshold))
  }

  eCarbsThreshold = e_carbs_threshold[[participant$projectMemberId]]
  if (!is.null(eCarbsThreshold)) {
    treatments %<>% filter((!is.na(insulin) & insulin > 0.0) | carbs > eCarbsThreshold)
  }

  list(
    projectMemberId = participant$projectMemberId,
    bgReadings = bgReadings,
    treatments = treatments
  )
}) %>% compact()

bg_readings <- lapply(rawStudyData, \(p) { p$bgReadings %>% mutate(project_member_id = p$projectMemberId) }) %>% list_rbind()
write_xlsx(bg_readings, path = outputExcel)

stop()

dataQuantity <- lapply(rawStudyData, \(participant) {
  tibble(
    projectMemberId = participant$projectMemberId,
    bgReadingsCount = nrow(participant$bgReadings),
    treatmentsCount = nrow(participant$treatments),
    smbs = nrow(participant$treatments %>% filter(isSMB & insulin > 0.0)),
    nonSMBs = nrow(participant$treatments %>% filter(!isSMB & insulin > 0.0)),
    carbEntries = nrow(participant$treatments %>% filter(carbs > 0.0)),
  )
}) %>% list_rbind()

treatments <- lapply(rawStudyData, \(p) { p$treatments %>% mutate(projectMemberId = p$projectMemberId) }) %>% list_rbind()

unannouncedCarbs <- bgReadings %>%
  group_by(projectMemberId) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    value = slide_dbl(
      value,
      .f = mean,
      .before = 2,
      .after = 2,
      .complete = FALSE
    ),
    deltaPerMinute = (value - lag(value)) / as.numeric(difftime(date, lag(date), units = "mins"))
  ) %>%
  filter(deltaPerMinute > 1) %>%
  mutate(
    timeDiff = as.numeric(difftime(date, lag(date), units = "mins")),
    windowGroup = cumsum(if_else(is.na(timeDiff) | timeDiff > 17.5, 1, 0))
  ) %>%
  group_by(projectMemberId, windowGroup) %>%
  mutate(
    duration = as.numeric(difftime(max(date), min(date), units = "mins")),
    avgDelta = mean(deltaPerMinute, na.rm = TRUE) * 5
  ) %>%
  filter(duration >= 20) %>%
  slice_head(n = 1) %>%
  filter(value < 100) %>%
  ungroup() %>%
  mutate(date = date - minutes(30)) %>%
  arrange(projectMemberId, date) %>%
  select(projectMemberId, date, duration, avgDelta)

psqi <- redCap %>%
  calculate_psqi_scores(.) %>%
  mutate(projectMemberId = redCap$project_member_id)

gvMetrics <- bgReadings %>%
  group_by(projectMemberId) %>%
  calculate_gv_metrics()

nightlyBgReadings <- bgReadings %>%
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
  select(projectMemberId, date, localDate, value)

nightlyGvMetrics <- nightlyBgReadings %>%
  group_by(projectMemberId) %>%
  calculate_gv_metrics()

manualSleepInteractions <- treatments %>%
  filter(!isSMB) %>%
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
  filter(!projectMemberId %in% excluded)

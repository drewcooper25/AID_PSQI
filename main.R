library(archive)
library(jsonlite)
library(cli)
library(tidyverse)
library(future)
library(future.apply)
library(progressr)
library(furrr)
library(magrittr)

source("nightscout.R")
source("androidaps.R")
source("file_indexing.R")
source("gateway_linkages.R")
source("psqi.R")
source("gv_metrics.R")

plan(multisession, workers = availableCores())

openHumansZip <- "C:/Users/Tebbe/Desktop/SIESTA/n=147_OPENonOH_23.5.2023.zip"
extractDir <- "C:/Users/Tebbe/Desktop/extract"
redCapDataFile <- "C:/Users/Tebbe/Desktop/SIESTA/OPEN_DATA_2023-07-18_1202.csv"
gatewayLinkagesFile <- "C:/Users/Tebbe/Desktop/SIESTA/Participants_FULL_BIGOPEN+OPENLight_2021.07.13.xlsx"


cli_h1("Loading REDCap dataset and Gateway linkages")

gatewayLinks = loadGatewayLinkages(gatewayLinkagesFile) %>%
  filter(!is.null(project_member_id) & !is.null(survey_record_id))

redCap <- read.csv("C:/Users/Tebbe/Desktop/SIESTA/OPEN_DATA_2023-07-18_1202.csv") %>%
  inner_join(loadGatewayLinkages(gatewayLinkagesFile), by = c("participant_id" = "id")) %>%
  filter(!is.na(project_member_id)) %>%
  # Ignore participants without PSQI scales
  filter(psqi_complete == 2) %>%
  # Adult and Child AID users only
  filter(enrollment_type %in% c(0, 2))

cli_alert_success("Found {.strong {nrow(redCap)}} applicable survey responses.")


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
  # Don't load data from excluded participants
  filter(projectMemberId %in% redCap$project_member_id) %>%
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
    arrange(date) %<>%
    mutate(timeDiff = as.numeric(difftime(date, lag(date), units = "secs"))) %<>%
    filter(is.na(timeDiff) | timeDiff > 60) %<>%
    select(-timeDiff)

  list(
    projectMemberId = participant$projectMemberId,
    bgReadings = bgReadings,
    treatments = treatments
  )
})

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

bgReadings <- lapply(rawStudyData, \(p) { p$bgReadings %>% mutate(projectMemberId = p$projectMemberId) }) %>% list_rbind()
treatments <- lapply(rawStudyData, \(p) { p$treatments %>% mutate(projectMemberId = p$projectMemberId) }) %>% list_rbind()

psqi <- redCap %>%
  calculatePSQIScores(.) %>%
  mutate(projectMemberId = redCap$project_member_id)

gvMetrics <- bgReadings %>%
  group_by(projectMemberId) %>%
  calculateGVMetrics()

ggplot(data = inner_join(gvMetrics, psqi, by = "projectMemberId") %>%
         filter(!is.na(globalScore),
                !is.na(percentOutOfRange)),
       aes(x = percentOutOfRange, y = globalScore)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Percent out of Range",
       y = "PSQI Global Score",
       title = "Percent out of Range vs PSQI Global Score") +
  theme_minimal()
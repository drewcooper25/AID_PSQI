library(archive)
library(jsonlite)
library(cli)
library(tidyverse)
library(future)
library(future.apply)
library(progressr)
library(furrr)

source("nightscout.R")
source("androidaps.R")
source("file_indexing.R")
source("gateway_linkages.R")
source("psqi.R")

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
  psqiTimestamp <- as_datetime(redCap$psqi_timestamp, tz = "Europe/Berlin")
  androidAps <- NULL
  if (!is.null(participant$androidAps)) {
    androidAps <- list(
      version1 = list(
        bgReadings = participant$androidAps$version1$bgReadings %>%
          filter(between(date, psqiTimestamp - days(28), psqiTimestamp)),
        treatments = participant$androidAps$version1$treatments %>%
          filter(between(date, psqiTimestamp - days(28), psqiTimestamp))
      ),
      version2 = list(
        glucoseValues = participant$androidAps$version2$glucoseValues %>%
          filter(between(timestamp, psqiTimestamp - days(28), psqiTimestamp)),
        boluses = participant$androidAps$version2$boluses %>%
          filter(between(timestamp, psqiTimestamp - days(28), psqiTimestamp)),
        carbs = participant$androidAps$version2$carbs %>%
          filter(between(timestamp, psqiTimestamp - days(28), psqiTimestamp))
      )
    )
  }
  nightscout <- NULL
  if (!is.null(participant$nightscout) &&
    nrow(participant$nightscout$entries) > 0 &&
    nrow(participant$nightscout$treatments) > 0) {
    nightscout <- list(
      entries = participant$nightscout$entries %>%
        filter(between(date, psqiTimestamp - days(28), psqiTimestamp)),
      treatments = participant$nightscout$treatments %>%
        filter(between(created_at, psqiTimestamp - days(28), psqiTimestamp))
    )
  }

  list(
    projectMemberId = participant$projectMemberId,
    psqiTimestamp = redCap$psqi_timestamp,
    redCap = redCap,
    androidAps = androidAps,
    nightscout = nightscout
  )
})

dataQuantityTotal <- lapply(treatmentData, \(participant) {
  tibble(
    projectMemberId = participant$projectMemberId,
    aapsV1BgReadingsCount = nrow(participant$androidAps$version1$bgReadings %||% tibble()),
    aapsV1TreatmentsCount = nrow(participant$androidAps$version1$treatments %||% tibble()),
    aapsV2GlucoseValuesCount = nrow(participant$androidAps$version2$glucoseValues %||% tibble()),
    aapsV2BolusesCount = nrow(participant$androidAps$version2$boluses %||% tibble()),
    aapsV2CarbsCount = nrow(participant$androidAps$version2$carbs %||% tibble()),
    nsEntriesCount = nrow(participant$nightscout$entries %||% tibble()),
    nsTreatmentsCount = nrow(participant$nightscout$treatments %||% tibble())
  )
}) %>% list_rbind()

dataQuantityPSQI <- lapply(rawStudyData, \(participant) {
  tibble(
    projectMemberId = participant$projectMemberId,
    aapsV1BgReadingsCount = nrow(participant$androidAps$version1$bgReadings %||% tibble()),
    aapsV1TreatmentsCount = nrow(participant$androidAps$version1$treatments %||% tibble()),
    aapsV2GlucoseValuesCount = nrow(participant$androidAps$version2$glucoseValues %||% tibble()),
    aapsV2BolusesCount = nrow(participant$androidAps$version2$boluses %||% tibble()),
    aapsV2CarbsCount = nrow(participant$androidAps$version2$carbs %||% tibble()),
    nsEntriesCount = nrow(participant$nightscout$entries %||% tibble()),
    nsTreatmentsCount = nrow(participant$nightscout$treatments %||% tibble())
  )
}) %>% list_rbind()
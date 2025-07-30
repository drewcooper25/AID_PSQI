library(archive)
library(jsonlite)
library(tidyverse)

readAndroidAPSFile <- function(archives, file) {
  df <- bind_rows(lapply(archives, \(archive) {
    contents <- archive(archive)$path
    if (file %in% contents) {
      return(archive_read(archive, file) %>% fromJSON())
    }
    return(NULL)
  }))
  return(df)
}

processAAPSV1Data <- function(data, schema) {
  tibble <- data %>% list_rbind()
  if (nrow(tibble) == 0) return(schema)
  tibble <- tibble %>%
    # V1 uses the date timestamp as a primary key...
    # Only keep the entry that was uploaded last.
    group_by(date, applicationId) %>%
    slice_max(order_by = uploadNumber, n = 1, with_ties = FALSE) %>%
    ungroup() %>%

    # Remove invalid entries.
    filter(isValid) %>%

    # If the last uploaded entry for a given timestamp is a "deletion", remove it from the dataset.
  { if ("isDeletion" %in% names(.)) filter(., !isDeletion) else . } %>%

    select(all_of(names(schema)))
  return(tibble)
}

processAAPSV2Data <- function(data, schema) {
  tibble <- data %>% list_rbind()
  if (nrow(tibble) == 0) return(schema)
  tibble <- tibble %>%
    # AAPS DB v2 stores "historic" entries when an entry is modified.
    # These entries reference the original one using "referenceId".
    # Remove them.
  { if ("referenceId" %in% names(.)) filter(., is.na(referenceId)) else . } %>%

    # When an entry is modified, the version is incremented.
    # We only care about the latest version
    group_by(id, applicationId) %>%
    slice_max(order_by = version, n = 1, with_ties = FALSE) %>%
    ungroup() %>%

    # AAPS DB doesn't remove data, instead it is "invalidated". We don't want those entries, too.
    # This field is a little broken, so we need some string matching.
    filter(str_detect(isValid, "isValid=true")) %>%

    select(all_of(names(schema)))
  return(tibble)
}


parseAndroidAPSFiles <- function(files) {
  #V1
  bgReadings <- list()
  treatments <- list()
  utcOffsets <- list()

  #V2
  glucoseValues <- list()
  boluses <- list()
  carbs <- list()

  for (i in seq_len(nrow(files))) {
    file <- files[i,]
    tryCatch({
      innerFiles <- archive(file$path)$path
      if (file$fileVersion == 1) {
        if ("BgReadings.json" %in% innerFiles) {
          bgReadings[[length(bgReadings) + 1]] = archive_read(file$path, "BgReadings.json") %>%
            fromJSON(flatten = TRUE) %>%
            mutate(applicationId = file$applicationId, uploadNumber = file$uploadNumber)
        }
        if ("Treatments.json" %in% innerFiles) {
          treatments[[length(treatments) + 1]] = archive_read(file$path, "Treatments.json") %>%
            fromJSON(flatten = TRUE) %>%
            mutate(applicationId = file$applicationId, uploadNumber = file$uploadNumber) %>%
            select(-starts_with("bolusCalcJson")) # Causes some headache with incompatible column data types, we don't need it...
        }
      } else if (file$fileVersion == 2) {
        if ("GlucoseValues.json" %in% innerFiles) {
          glucoseValues[[length(glucoseValues) + 1]] = archive_read(file$path, "GlucoseValues.json") %>%
            fromJSON(flatten = TRUE) %>%
            mutate(applicationId = file$applicationId)
        }
        if ("Boluses.json" %in% innerFiles) {
          boluses[[length(boluses) + 1]] = archive_read(file$path, "Boluses.json") %>%
            fromJSON(flatten = TRUE) %>%
            mutate(applicationId = file$applicationId)
        }
        if ("Carbs.json" %in% innerFiles) {
          carbs[[length(carbs) + 1]] = archive_read(file$path, "Carbs.json") %>%
            fromJSON(flatten = TRUE) %>%
            mutate(applicationId = file$applicationId)
        }
      } else {
        cli_alert_warning("Unknown file version {.strong {file$fileVersion}}: {.file {file$path}}")
      }
      uploadInfo <- archive_read(file$path, "UploadInfo.json") %>% fromJSON(flatten = TRUE)
      utcOffsets[[length(utcOffsets) + 1]] = tibble(date = as_datetime(uploadInfo$timestamp / 1000), utcOffset = as.integer(uploadInfo$utcOffset / 60000))
    }, error = \(e) {
      if (grepl("Unrecognized archive format", conditionMessage(e))) {
        cli_alert_warning("Invalid archive: {.file {enc2utf8(file$path)}} - skipping.")
      } else {
        stop(e)
      }
    })
  }

  list(
    version1 = list(
      bgReadings = bgReadings %>%
        processAAPSV1Data(tibble(date = integer(), value = numeric())) %>%
        mutate(date = as_datetime(date / 1000)),
      treatments = treatments %>%
        processAAPSV1Data(tibble(date = integer(), insulin = numeric(), carbs = numeric(), isSMB = logical())) %>%
        mutate(date = as_datetime(date / 1000))
    ),
    version2 = list(
      glucoseValues = glucoseValues %>%
        processAAPSV2Data(tibble(timestamp = integer(), value = numeric())) %>%
        mutate(timestamp = as_datetime(timestamp / 1000)),
      boluses = boluses %>%
        processAAPSV2Data(tibble(timestamp = integer(), type = character(), amount = numeric())) %>%
        mutate(timestamp = as_datetime(timestamp / 1000)),
      carbs = carbs %>%
        processAAPSV2Data(tibble(timestamp = integer(), amount = numeric())) %>%
        mutate(timestamp = as_datetime(timestamp / 1000))
    ),
    utcOffsets = utcOffsets %>% list_rbind()
  )
}
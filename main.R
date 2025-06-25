library(archive)
library(jsonlite)
library(cli)
library(tidyverse)
library(future)
library(future.apply)
library(progressr)
library(furrr)

plan(multisession, workers = availableCores())

zipArchive <- "C:/Users/Tebbe/Desktop/SIESTA/n=147_OPENonOH_23.5.2023.zip"
extractDir <- "C:/Users/Tebbe/Desktop/extract"


cli_h1("Loading OpenHumans dataset")

cli_text(format(Sys.time(), "%X"))
cli_text("Extracting archive...")
if (!file.exists(extractDir)) {
  archive_extract(zipArchive, extractDir)
  cli_alert_success("Successfully extracted archive")
} else {
  cli_alert_warning("Directory already exists. To extract archive again, please delete: {.file {extractDir}}")
}

cli_text(format(Sys.time(), "%X"))
cli_text("Indexing dataset...")
validEntries <- list.files(extractDir, recursive = TRUE, full.names = TRUE)

androidapsPattern <- "^.+\\/([0-9]{8})\\/direct-sharing-396\\/upload-num([0-9]+)-ver([0-9]+)-date([0-9]{8}T[0-9]{6})-appid([0-9a-f]{32}).zip$"
nightscoutPattern <- "^.+\\/([0-9]{8})\\/direct-sharing-31\\/([a-zA-Z]+)(?:_([0-9]{4}-[0-9]{2}-[0-9]{2})?_to_([0-9]{4}-[0-9]{2}-[0-9]{2}))?.json.gz$"

parseAAPS <- function(path) {
  match <- regexec(androidapsPattern, path)
  groups <- regmatches(path, match)[[1]]

  if (length(groups) > 0) {
    return(tibble(
      type = "aaps",
      projectMemberId = groups[2],
      path = path,
      uploadNumber = as.integer(groups[3]),
      fileVersion = as.integer(groups[4]),
      uploadDate = groups[5],
      applicationId = groups[6]
    ))
  }

  return(NULL)
}

parseNS <- function(path) {
  match <- regexec(nightscoutPattern, path)
  groups <- regmatches(path, match)[[1]]

  if (length(groups) > 0) {
    return(tibble(
      type = "ns",
      projectMemberId = groups[2],
      path = path,
      collection = groups[3],
      startDate = groups[4],
      endDate = groups[5]
    ))
  }

  return(NULL)
}

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
    }, error = \(e) {
      if (grepl("Unrecognized archive format", conditionMessage(e))) {
        cli_alert_warning("Invalid archive: {.file {enc2utf8(file$path)}} - skipping.")
      } else {
        stop(e)
      }
    })
  }

  return(list(
    bgReadings = bgReadings %>%
      processAAPSV1Data(tibble(date = integer(), value = numeric())) %>%
      mutate(date = as_datetime(date / 1000)),
    treatments = treatments %>%
      processAAPSV1Data(tibble(date = integer(), insulin = numeric(), carbs = numeric(), isSMB = logical())) %>%
      mutate(date = as_datetime(date / 1000)),
    glucoseValues = glucoseValues %>%
      processAAPSV2Data(tibble(timestamp = integer(), value = numeric())) %>%
      mutate(timestamp = as_datetime(timestamp / 1000)),
    boluses = boluses %>%
      processAAPSV2Data(tibble(timestamp = integer(), type = character(), amount = numeric())) %>%
      mutate(timestamp = as_datetime(timestamp / 1000)),
    carbs = carbs %>%
      processAAPSV2Data(tibble(timestamp = integer(), amount = numeric())) %>%
      mutate(timestamp = as_datetime(timestamp / 1000))
  ))
}

cli_text(format(Sys.time(), "%X"))
cli_text("Loading dataset...")
result <- with_progress({
  p <- progressor(along = filesByMember)
  future_map(filesByMember, \(files) {
    projectMemberId <- unique(files$projectMemberId)
    aapsFiles <- files %>% filter(type == "aaps")
    output <- NULL
    if (nrow(aapsFiles) > 0) {
      output <- parseAndroidAPSFiles(aapsFiles)
    }
    p()
    return(output)
  }, .options = furrr_options(seed = TRUE))
})

cli_text(format(Sys.time(), "%X"))
cli_alert_success("Successfully loaded dataset")
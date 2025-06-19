library(archive)
library(jsonlite)
library(cli)
library(tidyverse)
library(future)
library(future.apply)
library(progressr)

plan(multisession, workers = availableCores())

zipArchive <- "C:/Users/Tebbe/Desktop/SIESTA/n=147_OPENonOH_23.5.2023.zip"
extractDir <- "C:/Users/Tebbe/Desktop/extract"

cli_h1("Loading OpenHumans dataset")

cli_text("Extracting archive...")
if (!file.exists(extractDir)) {
  archive_extract(zipArchive, extractDir)
  cli_alert_success("Successfully extracted archive")
} else {
  cli_alert_warning("Directory already exists. To extract archive again, please delete: {.file {extractDir}}")
}

cli_text("Indexing dataset")
validEntries <- list.files(extractDir, recursive = TRUE, full.names = TRUE)

androidapsPattern <- "^.+\\/([0-9]{8})\\/direct-sharing-396\\/upload-num([0-9]+)-ver([0-9]+)-date([0-9]{8}T[0-9]{6})-appid([0-9a-f]{32}).zip$"
nightscoutPattern <- "^.+\\/([0-9]{8})\\/direct-sharing-31\\/([a-zA-Z]+)_([0-9]{4}-[0-9]{2}-[0-9]{2})?_to_([0-9]{4}-[0-9]{2}-[0-9]{2}).json.gz$"

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

files <- validEntries |>
  map(function(path) {
    result <- parseAAPS(path) %||% parseNS(path)
    if (is.null(result) && !str_ends(path, "/")) {
      cli_alert_warning("Unknown file: {.file {path}}")
    }
    return(result)
  }) |>
  list_rbind()

filesByMember <- files |>
  group_by(projectMemberId) |>
  group_split()

cli_alert_success("Detected {.strong {nrow(files)}} relevant files by {.strong {length(filesByMember)}} members in total")


readAndroidAPSFile <- function(archives, file) {
  df <- bind_rows(lapply(archives, \(archive) {
    contents <- archive(archive)$path
    if (file %in% contents) {
      return(archive_read(archive, file) |> fromJSON())
    }
    return(NULL)
  }))
  return(df)
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
          bgReadings[[length(bgReadings) + 1]] = archive_read(file$path, "BgReadings.json") |>
            fromJSON(flatten = TRUE)
        }
        if ("Treatments.json" %in% innerFiles) {
          treatments[[length(treatments) + 1]] = archive_read(file$path, "Treatments.json") |>
            fromJSON(flatten = TRUE) |>
            select(-starts_with("bolusCalcJson")) #Causes some headache with imcompatbile column data types...
        }
      } else if (file$fileVersion == 2) {
        if ("GlucoseValues.json" %in% innerFiles) {
          glucoseValues[[length(glucoseValues) + 1]] = archive_read(file$path, "GlucoseValues.json") |>
            fromJSON(flatten = TRUE)
        }
        if ("Boluses.json" %in% innerFiles) {
          boluses[[length(boluses) + 1]] = archive_read(file$path, "Boluses.json") |>
            fromJSON(flatten = TRUE)
        }
        if ("Carbs.json" %in% innerFiles) {
          carbs[[length(carbs) + 1]] = archive_read(file$path, "Carbs.json") |>
            fromJSON(flatten = TRUE)
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
    bgReadings = bgReadings |> list_rbind(),
    treatments = treatments |> list_rbind(),
    glucoseValues = glucoseValues |> list_rbind(),
    boluses = boluses |> list_rbind(),
    carbs = carbs |> list_rbind()
  ))
}

ignore <- with_progress({
  p <- progressor(along = filesByMember)
  future_lapply(filesByMember, \(files) {
    projectMemberId <- unique(files$projectMemberId)
    p(sprintf("Processing: %s", projectMemberId))
    aapsFiles <- files |> filter(type == "aaps")
    if (nrow(aapsFiles) > 0) {
      return(parseAndroidAPSFiles(aapsFiles))
    }
    return(NULL)
  }, future.seed = TRUE)
})
library(archive)
library(jsonlite)
library(cli)
library(tidyverse)
library(future)
library(future.apply)
library(progressr)
library(furrr)
library(magrittr)

extractDir <- "C:/Users/Tebbe/Desktop/extract"

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

parseNightscoutFiles <- function(files) {
  files %>%
    filter(collection == "profile") %>%
    pull(path) %>%
    as.list() %>%
    map(~fromJSON(gzfile(.x), flatten = FALSE, simplifyVector = FALSE)) %>%
    flatten() %>%
    map("store") %>%
    map(~map_chr(., "timezone")) %>%
    flatten_chr()
}

timezones <- with_progress({
  p <- progressor(along = filesByMember)
  map(filesByMember, \(files) {
    projectMemberId <- unique(files$projectMemberId)

    nsFiles <- files %>% filter(type == "ns")
    timezones <- NULL
    if (nrow(nsFiles) > 0) {
      timezones <- parseNightscoutFiles(nsFiles) %>% as_tibble() %>% count(value = value, sort = TRUE)
    }

    p()
    list(
      projectMemberId = projectMemberId,
      dataPoints = length(timezones),
      timezones = timezones
    )
  })
})

names(timezones) <- map_chr(timezones, "projectMemberId")
timezones <- map(timezones, "timezones")
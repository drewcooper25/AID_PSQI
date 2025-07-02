library(tidyverse)

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
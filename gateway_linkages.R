library(tidyverse)
library(readxl)

load_gateway_linkages <- function(file) {
  read_excel(file) %>%
    #Transform "NULL" string values in columns to NA
    mutate(
      id = as.integer(id),
      survey_record_id = as.integer(ifelse(survey_record_id == "NULL", NA, survey_record_id)),
      project_member_id = as.character(ifelse(project_member_id == "NULL", NA, project_member_id))
    ) %>%
    #Add missing zeros to project member IDs
    mutate(project_member_id = str_pad(as.character(project_member_id), width = 8, side = "left", pad = "0")) %>%
    select(id, survey_record_id, project_member_id)
}
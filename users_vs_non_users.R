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
library(readxl)

source("nightscout.R")
source("androidaps.R")
source("file_indexing.R")
source("gateway_linkages.R")
source("psqi.R")
source("gv_metrics.R")
source("cleanup_data.R")

redCapDataFile <- "C:/Users/Tebbe/Desktop/SIESTA/OPEN_DATA_2023-07-18_1202.csv"

labelsFile <- "C:/Users/Tebbe/Desktop/SIESTA/psqi_other_labels.xlsx"

labels <- read_excel(labelsFile)

redCap <- read.csv("C:/Users/Tebbe/Desktop/SIESTA/OPEN_DATA_2023-07-18_1202.csv") %>%
  left_join(labels, by = c("record_id" = "record_id")) %>%
  filter(psqi_complete == 2) %>%
  filter(enrollment_type %in% c(0, 1)) %>%
  rowwise() %>%
  ungroup()

psqi <- redCap %>%
  calculatePSQIScores(.) %>%
  mutate(recordId = redCap$record_id, enrollmentType = redCap$enrollment_type)

wilcox.test(globalScore ~ enrollmentType, data = psqi)
by(psqi$globalScore, psqi$enrollmentType, summary)

# Calculate proportion of scores above clinical cutoff (5)
prop.test(sum(psqi$globalScore[psqi$enrollmentType == 0] > 5, na.rm = TRUE),
          sum(!is.na(psqi$globalScore[psqi$enrollmentType == 0])),
          p = 0.5)  # testing against 50% threshold

# One-sample Wilcoxon test against reference value 5
wilcox.test(psqi$globalScore[psqi$enrollmentType == 0], mu = 5, alternative = "two.sided")


# Calculate proportion of scores above clinical cutoff (5)
prop.test(sum(psqi$globalScore[psqi$enrollmentType == 1] > 5, na.rm = TRUE),
          sum(!is.na(psqi$globalScore[psqi$enrollmentType == 1])),
          p = 0.5)  # testing against 50% threshold

# One-sample Wilcoxon test against reference value 5
wilcox.test(psqi$globalScore[psqi$enrollmentType == 1], mu = 5, alternative = "two.sided")

diabetes_table <- with(redCap %>% filter(!is.na(psqi$globalScore)), table(enrollment_type, grepl("Diabetes", labels, ignore.case = TRUE)))

# Perform chi-square test
chi_test <- chisq.test(diabetes_table)
print(chi_test)

# Display contingency table with proportions
prop.table(diabetes_table, margin = 1) # row proportions

# For better visualization, we can also create a data summary
diabetes_summary <- redCap %>%
  group_by(enrollment_type) %>%
  summarise(
    total_count = n(),
    diabetes_count = sum(grepl("Diabetes", labels, ignore.case = TRUE)),
    proportion = diabetes_count / total_count
  )

print(diabetes_summary)

# If we have small counts, we might want to use Fisher's exact test instead
fisher_test <- fisher.test(diabetes_table)
print(fisher_test)
###OPEN Pre-Processing Script 18.04.2025
###Updated script for ISPAD Abstract; focusing more on child participants and widening data range(s)
###Crack open functions using package_name:::function

#Major changes:
# -Changed age buckets to capture greater child, teen and young adult differences
# -Lowered day-data threshold to 1 day (captures more participants)

rm(list = ls()) #clear R environment
#install.packages("BiocManager)
#BiocManager::install("GEOquery")
library("GEOquery")
library("tidyverse")
library("jsonlite")
library("readxl")
library("writexl")
library("cgmquantify")
library("broom")
library("npmv")

################################################################################

#Import participant linkages; inject leading zeros back in to project_member_ids and remove record_id NULLs:
links <- read_excel("/Users/drew.cooper/OPENonOH/Participants_FULL_BIGOPEN+OPENLight_2021.07.13.xlsx")
links$project_member_id <- str_pad(links$project_member_id, width = 8, side = "left", pad = "0")
links <- subset(links, project_member_id != "0000NULL") #remove NULL project member IDs
links <- links[!is.na(links$survey_record_id) & links$survey_record_id != "NULL", ] #remove more NAs/NULLs
links <- subset(links, select = -followup_survey_record_id) #remove ID you're not using

#Import REDCap data and pair down to only those w/ valid project_member_ids:
REDCap <- read.csv("/Users/drew.cooper/REDCap/OPEN_DATA_2023-07-18_1202.csv")
REDCap <- REDCap[REDCap$record_id %in% links$survey_record_id, ]

################################################################################

#Check for factored variables!!!

#Enrollment Type Conversion:
REDCap$enrollment_type <- factor(REDCap$enrollment_type, levels = c(0,1,2,3), labels = c("Adult (AID)", "Adult (non-user)", "Child (AID)", "Child (non-user)"))

#Combined Age variable:
OPEN_end <- as.POSIXct("2020-12-01", format = "%Y-%m-%d")
Adult_Age <- as.POSIXct(paste(REDCap$year_of_birth, REDCap$month_of_birth, "01", sep = "-"), TZ = "", format = "%Y-%m-%d")
Adult_Age <- as.numeric(difftime(OPEN_end, Adult_Age, units = "days")) / 365.25
Child_Age <- as.POSIXct(paste(REDCap$year_of_birth_child, REDCap$month_of_birth_child, "01", sep = "-"), TZ = "", format = "%Y-%m-%d")
Child_Age <- as.numeric(difftime(OPEN_end, Child_Age, units = "days")) / 365.25
REDCap$Age <- ifelse(is.na(Adult_Age), Child_Age, Adult_Age)
REDCap$Age <- as.integer(REDCap$Age)

#Age buckets:
REDCap$Age_group <- ifelse(REDCap$Age <= 11, "0-11", ifelse(REDCap$Age <= 18, "11-18", ifelse(REDCap$Age <= 25, "18-25", ifelse(REDCap$Age <= 40, "25-40", ifelse(REDCap$Age <= 55, "40-55", ifelse(REDCap$Age > 55, "55+", REDCap$Age))))))

#Combined Sex variable:
REDCap$Sex <- ifelse(is.na(REDCap$gender), REDCap$gender_child, REDCap$gender)
REDCap$Sex <- factor(REDCap$Sex, levels = c(1,2), labels = c("Female", "Male"))

#Combined type of diabetes:
REDCap$Type_Diabetes <- dplyr::coalesce(REDCap$type_of_diabetes, REDCap$type_of_diabetes_child, REDCap$diynot_diab_type, REDCap$diynot_diab_type_parents)

#Duration of Diabetes:
REDCap$Duration_Diabetes_adults_aid <- as.POSIXct(paste(REDCap$year_of_diagnosis, REDCap$month_of_diagnosis, "01", sep = "-"), TZ = "", format = "%Y-%m-%d")
REDCap$Duration_Diabetes_adults_aid <- ifelse(is.na(REDCap$Duration_Diabetes_adults_aid), as.POSIXct(paste(REDCap$year_of_diagnosis_2, "01", "01", sep = "-"), TZ = "", format = "%Y-%m-%d"), REDCap$Duration_Diabetes_adults_aid)

REDCap$Duration_Diabetes_adults_non <- as.POSIXct(paste(REDCap$diynot_diagnosis_date_yyyy, REDCap$diynot_diagnosis_date_mm, "01", sep = "-"), TZ = "", format = "%Y-%m-%d")
REDCap$Duration_Diabetes_adults_non <- ifelse(is.na(REDCap$Duration_Diabetes_adults_non), as.POSIXct(paste(REDCap$diynot_diagnosis_date_yyyy_2, "01", "01", sep = "-"), TZ = "", format = "%Y-%m-%d"), REDCap$Duration_Diabetes_adults_non)

REDCap$Duration_Diabetes_child_aid <- as.POSIXct(paste(REDCap$year_of_diagnosis_child, REDCap$month_of_diagnosis_child, "01", sep = "-"), TZ = "", format = "%Y-%m-%d")
REDCap$Duration_Diabetes_child_aid <- ifelse(is.na(REDCap$Duration_Diabetes_child_aid), as.POSIXct(paste(REDCap$year_of_diagnosis_2_child, "01", "01", sep = "-"), TZ = "", format = "%Y-%m-%d"), REDCap$Duration_Diabetes_child_aid)

REDCap$Duration_Diabetes_child_non <- as.POSIXct(paste(REDCap$diynot_diagnosis_date_yyyy_parents, REDCap$diynot_diagnosis_date_mm_parents, "01", sep = "-"), TZ = "", format = "%Y-%m-%d")
REDCap$Duration_Diabetes_child_non <- ifelse(is.na(REDCap$Duration_Diabetes_child_non), as.POSIXct(paste(REDCap$diynot_diagnosis_date_yyyy_2_parents, "01", "01", sep = "-"), TZ = "", format = "%Y-%m-%d"), REDCap$Duration_Diabetes_child_non)

REDCap$Duration_Diabetes <- dplyr::coalesce(REDCap$Duration_Diabetes_adults_aid, REDCap$Duration_Diabetes_adults_non, REDCap$Duration_Diabetes_child_aid, REDCap$Duration_Diabetes_child_non)
REDCap$Duration_Diabetes <- as.numeric(difftime(OPEN_end, REDCap$Duration_Diabetes, units = "days")) / 365.25

#Combined type of DIYAPS:
mapping <- c(
  "type_of_diyaps___1" = "AAPS",
  "type_of_diyaps___2" = "Loop",
  "type_of_diyaps___3" = "OpenAPS",
  "type_of_diyaps___77777" = "Other",
  "type_of_diyaps___88888" = "I don't know",
  "type_of_diyaps___88889" = "I'd rather not say",
  "type_of_diyaps_child___1" = "AAPS",
  "type_of_diyaps_child___2" = "Loop",
  "type_of_diyaps_child___3" = "OpenAPS",
  "type_of_diyaps_child___77777" = "Other",
  "type_of_diyaps_child___88888" = "I don't know",
  "type_of_diyaps_child___88889" = "I'd rather not say",
  "diynot_dia_management___1" = "Commercial hybrid closed-loop system (e.g. Medtronic 640G/670G, Tandem, Insulet)",
  "diynot_dia_management___2" = "CGM - real-time CGM (rtCGM) or intermittent scanning CGM (isCGM Libre)",
  "diynot_dia_management___3" = "Manual blood  checks (SMBG)",
  "diynot_dia_management___4" = "Insulin pump (not senor-augmented)",
  "diynot_dia_management___5" = "Multiple daily injections",
  "diynot_dia_management___6" = "Inhaled insulin (e.g. Afrezza®)",
  "diynot_dia_management___7" = "SGLT2-Inhibitors (e.g. Forxiga®, Jardiance®)",
  "diynot_dia_management___77777" = "Other",
  "diynot_dia_management___88888" = "I don't know",
  "diynot_dia_management___99999" = "I'd rather not say",
  "diynot_dia_management_parents___1" = "Commercial hybrid closed-loop system (e.g. Medtronic 640G/670G, Tandem, Insulet)",
  "diynot_dia_management_parents___2" = "CGM - real-time CGM (rtCGM) or intermittent scanning CGM (isCGM Libre)",
  "diynot_dia_management_parents___3" = "Manual blood  checks (SMBG)",
  "diynot_dia_management_parents___4" = "Insulin pump (not senor-augmented)",
  "diynot_dia_management_parents___5" = "Multiple daily injections",
  "diynot_dia_management_parents___6" = "Inhaled insulin (e.g. Afrezza®)",
  "diynot_dia_management_parents___7" = "SGLT2-Inhibitors (e.g. Forxiga®, Jardiance®)",
  "diynot_dia_management_parents___77777" = "Other",
  "diynot_dia_management_parents___88888" = "I don't know",
  "diynot_dia_management_parents___99999" = "I'd rather not say"
)

for (col in names(mapping)) {
  REDCap[[col]] <- ifelse(REDCap[[col]] == 1, mapping[col], NA)
}

REDCap$Type_System <- apply(REDCap[, names(mapping)], 1, function(row) {
  paste(na.omit(row), collapse = ", ")
})

REDCap$Type_System[REDCap$Type_System == ""] <- NA

#Duration of Looping:
REDCap$Duration_Loop_adult <- as.POSIXct(paste(REDCap$year_diyaps_commencement, REDCap$month_diyaps_commencement, "01", sep = "-"), TZ = "", format = "%Y-%m-%d")
REDCap$Duration_Loop_adult <- ifelse(is.na(REDCap$Duration_Loop_adult), as.POSIXct(paste(REDCap$year_diyaps_commencement_2, "01", "01", sep = "-"), TZ = "", format = "%Y-%m-%d"), REDCap$Duration_Loop_adult)

REDCap$Duration_Loop_child <- as.POSIXct(paste(REDCap$year_diyaps_commencement_child, REDCap$month_diyaps_commencement_child, "01", sep = "-"), TZ = "", format = "%Y-%m-%d")
REDCap$Duration_Loop_child <- ifelse(is.na(REDCap$Duration_Loop_child), as.POSIXct(paste(REDCap$year_diyaps_commencement_2_child, "01", "01", sep = "-"), TZ = "", format = "%Y-%m-%d"), REDCap$Duration_Loop_child)

REDCap$Duration_Loop <- dplyr::coalesce(REDCap$Duration_Loop_adult, REDCap$Duration_Loop_child)
REDCap$Duration_Loop <- as.numeric(difftime(OPEN_end, REDCap$Duration_Loop, units = "days")) / 365.25

#Combined DIYAPS features:
mapping <- c(
  "diyaps_features_aaps___0" = "None",
  "diyaps_features_aaps___1" = "Temporary targets",
  "diyaps_features_aaps___2" = "Remote follower option",
  "diyaps_features_aaps___4" = "Profile switches",
  "diyaps_features_aaps___5" = "Super Micro Bolus (SMB)",
  "diyaps_features_aaps___8" = "Unannounced Meals (UAM)",
  "diyaps_features_aaps___9" = "Automation",
  "diyaps_features_aaps___11" = "Autotune (manual)",
  "diyaps_features_aaps___12" = "Superbolus",
  "diyaps_features_aaps___13" = "Autosens",
  "diyaps_features_aaps___88888" = "I don't know",
  "diyaps_features_aaps___99999" = "I'd rather not say",
  "diyaps_features_openaps___0" = "None",
  "diyaps_features_openaps___1" = "Temporary targets",
  "diyaps_features_openaps___2" = "Remote follower option",
  "diyaps_features_openaps___4" = "Profile switches",
  "diyaps_features_openaps___5" = "Super Micro Bolus (SMB)",
  "diyaps_features_openaps___8" = "Unannounced Meals (UAM)",
  "diyaps_features_openaps___9" = "Automation",
  "diyaps_features_openaps___10" = "Autotune (automatic)",
  "diyaps_features_openaps___11" = "Autotune (manual)",
  "diyaps_features_openaps___13" = "Autosens",
  "diyaps_features_openaps___88888" = "I don't know",
  "diyaps_features_openaps___99999" = "I'd rather not say",
  "diyaps_features_loop___0" = "None",
  "diyaps_features_loop___2" = "Remote follower option",
  "diyaps_features_loop___3" = "Manual overrides",
  "diyaps_features_loop___6" = "Autoboli",
  "diyaps_features_loop___7" = "Automated microboli",
  "diyaps_features_loop___11" = "Autotune (manual)",
  "diyaps_features_loop___88888" = "I don't know",
  "diyaps_features_loop___99999" = "I'd rather not say",
  
  "diyaps_features_child_aaps___0" = "None",
  "diyaps_features_child_aaps___1" = "Temporary targets",
  "diyaps_features_child_aaps___2" = "Remote follower option",
  "diyaps_features_child_aaps___4" = "Profile switches",
  "diyaps_features_child_aaps___5" = "Super Micro Bolus (SMB)",
  "diyaps_features_child_aaps___8" = "Unannounced Meals (UAM)",
  "diyaps_features_child_aaps___9" = "Automation",
  "diyaps_features_child_aaps___11" = "Autotune (manual)",
  "diyaps_features_child_aaps___12" = "Superbolus",
  "diyaps_features_child_aaps___13" = "Autosens",
  "diyaps_features_child_aaps___88888" = "I don't know",
  "diyaps_features_child_aaps___99999" = "I'd rather not say",
  "diyaps_features_child_openaps___0" = "None",
  "diyaps_features_child_openaps___1" = "Temporary targets",
  "diyaps_features_child_openaps___2" = "Remote follower option",
  "diyaps_features_child_openaps___4" = "Profile switches",
  "diyaps_features_child_openaps___5" = "Super Micro Bolus (SMB)",
  "diyaps_features_child_openaps___8" = "Unannounced Meals (UAM)",
  "diyaps_features_child_openaps___9" = "Automation",
  "diyaps_features_child_openaps___10" = "Autotune (automatic)",
  "diyaps_features_child_openaps___11" = "Autotune (manual)",
  "diyaps_features_child_openaps___13" = "Autosens",
  "diyaps_features_child_openaps___88888" = "I don't know",
  "diyaps_features_child_openaps___99999" = "I'd rather not say",
  "diyaps_features_child_loop___0" = "None",
  "diyaps_features_child_loop___2" = "Remote follower option",
  "diyaps_features_child_loop___3" = "Manual overrides",
  "diyaps_features_child_loop___6" = "Autoboli",
  "diyaps_features_child_loop___7" = "Automated microboli",
  "diyaps_features_child_loop___11" = "Autotune (manual)",
  "diyaps_features_child_loop___88888" = "I don't know",
  "diyaps_features_child_loop___99999" = "I'd rather not say"
)

for (col in names(mapping)) {
  REDCap[[col]] <- ifelse(REDCap[[col]] == 1, mapping[col], NA)
}

REDCap$Features_DIYAPS <- apply(REDCap[, names(mapping)], 1, function(row) {
  paste(na.omit(row), collapse = ", ")
})

REDCap$Features_DIYAPS[REDCap$Features_DIYAPS == ""] <- NA

#Combined HbA1c variable:
REDCap$Reported_HbA1c <- ifelse(is.na(REDCap$most_recent_hba1c), REDCap$most_recent_hba1c_child, REDCap$most_recent_hba1c)

REDCap$Reported_HbA1c <- dplyr::coalesce(REDCap$most_recent_hba1c, REDCap$most_recent_hba1c_child, REDCap$diynot_dia_management_hba1c_dcct, REDCap$diynot_dia_management_hba1c_dcct_parents)

#Insulin: #should this be a factor???
REDCap$insulin_absorption_model <- factor(REDCap$insulin_absorption_model, levels = c(1,2,3,4,6,7,8,77777,88888,99999), labels = c("Rapid-acting/Rapid-acting Oref/Rapid-acting adults", "Rapid-acting children (Loop/iOS)", "Ultra-rapid/Ultra-rapid Oref/Fiasp", "Bilinear (OpenAPS)", "Custom Peak Time (OpenAPS)", "Free-Peak Oref (AndroidAPS)", "Walsh (Loop/iOS)", "Other", "I don't know", "I'd rather not say"))

REDCap$insulin_absorption_model_child <- factor(REDCap$insulin_absorption_model_child, levels = c(1,2,3,4,6,7,8,77777,88888,99999), labels = c("Rapid-acting/Rapid-acting Oref/Rapid-acting adults", "Rapid-acting children (Loop/iOS)", "Ultra-rapid/Ultra-rapid Oref/Fiasp", "Bilinear (OpenAPS)", "Custom Peak Time (OpenAPS)", "Free-Peak Oref (AndroidAPS)", "Walsh (Loop/iOS)", "Other", "I don't know", "I'd rather not say"))

REDCap$Insulin_Absorption <- dplyr::coalesce(REDCap$insulin_absorption_model, REDCap$insulin_absorption_model_child)

################################################################################

#Sleep Duration:
PSQI_DURAT <- ifelse(REDCap$psqi_4 >= 7, 0, ifelse(REDCap$psqi_4 >= 6, 1, ifelse(REDCap$psqi_4 >= 5, 2, ifelse(REDCap$psqi_4 < 5, 3, REDCap$psqi_4))))
#Sleep Disturbances:
REDCap$psqi_5j <- ifelse(is.null(REDCap$psqi_5j), 0)
psqi_5sum <- num(REDCap$psqi_5b + REDCap$psqi_5c + REDCap$psqi_5d + REDCap$psqi_5e + REDCap$psqi_5f + REDCap$psqi_5g + REDCap$psqi_5h + REDCap$psqi_5i + REDCap$psqi_5j)
PSQI_DISTB <- ifelse(psqi_5sum <= 0, 0, ifelse(psqi_5sum <= 9, 1, ifelse(psqi_5sum <= 18, 2, ifelse(psqi_5sum > 18, 3, psqi_5sum))))
#Sleep Latency:
psqi_2new <- ifelse(REDCap$psqi_2 <= 15, 0, ifelse(REDCap$psqi_2 <= 30, 1, ifelse(REDCap$psqi_2 <= 60, 2, ifelse(REDCap$psqi_2 > 60, 3, REDCap$psqi_2))))
psqi_5a_2new <- num(REDCap$psqi_5a + psqi_2new)
PSQI_LATEN <- ifelse(psqi_5a_2new <= 0, 0, ifelse(psqi_5a_2new <= 2, 1, ifelse(psqi_5a_2new <= 4, 2, ifelse(psqi_5a_2new <= 6, 3, ifelse(psqi_5a_2new > 6, 3, psqi_5a_2new)))))
#Days Dysfunctional due to Sleepiness:
psqi_8_9 <- num(REDCap$psqi_8 + REDCap$psqi_9)
PSQI_DAYSDYS <- ifelse(psqi_8_9 <= 0, 0, ifelse(psqi_8_9 <= 2, 1, ifelse(psqi_8_9 <= 4, 2, ifelse(psqi_8_9 <= 6, 3, ifelse(psqi_8_9 > 6, 3, psqi_8_9)))))
#Habitual Sleep Efficiency:
newtime1 <- strptime(REDCap$psqi_1, "%H:%M")
newtime3 <- strptime(REDCap$psqi_3, "%H:%M")
Diffhour <- abs(as.numeric(newtime1 - newtime3))
newtib <- ifelse(Diffhour > 24, (Diffhour - 24), ifelse(Diffhour - 24, (Diffhour)))
tmphse <- (REDCap$psqi_4/newtib) * 100
PSQI_HSE <- ifelse(tmphse >= 85, 0, ifelse(tmphse >= 75, 1, ifelse(tmphse >= 65, 2, ifelse(tmphse < 65, 3, tmphse))))
#Overall Sleep Quality:
PSQI_SLPQUAL <- REDCap$psqi_6
#Need Meds to Sleep:
PSQI_MEDS <- REDCap$psqi_7
#Total:
PSQI_Total <- PSQI_DURAT + PSQI_DISTB + PSQI_LATEN + PSQI_DAYSDYS + PSQI_HSE + PSQI_SLPQUAL + PSQI_MEDS
#Timestamp:
PSQI_Completed <- as.POSIXct(paste(REDCap$psqi_timestamp), TZ = "", format = "%Y-%m-%d")

################################################################################

#Fear of Hypos
#Start by redefining all scores to true 0-4 scale instead of false 1-5 scale...
REDCap$fear_of_hypo_1 <- ifelse(REDCap$fear_of_hypo_1 == 1, 0, ifelse(REDCap$fear_of_hypo_1 == 2, 1, ifelse(REDCap$fear_of_hypo_1 == 3, 2, ifelse(REDCap$fear_of_hypo_1 == 4, 3, ifelse(REDCap$fear_of_hypo_1 == 5, 4, REDCap$fear_of_hypo_1)))))

REDCap$fear_of_hypo_2 <- ifelse(REDCap$fear_of_hypo_2 == 1, 0, ifelse(REDCap$fear_of_hypo_2 == 2, 1, ifelse(REDCap$fear_of_hypo_2 == 3, 2, ifelse(REDCap$fear_of_hypo_2 == 4, 3, ifelse(REDCap$fear_of_hypo_2 == 5, 4, REDCap$fear_of_hypo_2)))))

REDCap$fear_of_hypo_3 <- ifelse(REDCap$fear_of_hypo_3 == 1, 0, ifelse(REDCap$fear_of_hypo_3 == 2, 1, ifelse(REDCap$fear_of_hypo_3 == 3, 2, ifelse(REDCap$fear_of_hypo_3 == 4, 3, ifelse(REDCap$fear_of_hypo_3 == 5, 4, REDCap$fear_of_hypo_3)))))

REDCap$fear_of_hypo_4 <- ifelse(REDCap$fear_of_hypo_4 == 1, 0, ifelse(REDCap$fear_of_hypo_4 == 2, 1, ifelse(REDCap$fear_of_hypo_4 == 3, 2, ifelse(REDCap$fear_of_hypo_4 == 4, 3, ifelse(REDCap$fear_of_hypo_4 == 5, 4, REDCap$fear_of_hypo_4)))))

REDCap$fear_of_hypo_5 <- ifelse(REDCap$fear_of_hypo_5 == 1, 0, ifelse(REDCap$fear_of_hypo_5 == 2, 1, ifelse(REDCap$fear_of_hypo_5 == 3, 2, ifelse(REDCap$fear_of_hypo_5 == 4, 3, ifelse(REDCap$fear_of_hypo_5 == 5, 4, REDCap$fear_of_hypo_5)))))

REDCap$hypo_worry_1 <- ifelse(REDCap$hypo_worry_1 == 1, 0, ifelse(REDCap$hypo_worry_1 == 2, 1, ifelse(REDCap$hypo_worry_1 == 3, 2, ifelse(REDCap$hypo_worry_1 == 4, 3, ifelse(REDCap$hypo_worry_1 == 5, 4, REDCap$hypo_worry_1)))))

REDCap$hypo_worry_2 <- ifelse(REDCap$hypo_worry_2 == 1, 0, ifelse(REDCap$hypo_worry_2 == 2, 1, ifelse(REDCap$hypo_worry_2 == 3, 2, ifelse(REDCap$hypo_worry_2 == 4, 3, ifelse(REDCap$hypo_worry_2 == 5, 4, REDCap$hypo_worry_2)))))

REDCap$hypo_worry_3 <- ifelse(REDCap$hypo_worry_3 == 1, 0, ifelse(REDCap$hypo_worry_3 == 2, 1, ifelse(REDCap$hypo_worry_3 == 3, 2, ifelse(REDCap$hypo_worry_3 == 4, 3, ifelse(REDCap$hypo_worry_3 == 5, 4, REDCap$hypo_worry_3)))))

REDCap$hypo_worry_4 <- ifelse(REDCap$hypo_worry_4 == 1, 0, ifelse(REDCap$hypo_worry_4 == 2, 1, ifelse(REDCap$hypo_worry_4 == 3, 2, ifelse(REDCap$hypo_worry_4 == 4, 3, ifelse(REDCap$hypo_worry_4 == 5, 4, REDCap$hypo_worry_4)))))

REDCap$hypo_worry_5 <- ifelse(REDCap$hypo_worry_5 == 1, 0, ifelse(REDCap$hypo_worry_5 == 2, 1, ifelse(REDCap$hypo_worry_5 == 3, 2, ifelse(REDCap$hypo_worry_5 == 4, 3, ifelse(REDCap$hypo_worry_5 == 5, 4, REDCap$hypo_worry_5)))))

REDCap$hypo_worry_6 <- ifelse(REDCap$hypo_worry_6 == 1, 0, ifelse(REDCap$hypo_worry_6 == 2, 1, ifelse(REDCap$hypo_worry_6 == 3, 2, ifelse(REDCap$hypo_worry_6 == 4, 3, ifelse(REDCap$hypo_worry_6 == 5, 4, REDCap$hypo_worry_6)))))

REDCap$Fear_Hypo <- REDCap$fear_of_hypo_1 + REDCap$fear_of_hypo_2 + REDCap$fear_of_hypo_3 + REDCap$fear_of_hypo_4 + REDCap$fear_of_hypo_5 + REDCap$hypo_worry_1 + REDCap$hypo_worry_2 + REDCap$hypo_worry_3 + REDCap$hypo_worry_4 + REDCap$hypo_worry_5 + REDCap$hypo_worry_6

################################################################################

#Load elements into data frame:
REDCap_results <- data.frame(
  "survey_record_id" = REDCap$record_id,
  "project_member_id" = links$project_member_id,
  "Enrollment_Type" = REDCap$enrollment_type,
  "Age_years" = REDCap$Age,
  "Age_group" = REDCap$Age_group,
  "Sex" = REDCap$Sex,
  "Type_Diabetes" = REDCap$Type_Diabetes,
  "Duration_Diabetes_years" = REDCap$Duration_Diabetes,
  "Type_System" = REDCap$Type_System,
  "Duration_Looping_years" = REDCap$Duration_Loop,
  "Features_AID" = REDCap$Features_DIYAPS,
  "Reported_HbA1c_mg/dL" = REDCap$Reported_HbA1c,
  "PSQI_Score" = as.numeric(PSQI_Total),
  "HFS-II_Short_Scale" = as.numeric(REDCap$Fear_Hypo),
  "Reason_Sleep_Disturbance" = REDCap$psqi_5other,
  "Insulin_Absorption" = REDCap$Insulin_Absorption,
  "PSQI_Timestamp" = PSQI_Completed,
  "Sleep_Latency" = PSQI_LATEN,
  "Sleep_Disturbance" = PSQI_DISTB,
  "Sleep_Efficiency" = PSQI_HSE
)

################################################################################

dir <- setwd("/Users/drew.cooper/OPENonOH/n=147_OPENonOH_23.08.2024")

#List all .zip files and put into data frame:
dirs <- list.files(path = dir, pattern = ".gz|.zip", recursive = TRUE, include.dirs = FALSE)
dirs <- dirs[!grepl("direct-sharing-133", dirs)] #remove all data selfie files...

#Unzip .gz and .zip to access all files within:
for (x in dirs) {
  # check if .zip file already exists; unzip .zip
  dir_name <- gsub("[.]gz$", "", x)
  if(!file.exists(dir_name)) {
    GEOquery::gunzip(x, destname = dir_name, remove = FALSE)
    print(paste("File", x, "has been unzipped!"))
  }
  dir_name <- gsub("[.]zip$", "", x)
  if(!file.exists(dir_name)) {
    unzip(x, overwrite = FALSE, exdir = dir_name)
    print(paste("File", x, "has been unzipped!"))
  }
}

################################################################################

#List all entries.json, BgReadings.json and GlucoseValues.json files and reorder by filename:
sensorGluFiles <- list.files(path = dir, pattern = "entries|BgReadings|GlucoseValues", recursive = TRUE, include.dirs = FALSE)
sensorGluFiles <- sensorGluFiles[!grepl(".gz|.zip", sensorGluFiles)]
project_member_id <- as.numeric(substr(sensorGluFiles, 1, 8))
num_term <- as.numeric(gsub("\\D", "", gsub(".+num(\\d+)-ver1", "\\1", sensorGluFiles)))
ver_term <- as.numeric(gsub("\\D", "", gsub(".+num(\\d+)-ver1", "\\1", sensorGluFiles)))
sensorGluFiles <- sensorGluFiles[order(project_member_id, num_term, ver_term, na.last = NA)]

#Paring down participants prior to data conversion:
sensorGluFiles <- as.data.frame(matrix(sensorGluFiles, ncol = 1, byrow = TRUE)) #create data frame
names(sensorGluFiles)[names(sensorGluFiles) == "V1"] <- "file_path"
sensorGluFiles$project_member_id <- str_pad(project_member_id, width = 8, side = "left", pad = "0") #add padding zeros
sensorGluFiles <- cbind(sensorGluFiles, num_term, ver_term) #add in other path terms
sensorGluFiles <- semi_join(sensorGluFiles, REDCap_results, by = "project_member_id") #filter to PSQI respondents

################################################################################

#Make empty data frame list:
df_list <- list()

#Convert .json to dataframes; fill empty dataframe list:
for (x in sensorGluFiles$file_path) {
  tryCatch({
    data <- jsonlite::fromJSON(x)
    file_name <- gsub("", "", x)
    df_list[[file_name]] <- data
    
    print(data)
    
  }, error = function(e) {
    cat("Error parsing", x, ":", conditionMessage(e), "\n")
    next
  })
}

################################################################################

#Setup for combining data frames:
proj_member_id_seq <- unique(sensorGluFiles$project_member_id)
sgv_terms <- c("entries", "BgReadings", "GlucoseValues")

################################################################################

concat_list <- list()

#Concatenate BgReadings and GlucoseValues while separating entries files:
for (seq in proj_member_id_seq) {
  # Loop through each file type
  for (term in sgv_terms) {
    # Extract data frames corresponding to the current participant ID and file type
    subset_list <- df_list[sapply(names(df_list), function(name) grepl(seq, name) & grepl(term, name))]
    
    # Check if there are multiple files of the same type
    if (length(subset_list) > 1) {
      # If yes, combine BgReadings and GlucoseValues files and add to psqi_list
      if (term %in% c("BgReadings", "GlucoseValues")) {
        concat_list[[paste0(seq, "-", term)]] <- bind_rows(subset_list)
        cat("Combining", seq, "AAPS files.\n")
      } else {
        # If it's an entries file, add each file separately to psqi_list
        for (i in seq_along(subset_list)) {
          concat_list[[paste0(seq, "-entries", i)]] <- subset_list[[i]]
        }
        cat("Storing", seq, "NS files.\n")
      }
    } else if (length(subset_list) == 1) {
      # If there's only one file of the type, add it to psqi_list
      concat_list[[paste0(seq, "-", term)]] <- subset_list[[1]]
      cat("Storing", seq, "NS file.\n")
    }
  }
}

#Remove the single null data frame causing problems:
concat_list <- concat_list[!names(concat_list) %in% "46616418-entries"]

################################################################################

#Load psqi_list as df_list (this way we get pre-processing all out of the way and consolidate vectors)
psqi_list <- concat_list

#Create function to remove duplicate NaN variables:
remove_extra_vars <- function(df) {
  char_indices <- which(names(df) %in% c("Date", "glucose") & sapply(df, is.character))
  if (length(char_indices) > 0) {
    df <- df[, -char_indices, drop = FALSE]
  }
  return(df)
}

#Clean list to only Date and glucose variables:
for (i in 1:length(psqi_list)) {
  #Check for empty data frames (important!):
  if (NROW(psqi_list[[i]]) == 0) {
    cat("Data frame",  names(psqi_list)[i], "is empty.\n")
  } else {
    #Standardise variable names:
    names(psqi_list[[i]])[names(psqi_list[[i]]) == "date"] <- "Date"
    names(psqi_list[[i]])[names(psqi_list[[i]]) == "timestamp"] <- "Date"
    names(psqi_list[[i]])[names(psqi_list[[i]]) == "value"] <- "glucose"
    names(psqi_list[[i]])[names(psqi_list[[i]]) == "sgv"] <- "glucose"
    #Apply remove NaN variable(s) function and parse down to "Date" and "glucose" variables:
    psqi_list[[i]] <- remove_extra_vars(psqi_list[[i]])
    psqi_list[[i]] <- subset(psqi_list[[i]], select = c(Date, glucose))
    cat("Data frame", names(psqi_list)[i], "now only contains Date and glucose data.\n")
  }
}

#Format variable types, restrict to PSQI time frame and standardize scales:
for (i in 1:length(psqi_list)) {
  if (NROW(psqi_list[[i]]) == 0) {
    cat("Data frame",  names(psqi_list)[i], "is empty.\n")
  } else {
    #Unix to POSIX:
    psqi_list[[i]]$Date <- as.POSIXct(psqi_list[[i]]$Date/1000, TZ = "", format = "%Y-%m-%d %H:%M:%S")
    #Extract data frame names and match to project member IDs:
    df_name <- names(psqi_list)[i]
    identifier <- sub("-.*", "", df_name)
    match_idx <- which(REDCap_results$project_member_id == identifier)
    #Loop to filter by date range:
    if (length(match_idx) > 0) {
      timestamp <- REDCap_results$PSQI_Timestamp[match_idx]
      start_date <- timestamp - (28*24*60*60)
      end_date <- timestamp
      psqi_list[[i]] <- subset(psqi_list[[i]], Date >= start_date & Date <= end_date)
    }
    #Convert glucose to numeric:
    psqi_list[[i]]$glucose <- as.numeric(psqi_list[[i]]$glucose)
    #Subset glucose range to physiological levels (in mg/dL):
    psqi_list[[i]] <- subset(psqi_list[[i]], glucose > 40 & glucose < 1000)
    cat("Data frame", names(psqi_list)[i], "has filtered Date and glucose data.\n")
  }
}

#Automated removal of data frames below a day data threshold:
for (i in length(psqi_list):1) {
  if (NROW(psqi_list[[i]]) == 0) {
    psqi_list[[i]] <- NULL
    cat(names(psqi_list)[i], "is empty and has been EXPELLED.\n")
  } else {
    min_date <- min(psqi_list[[i]]$Date)
    max_date <- max(psqi_list[[i]]$Date)
    num_days <- as.numeric(difftime(max_date, min_date, units = "days"))
    if (num_days < 1) { #change this number if you want to be more or less ruthless.
      psqi_list[[i]] <- NULL
      cat(names(psqi_list)[i], "contains less than 1 day(s) worth of data and has been EXPELLED.\n")
    } else {
      cat(names(psqi_list)[i], "contains", num_days, "days worth of data.\n")
    }
  }
}

#Delete duplicate data frames based on which has less data:
identifiers <- sapply(names(psqi_list), function(x) sub("-.*", "", x))
num_rows <- sapply(psqi_list, NROW)
max_rows_index <- vector("list", length(unique(identifiers)))

for (i in seq_along(unique(identifiers))) {
  identifier <- unique(identifiers)[i]
  idx <- which(identifiers == identifier)
  max_idx <- which.max(num_rows[idx])
  max_rows_index[[i]] <- idx[max_idx]
}

max_rows_index <- unlist(max_rows_index)
psqi_list <- psqi_list[max_rows_index]

#Explicitly remove participants by ID and why (IDed by lower facet graph; little bit backwards but okay):
participants_to_remove <- c(
  "00752599-entries", #non-user of AID.
  "00977838-entries", #missing PSQI score.
  "09635199-BgReadings", #over 3 million data points; something is off...
  "13577203-entries", #type 4 diabetic (Late Autoimmune Diabetes in Adults).
  "19099241-BgReadings", #missing age.
  "27718918-BgReadings", #missing age.
  "32048572-BgReadings", #missing PSQI score.
  "34208593-BgReadings", #almost 3 million data points; again, weird...
  #"37159654-entries", #missing central chunk of data.
  #"54943175-entries", #missing later chunk of data.
  "81133880-entries13", #type 77777 diabetic (Other; unspecified).
  "94155197-entries", #non-user of AID.
  "99478201-entries" #type 2 diabetic.
)
psqi_list <- psqi_list[setdiff(names(psqi_list), participants_to_remove)]

################################################################################

#GV FUNCTIONS (i.e. I cannot believe cgmquantify got published):

#total_time:
total_time <- function(df) {
  df <- df[order(df$Date), ]
  total_time <- sum(c(0, diff(as.numeric(df$Date)))) / 60 / 60 / 24 #convert to days
}

#TIR:
TIR <- function(df) {
  #Define the glucose range
  lower <- 70
  upper <- 180
  
  #Sort Date data into chronological order (just in case)
  df <- df[order(df$Date), ]
  
  #Calculate time difference between consecutive readings
  df$time_diff <- c(0, diff(as.numeric(df$Date))) #time difference in seconds
  
  #Subset data to only TIR values and calculate total time by summing time differences
  TIR = sum(df$time_diff[df$glucose >= lower & df$glucose <= upper])
  
  TIR = TIR / 60 / 60 / 24 #convert to days
}

#TOR:
TOR <- function(df) {
  lower <- 70
  upper <- 180
  
  df <- df[order(df$Date), ]
  df$time_diff <- c(0, diff(as.numeric(df$Date)))
  
  TOR = sum(df$time_diff[df$glucose <= lower | df$glucose >= upper])
  TOR = TOR / 60 / 60 / 24
}

#TBR:
TBR <- function(df) {
  lower <- 70
  upper <- 180
  
  df <- df[order(df$Date), ]
  df$time_diff <- c(0, diff(as.numeric(df$Date)))
  
  TBR = sum(df$time_diff[df$glucose <= lower])
  TBR = TBR / 60 / 60 / 24
}

#TAR:
TAR <- function(df) {
  lower <- 70
  upper <- 180
  
  df <- df[order(df$Date), ]
  df$time_diff <- c(0, diff(as.numeric(df$Date)))
  
  TAR = sum(df$time_diff[df$glucose >= upper])
  TAR = TAR / 60 / 60 / 24
}

#PIR:
PIR <- function(df) {
  df <- df[order(df$Date), ]
  PIR <- TIR(df) / total_time(df)
}

#POR:
POR <- function(df) {
  df <- df[order(df$Date), ]
  POR <- TOR(df) / total_time(df)
}

#PBR:
PBR <- function(df) {
  df <- df[order(df$Date), ]
  PBR <- TBR(df) / total_time(df)
}

#PAR:
PAR <- function(df) {
  df <- df[order(df$Date), ]
  PAR <- TAR(df) / total_time(df)
}

#CV:
CV <- function(df) {
  mean_glu <- mean(df$glucose, na.rm = TRUE)
  sd_glu <- sd(df$glucose, na.rm = TRUE)
  CV <- (sd_glu / mean_glu) * 100
}

#TITR:
TITR <- function(df) {
  lower <- 70
  upper <- 140
  
  df <- df[order(df$Date), ]
  df$time_diff <- c(0, diff(as.numeric(df$Date)))
  
  TITR = sum(df$time_diff[df$glucose >= lower & df$glucose <= upper])
  TITR = TITR / 60 / 60 / 24
}

################################################################################

#Structure cgmquantify outputs into a data frame:
OH_results <- data.frame()
  
#Use psqi_list to calculate GV metrics for OH data corresponding to PSQI time frame:
for (i in 1:length(psqi_list)) {
  x = data.frame(
    "proj_member_ID_SGV" = names(psqi_list[i]),
    "total_time" = total_time(psqi_list[[i]]),
    "TIR" = TIR(psqi_list[[i]]),
    "TOR" = TOR(psqi_list[[i]]),
    "TBR" = TBR(psqi_list[[i]]),
    "TAR" = TAR(psqi_list[[i]]),
    "PIR" = PIR(psqi_list[[i]]),
    "POR" = POR(psqi_list[[i]]),
    "PBR" = PBR(psqi_list[[i]]),
    "PAR" = PAR(psqi_list[[i]]),
    "CV" = CV(psqi_list[[i]]),
    "TITR" = TITR(psqi_list[[i]])
  )
  OH_results <- rbind(OH_results, x)
}

OH_results <- separate(OH_results, proj_member_ID_SGV,
                       into = c("project_member_id", "SGV_Type"), sep = "-")

#Filter REDCap data to only include those who also have OH data:
OPEN_results <- REDCap_results[REDCap_results$project_member_id %in% OH_results$project_member_id, ] #filter by REDCap project member ids
OPEN_results <- merge(OPEN_results, OH_results, by = "project_member_id", all = TRUE) #merge in cgmq calcs
OPEN_results <- OPEN_results %>% distinct(project_member_id, .keep_all = TRUE) #clear out duplicates
OPEN_results <- OPEN_results %>% relocate(SGV_Type, .after = survey_record_id) #reorder variables
OPEN_results$SGV_Type <- gsub("entries\\d+", "entries", OPEN_results$SGV_Type) #clean up entries names

#Output excel file of all results
write_xlsx(OPEN_results, "/Users/drew.cooper/Documents/HDS_PhD/Project_1/OPEN_results_23.08.2024.xlsx")

################################################################################

# Note: DO NOT USE MODELS HERE. These are only for predictions and/or inferences. We are just DESCRIBING.

#Number of poor sleepers (PSQI_Score > 5):
poor_sleepers_percent <- (as.numeric(sum(OPEN_results$PSQI_Score > 5)) / as.numeric(length(OPEN_results$PSQI_Score))) * 100

#STATS:
cor.test(OPEN_results$TITR, OPEN_results$Sleep_Efficiency, method = "spearman")
wilcox.test(PBR ~ Sex, data = OPEN_results) #for 2 groups, numeric data (nonparametric t-test)
kruskal.test(CV ~ Age_group, data = OPEN_results) #for 3+ groups (nonparametric ANOVA)

#CIs without modeling (remember: CIs tell us about the stability of our estimate):
group_by(OPEN_results, Age_group) %>%
  summarise(
    n = n(),
    median = median(PSQI_Score),
    mean = mean(PSQI_Score),
    sd = sd(PSQI_Score),
    se = sd / sqrt(n),
    lower_ci = mean - qt(0.975, df = n - 1) * se,
    upper_ci = mean + qt(0.975, df = n - 1) * se
  )

#Subgroup(s):
OPEN_results %>%
  group_by(OPEN_results$Age_group) %>%
  summarise(
    spearman_rho = cor(OPEN_results$CV, OPEN_results$PSQI_Score, method = "spearman"),
    p_value = cor.test(OPEN_results$CV, OPEN_results$PSQI_Score, method = "spearman")$p.value
  )

#Categorical stats summary:
OPEN_results$Sex <- ordered(OPEN_results$Sex, levels = c("Female", "Male"))
group_by(OPEN_results, Sex) %>%
  summarise(
    count = n(),
    mean = mean(PSQI_Score, na.rm = TRUE),
    sd = sd(PSQI_Score, na.rm = TRUE),
    median = median(PSQI_Score, na.rm = TRUE),
    IQR = IQR(PSQI_Score, na.rm = TRUE)
  )

################################################################################

#Single glucose plot:
ggplot(data = psqi_list[["01104400-entries"]], aes(x = Date, y = glucose, group = 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "whitesmoke"),
        text = element_text(size = 25)) +
  geom_point(colour = "red", size = 3, alpha = 0.1) +
  geom_line(colour = "red", linewidth = 1, alpha = 0.1) +
  scale_x_datetime(date_breaks = "days") + ylim(0, 420) +
  labs(x = "Date (YYYY-MM-DD)", y = "Sensor Glucose Values (mg/dL)", title = "Donated AID Data") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_line(y = 70, linewidth = 2, colour = "blue") + geom_line(y = 180, linewidth = 2, colour = "blue") +
  stat_smooth(method = "gam", formula = y ~ poly(x, 20), linewidth = 3, position = "jitter")

#Plotting glucose data in a facet graph:
combined_df <- bind_rows(psqi_list, .id = "group")
ggplot(data = combined_df, aes(x = Date, y = glucose, group = 1)) +
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "whitesmoke")) +
  geom_point(colour = "red", alpha = 0.01) +
  geom_line(colour = "red", alpha = 0.01) +
  scale_x_datetime(date_breaks = "months") + ylim(0, 420) +
  labs(x = "Date (YYYY-MM-DD)", y = "Sensor Glucose Values (mg/dL)", title = "PSQI-AID Data") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_line(y = 70, colour = "blue") + geom_line(y = 180, colour = "blue") +
  stat_smooth(method = "gam", formula = y ~ poly(x, 15), position = "jitter") +
  facet_wrap(~ group, ncol = 6)

#Linear Plot:
ggplot(data = OPEN_results, aes(x = PIR, y = PSQI_Score)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "lavender")) +
  geom_point(size = 3, color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "red") +
  #geom_line(data = model_df, aes(x = PIR, y = .fitted), linetype = 1, size = 1.5, color = "red", alpha = 0.75) +
  geom_hline(yintercept = 5, linetype = 5, size = 1.5, color = "lavenderblush4", alpha = 0.5) +
  xlim(0, 1) + ylim(0, 21) +
  xlab("% Time-in-range (TIR)") + ylab("PSQI Score")

#Subgroup Linear Plot:
ggplot(data = OPEN_results, aes(x = PIR, y = PSQI_Score)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "lavender")) +
  geom_point(size = 3, aes(color = Age_group), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, aes(color = Age_group)) +
  #geom_line(data = model_df, aes(x = PIR, y = .fitted), linetype = 1, size = 1.5, color = "red", alpha = 0.75) +
  geom_hline(yintercept = 5, linetype = 5, size = 1.5, color = "lavenderblush4", alpha = 0.5) +
  xlim(0, 1) + ylim(0, 21) +
  xlab("% Time-in-range (TIR)") + ylab("PSQI Score")

#Violin Plot:
p <- ggplot(data = OPEN_results, aes(x = Age_group, y = PSQI_Score, fill = Age_group)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "whitesmoke"),
        text = element_text(size = 17)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.3) +
  geom_jitter(color = "black", size = 3, alpha = 0.5) +
  ylim(-1, 21) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Age Group (years)", y = "PSQI Score")

p + theme(legend.position = "none")

################################################################################
################################################################################

#NIGHTTIME DATA ONLY:
psqi_list_night <- lapply(psqi_list, function(df) {
  hour <- as.integer(format(df$Date, "%H"))
  df_night <- df[hour >= 22 | hour < 6, ]
  return(df_night)
})

#Structure cgmquantify outputs into a data frame:
OH_results_night <- data.frame()

#Use psqi_list to calculate GV metrics for OH data corresponding to PSQI time frame:
for (i in 1:length(psqi_list_night)) {
  x = data.frame(
    "proj_member_ID_SGV" = names(psqi_list_night[i]),
    "total_time" = total_time(psqi_list_night[[i]]),
    "TIR" = TIR(psqi_list_night[[i]]),
    "TOR" = TOR(psqi_list_night[[i]]),
    "TBR" = TBR(psqi_list_night[[i]]),
    "TAR" = TAR(psqi_list_night[[i]]),
    "PIR" = PIR(psqi_list_night[[i]]),
    "POR" = POR(psqi_list_night[[i]])
  )
  OH_results_night <- rbind(OH_results_night, x)
}

OH_results_night <- separate(OH_results_night, proj_member_ID_SGV,
                       into = c("project_member_id", "SGV_Type"), sep = "-")

#Filter REDCap data to only include those who also have OH data:
OPEN_results_night <- REDCap_results[REDCap_results$project_member_id %in% OH_results_night$project_member_id, ] #filter by REDCap project member ids
OPEN_results_night <- merge(OPEN_results_night, OH_results_night, by = "project_member_id", all = TRUE) #merge in cgmq calcs
OPEN_results_night <- OPEN_results_night %>% distinct(project_member_id, .keep_all = TRUE) #clear out duplicates
OPEN_results_night <- OPEN_results_night %>% relocate(SGV_Type, .after = survey_record_id) #reorder variables
OPEN_results_night$SGV_Type <- gsub("entries\\d+", "entries", OPEN_results_night$SGV_Type) #clean up entries names

#STATS:
cor.test(OPEN_results_night$TBR, OPEN_results_night$PSQI_Score, method = "spearman")
wilcox.test(TIR ~ Sex, data = OPEN_results_night)

#Linear Plot:
ggplot(data = OPEN_results_night, aes(x = PIR, y = PSQI_Score)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "lavender")) +
  geom_point(size = 3, color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "red") +
  #geom_line(data = model_df, aes(x = PIR, y = .fitted), linetype = 1, size = 1.5, color = "red", alpha = 0.75) +
  geom_hline(yintercept = 5, linetype = 5, size = 1.5, color = "lavenderblush4", alpha = 0.5) +
  xlim(0, 1) + ylim(0, 21) +
  xlab("% Time-in-range (TIR)") + ylab("PSQI Score")

excluded <- list(
  "02843395", # Missing Bolus data
  "04574576", # Missing 14 days of CGM data
  "06959790", # Missing 16 days of data
  "13570824", # Missing Treatment data for 24 days
  "15327577", # Missing 21 days of data
  "18361363", # Missing 8 days of data
  "19099241", # Missing 14 days of data
  "21218454", # Missing 18 days of treatment data
  "37159654", # Missing 16 days of data
  "38625719", # Missing 9 days of Treatment data
  "45407256", # Missing 21 days of data
  "47411539", # Missing 23 days of data
  "48546317", # Missing 10 days of data
  "50641121", # Missing 25 days of treatment data
  "54943175", # Missing 11 days of CGM data
  "66162958", # Missing 12 days of data
  "71748184", # Missing 27 days of data
  "76836292", # Missing 7 days of data
  "82464452", # Missing Treatments data
  "90860949" # Missing 21 days of data
)

# The data of these participants doesn't have flags whether a bolus was automated or not.
# However, there are repeated small amounts of boluses, which is indicative of SMBs.
# Boluses are manually classified as SMBs based on their amount
smb_thresholds <- list(
  "13577203" = 0.5,
  "16184009" = 0.5,
  "20104403" = 0.5,
  "22224188" = 0.5,
  "25127006" = 0.5,
  "46968717" = 0.5,
  "53661149" = 0.5,
  "74373797" = 0.5,
  "74812299" = 0.5,
  "83320236" = 0.5,
  "98275826" = 0.5,
  "99478201" = 0.5
)

# The data of these peoples shows that they are using eCarbs ("extended carbs", small amounts of carbs logged repeatedly) to cope with fatty and high-protein meals.
# We remove them based on their amounts
e_carbs_threshold <- list(
  "00752599" = 3,
  "03468378" = 5,
  "02773391" = 2,
  "04848189" = 4,
  "06601878" = 6,
  "08814820" = 3,
  "09612394" = 5,
  "13788197" = 3,
  "18927745" = 5,
  "19067168" = 5,
  "22919204" = 3,
  "23777770" = 4,
  "26745937" = 3,
  "27718918" = 7,
  "39260075" = 4,
  "40974871" = 11,
  "65033531" = 5,
  "70981757" = 5,
  "72492017" = 2,
  "91391413" = 3,
  "92076618" = 7,
  "93708481" = 3
)

# These people probably had some misunderstandings with the 24 hour system
# resulting in very unrealistically long sleep windows
bedtime_override <- list(
  "81133880" = "23:00", # Orginally 11:00
  "99478201" = "22:00" # Originally 10:00
)
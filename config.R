# Define the AID_PSQI repo as the working directory
script_dir <- normalizePath("/Users/drew.cooper/AID_PSQI")

# Set the working directory
setwd(script_dir)

# Define the data directory; this MUST be in the same parent folder as AID_PSQI for anything to work
data_dir <- normalizePath(file.path("..", "OPEN-Project-data"))

# Generate the empty folders for first time script users
create_dirs <- function(dirs) {
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
}

output_dirs <- c(
  "archive",
  "figures",
  "output"
)

create_dirs(output_dirs)
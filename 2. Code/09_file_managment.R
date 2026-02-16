# File managment

# Copy all files from source to destination
source_folder <- "1. Data/Graphs"
destination_folder <- "3. Graphs"

# Create destination folder if needed
#dir.create(destination_folder, recursive = TRUE, showWarnings = FALSE)

# Copy all files
file.copy(
  from = list.files(source_folder, full.names = TRUE),
  to = destination_folder,
  overwrite = TRUE
)

cat("All files copied successfully to 3. Graphs!\n")

file.copy(
  from = list.files("3. Graphs", full.names = TRUE),
  to = "4. LaTeX/Graphs",
  overwrite = TRUE
)

cat("All files copied successfully to LaTeX folder!\n")

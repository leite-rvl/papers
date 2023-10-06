# -------------------------------------------------------------------------
# This script extracts the waveform and reflectance information
# from the ecospeclib files
# r.vieiraleite@gmail.com
# https://github.com/leite-rvl/GEE-backup
# -------------------------------------------------------------------------



# -------------------------------------------------------------------------
# Building ECOSPECLIB table ----------------------------------------------
# -------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

library('dplyr')
library('tidyr')


# Directories -------------------------------------------------------------
# Directory where the .txt files of the ECOSTRESS spectral library are stored
dir_tables <- ''




# Set the path to your folder containing the .txt files
folder_path <- file.path(dir_tables,
                         'ecospeclib-1691444340058')

# List all files ending with "spectrum.txt"
file_list <- list.files(folder_path, pattern = "spectrum.txt", full.names = TRUE)


# Initialize an empty data frame to store the results
result_df <- data.frame()

# Loop through each file and extract the information
# file_path = file_list[1]
for (file_path in file_list) {
  
  # Read the file, skipping the first 20 rows
  file_data <- read.table(file_path,
                          skip = 20, 
                          header = FALSE, 
                          col.names = c("Wavelength", "Reflectance"))
  
  
  
  
  # Read the header section of the file
  header_lines <- readLines(file_path, n = 15)
  
  # Extract header values using regular expressions
  name <- sub("^Name: ", "", header_lines[1])
  type <- sub("^Type: ", "", header_lines[2])
  class <- sub("^Class: ", "", header_lines[3])
  genus <- sub("^Genus: ", "", header_lines[4])
  species <- sub("^Species: ", "", header_lines[5])
  sample <- sub("^Sample No.: ", "", header_lines[6])
  WavelengthRange <- sub("^Wavelength Range: ", "", header_lines[8])
  
  # Create a data frame for the current file
  file_df <- data.frame(
    Name = name,
    Type = type,
    Class = class,
    Genus = genus,
    Species = species,
    Sample = sample,
    WavelengthRange = WavelengthRange,
    Wavelength = file_data$Wavelength,
    Reflectance = file_data$Reflectance
  )
  
  
  # Append the data for the current file to the result data frame
  result_df <- bind_rows(result_df, file_df)
}

# Print the first few rows of the result data frame
head(result_df)


unique(result_df$Name) %>% length()
unique(result_df$Sample) %>% length()

unique(result_df$Sample) %>% length()# Write the data frame to a CSV file
write.csv(result_df, 
          file = file.path(dir_tables,
                           'ecospeclib_df.csv'), 
          row.names = FALSE)


#------------------------------------------------------------------------------#
#AUTHORS: Dias, B.S.
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# Function to process the niche overlap results from EwE
#------------------------------------------------------------------------------#


library(tidyverse)

process_all_niche_overlap_files <- function(folder_path, output_suffix = "_processed.csv") {
  
  # List all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Identify the unique niche_key file (assumes its filename contains "niche_overlap_key")
  niche_key_file <- csv_files[grepl("niche_overlap_key", csv_files, ignore.case = TRUE)]
  
  if (length(niche_key_file) == 0) {
    stop("niche_key file not found in the folder.")
  }
  
  # Read the niche_key file (assuming only one exists)
  niche_key <- read.csv(niche_key_file[1])
  
  # Identify all niche_overlap files 
  # (assumes their filenames contain "Niche overlap", but not "niche_overlap_key")
  niche_overlap_files <- csv_files[
    grepl("graph_Niche[[:space:]_]?overlap", csv_files, ignore.case = TRUE) &
      !grepl("niche_overlap_key", csv_files, ignore.case = TRUE)
  ]
  
  if(length(niche_overlap_files) == 0) {
    stop("No niche_overlap files found in the folder.")
  }
  
  # Process each niche_overlap file individually
  for (file in niche_overlap_files) {
    # Read the niche_overlap file
    niche_overlap <- read.csv(file)
    
    # Pivot the data: all columns except 'Predator_overlap' become rows.
    niche_overlap_long <- niche_overlap %>% 
      pivot_longer(cols = -Predator_overlap, names_to = "Groups", values_to = "Prey_overlap")
    
    # Remove NA rows and replace ".." with ":" in the Groups column.
    base_overlap <- niche_overlap_long %>% 
      na.omit() %>%
      mutate(Groups = gsub("\\.\\.", ":", Groups)) %>%
      separate(Groups, into = c("Group1", "Group2"), sep = ":")
    
    # Join the key file twice to add lookup values for each group.
    # Assumes niche_key has columns "Group.name" and "Node".
    base_overlap <- base_overlap %>%
      left_join(niche_key, by = c("Group1" = "Group.name")) %>% 
      rename(Node1 = Node) %>% 
      left_join(niche_key, by = c("Group2" = "Group.name")) %>% 
      rename(Node2 = Node)
    
    # Merge the Group1 and Group2 columns into a single 'Group' column,
    # and merge Node1 and Node2 into a single 'NewValue' column.
    base_overlap <- base_overlap %>%
      mutate(
        Group = paste(Group1, Group2, sep = ":"),
        Nodes = paste(Node1, Node2, sep = ":")
      )
    
    # Construct the output filename by appending the suffix to the original filename (without extension)
    output_filename <- paste0(tools::file_path_sans_ext(basename(file)), output_suffix)
    output_path <- file.path(folder_path, output_filename)
    
    # Write the processed data frame to a new CSV file.
    write_csv(base_overlap, output_path)
    
    message("Processed file: ", file, " -> ", output_path)
  }
}

# Example usage:
process_all_niche_overlap_files("DATA/Niche_Overlap/")



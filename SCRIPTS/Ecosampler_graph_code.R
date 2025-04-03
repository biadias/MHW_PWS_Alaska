#------------------------------------------------------------------------------#
#AUTHORS: Dias, B.S.
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# Code to make plots for EwE Ecosampler results. 
# https://doi.org/10.1016/j.softx.2018.06.004
#------------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(purrr)
library(readr)

# Set your working directory to where all Sample_00xxx folders and Sample_baseline are located:
setwd("DATA/ecosampler_results")

# List all folders that start with "Sample_" (including baseline)
all_folders <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE)
all_folders <- all_folders[grepl("Sample_", basename(all_folders))]

# For clarity, you can separate baseline from the others if needed
baseline_folder <- all_folders[grepl("baseline", basename(all_folders))]
sample_folders  <- setdiff(all_folders, baseline_folder)

baseline_folder
sample_folders


# A helper function to read the CSV in each folder and label it
read_estimates <- function(folder) {
  df <- read_csv(file.path(folder, "basic_estimates.csv"), show_col_types = FALSE)
  df$run <- basename(folder)  # Tag each row with the folder (run) name
  return(df)
}

# Read baseline data
sample_baseline <- read_estimates(baseline_folder)

# Read each of the sample folders
samples <- map_df(sample_folders, read_estimates)

ecosampler_dt <- bind_rows(sample_baseline, samples)
head(ecosampler_dt)

name_vector <- unique(ecosampler_dt$Group)

ecosampler_dt$Group <- factor(ecosampler_dt$Group, levels= name_vector)

vars <- c( #variables of interest
  "B",
  "PB",
  "QB"
)

vars_long <- ecosampler_dt %>%
  select(run, GroupNo, Group, any_of(vars)) %>%
  pivot_longer(cols      = any_of(vars),
               names_to  = "variable",
               values_to = "value") %>% filter(!value <= 0)  %>%
  filter(!Group == "Offshore_detritus" &
           !Group == "Inshore_detritus" & !Group == "Nekton_falls")

head(vars_long) # variables in long format to facilitate plotting

summary_data <- vars_long %>%
  group_by(Group, variable) %>%
  summarise(
    Mean     = mean(value, na.rm = TRUE),
    LCI      = quantile(value, 0.025, na.rm = TRUE),
    UCI      = quantile(value, 0.975, na.rm = TRUE),
    MinV     = min(value, na.rm = TRUE),
    MaxV     = max(value, na.rm = TRUE),
    # For convenience, also store the log-scale versions
    Mean_log = mean(log(value), na.rm = TRUE),
    LCI_log  = quantile(log(value), 0.025, na.rm = TRUE),
    UCI_log  = quantile(log(value), 0.975, na.rm = TRUE),
    MinV_log  = min(log(value), na.rm = TRUE),
    MaxV_log  = max(log(value), na.rm = TRUE),
    .groups  = "drop"
  )

head(summary_data)


Biomass_plot <- vars_long %>% 
  filter(variable== "B") %>% 
ggplot(aes(y = Group, x = value)) +
  geom_boxplot(fill= "white", color= "#046C9A",position = position_dodge(0.75), outlier.shape = NA) +
  #scale_x_log10()+
  theme_bw() +
  labs(
    x = "Biomass",
    y = " "
  )

Consumption_plot <- vars_long %>% 
  filter(variable== "QB") %>% 
  ggplot(aes(y = Group, x = value)) +
  geom_boxplot(fill= "white", color= "#046C9A",position = position_dodge(0.75), outlier.shape = NA) +
  #scale_x_log10()+
  theme_bw() +
  labs(
    x = "QB",
    y = " "
  )

Production_plot <- vars_long %>% 
  filter(variable== "PB") %>% 
  ggplot(aes(y = Group, x = value)) +
  geom_boxplot(fill= "white", color= "#046C9A",position = position_dodge(0.75), outlier.shape = NA) +
  #scale_x_log10()+
  theme_bw() +
  labs(
    x = "PB",
    y = " "
  )


ggsave(
  "FIGURES/Ecosampler_500_Biomass_plot.png",
  Biomass_plot,
  width = 8,
  height = 4.6
)
ggsave(
  "FIGURES/Ecosampler_500_Consumption_plot.png",
  Consumption_plot,
  width = 8,
  height = 4.6
)
ggsave(
  "FIGURES/Ecosampler_500_Production_plot.png",
  Production_plot,
  width = 8,
  height = 4.6
)

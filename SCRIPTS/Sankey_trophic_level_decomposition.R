# Install packages if you haven't already
# install.packages(c("ggplot2", "ggalluvial", "dplyr", "readr"))

library(ggplot2)
library(ggalluvial)
library(dplyr)
library(readr)
library(tidyverse)



# Data
data_rel <- read_csv("DATA/sankey_data_relative.csv")
data_abs <- read_csv("DATA/sankey_data_absolute.csv")
#-----------------------------------------------------------------------------#
#Trophic level decomposition_ trophic change scores ####
# This code is to find the groups that had significat changes in TLD scores 
# between 2010 and 2016 MHW, and between 1990 2000 post oil spill period
#-----------------------------------------------------------------------------#

data <- data_rel

# Oil TLD sankey ####
# We split the data into 1990 and 2000 sets and join them to compare side-by-side
structural_changes <- data %>%
  filter(Year %in% c("1990", "2000")) %>%
  select(Species, Year, Trophic_Level, Value) %>%
  
  # Spread to wide format or join self to get 1990 and 2000 columns
  pivot_wider(names_from = Year, values_from = Value, values_fill = 0) %>%
  
  # Remove species that don't exist in both years (optional, but safer)
  filter(`1990` > 0 | `2000` > 0) %>%
  
  # 3. Calculate Differences
  mutate(
    Diff = `2000` - `1990`,
    Abs_Diff = abs(Diff)
  ) %>%
  
  # 4. Sum the differences per Species (The L1 Norm Score)
  group_by(Species) %>%
  summarise(
    Total_Score = sum(Abs_Diff),
    # Create a summary string of the biggest changes for context
    Major_Shifts = paste0(
      Trophic_Level[abs(Diff) > 0.01], " (", round(Diff[abs(Diff) > 0.01], 3), ")",
      collapse = ", "
    )
  ) %>%
  
  # 5. Sort by biggest change
  arrange(desc(Total_Score))

# 6. View the Top 5 Affected Species
str_changes_oil <- print(head(structural_changes, 10))

# 2010 - 2016 MHW

# Analysis
# We split the data into 1990 and 2000 sets and join them to compare side-by-side
structural_changes_mhw <- data %>%
  filter(Year %in% c("2010", "2016 MHW")) %>%
  select(Species, Year, Trophic_Level, Value) %>%
  
  # Spread to wide format or join self to get 1990 and 2000 columns
  pivot_wider(names_from = Year, values_from = Value, values_fill = 0) %>%
  
  # Remove species that don't exist in both years (optional, but safer)
  filter(`2010` > 0 | `2016 MHW` > 0) %>%
  
  # 3. Calculate Differences
  mutate(
    Diff = `2016 MHW` - `2010`,
    Abs_Diff = abs(Diff)
  ) %>%
  
  # 4. Sum the differences per Species (The L1 Norm Score)
  group_by(Species) %>%
  summarise(
    Total_Score = sum(Abs_Diff),
    # Create a summary string of the biggest changes for context
    Major_Shifts = paste0(
      Trophic_Level[abs(Diff) > 0.01], " (", round(Diff[abs(Diff) > 0.01], 3), ")",
      collapse = ", "
    )
  ) %>%
  
  # 5. Sort by biggest change
  arrange(desc(Total_Score))

# 6. View the Top 5 Affected Species
str_changes_mhw <- print(head(structural_changes_mhw, 6
                              ))




# --- OPTIONAL: VISUALIZE THE SHIFT FOR TOP SPECIES ---
# Let's plot the top species to see the shift
top_species <- structural_changes$Species[4] # e.g., "Seabirds"

data %>%
  filter(Species == top_species, Year %in% c("1990", "2000")) %>%
  ggplot(aes(x = Trophic_Level, y = Value, fill = Year)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("1990" = "#2166ac", "2000" = "#4393c3")) +
  labs(
    title = paste("Trophic Shift: ", top_species, "(1990 vs 2000)"),
    y = "Relative Contribution",
    x = "Trophic Level Input"
  ) +
  theme_minimal()

## Sankey plots bulk ####

# Plotting function
plot_species_sankey <- function(data, species_name, years_to_include = NULL) {
  
  # --- Data Filtering & Prep ---
  
  # Filter for the specific species
  species_data <- data %>%
    filter(Species == species_name)
  
  # Filter specific years if requested
  if (!is.null(years_to_include)) {
    species_data <- species_data %>% filter(Year %in% years_to_include)
  }
  
  # 1. Enforce Chronological Order for Years
  year_order <- c("1990", "1997 MHW", "2000", "2005 MHW", "2010", "2016 MHW", "2019 MHW")
  # Only use years that actually exist in the filtered data
  existing_years <- year_order[year_order %in% unique(species_data$Year)]
  species_data$Year <- factor(species_data$Year, levels = existing_years)
  
  # 2. Enforce Numerical Order for Trophic Levels (Roman Numerals)
  tl_order <- c("II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
  species_data$Trophic_Level <- factor(species_data$Trophic_Level, levels = tl_order)
  
  # --- Color Definition (Warm vs Cold) ---
  custom_colors <- c(
    "1990" = "#92c5de",      # Cold (Blue)"#2166ac"
    "2000" = "#4393c3",      # Cold (Blue)"#4393c3"
    "2010" = "#2166ac",      # Cold (Blue)"#92c5de"
    "1997 MHW" = "#b2182b",  # Warm (Red)"#b2182b"
    "2005 MHW" = "#d6604d",  # Warm (Orange)"#d6604d"
    "2016 MHW" = "#d6604d",  # Warm (Salmon)"#f4a582"
    "2019 MHW" = "#b2182b"   # Warm (Peach)"#fddbc7"
  )
  
  # --- Plotting ---
  
  # We switch from aes(x=Year) to aes(axis1, axis2) to get the Left-to-Right flow
  ggplot(species_data,
         aes(y = Value, 
             axis1 = Trophic_Level,  # LEFT Axis
             axis2 = Year)) +        # RIGHT Axis
    
    # 1. The Flows (Colored by Year)
    geom_alluvium(aes(fill = Year), width = 1/12) +
    
    # 2. The Vertical Bars (Strata)
    geom_stratum(width = 1/12, fill = "grey95", color = "grey50") +
    
    # 3. Text Labels (Trophic Levels and Years)
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    
    # 4. Settings
    scale_x_discrete(limits = c("Trophic Level", "Year"), expand = c(.05, .05)) +
    scale_fill_manual(values = custom_colors) +
    theme_minimal() +
    labs(
      title = paste(species_name),
      subtitle = "Relative Contribution of Trophic Levels (Left) to Year (Right)",
      y = "Relative Contribution"
    ) +
    theme(
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

# Example A: Plot a single species (All Years)
plot_species_sankey(data_rel, "Transient_orca", years_to_include = c("2010", "2016 MHW"))
plot_species_sankey(data_abs, "Sea_otters", years_to_include = c("2010", "2016 MHW"))

plot_species_sankey(data_abs, "Sleeper_sharks", years_to_include = c("2010", "2016 MHW"))

# Example B: Plot a single species (Specific Years Only)
# Comparing 1990 to 2019
plot_species_sankey(data_abs, "Halibut", years_to_include = c("2000", "2016 MHW"))

# Example C: Generate ALL plots and save to one PDF
save_all_species_to_pdf <- function(data, filename="Sankey_Plots.pdf", years=NULL) {
  
  pdf(filename, width = 10, height = 7)
  
  # Get list of all species
  all_species <- sort(unique(data$Species))
  
  for (sp in all_species) {
    # Check if data exists for this species in the requested years
    # (Prevents errors if a species is missing from specific years)
    tryCatch({
      print(paste("Generating:", sp))
      p <- plot_species_sankey(data, sp, years_to_include = c("2000", "2016 MHW"))
      print(p) # Print to PDF
    }, error = function(e) {
      print(paste("Skipping", sp, "due to missing data or error."))
    })
  }
  
  dev.off()
  print(paste("Done! Saved to", filename))
}

# Run the bulk save:
save_all_species_to_pdf(data, "My_Sankey_Plots_TLD.pdf")





#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Percent change MHW 2010 vs 2016 ####
# Install packages if missing
# install.packages(c("tidyverse", "scales"))

library(tidyverse)
library(scales)

# Absolute Data
# Make sure "sankey_data_absolute.csv" is in your working directory
data <- data_abs %>% 
  filter(!Species %in% "Total")

# Filter and Calculate Totals
# We sum across all Trophic Levels to get the total biomass/flow per species per year
pct_change_data <- data %>%
  filter(Year %in% c("2010", "2016 MHW")) %>%
  group_by(Species, Year) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Total_Value, values_fill = 0) %>%
  
  # Percent Change
  # Formula: (New - Old) / Old * 100
  mutate(
    Diff = `2016 MHW` - `2010`,
    Pct_Change = (Diff / `2010`) * 100
  ) %>%
  
  # Remove species with 0 value in year 2000 (div by zero) or negligible change
  filter(`2010` > 0.001) %>%
  filter(!is.na(Pct_Change)) %>%
  
  # Sort and Filter for Top Changes
  # Let's keep the Top 20 species with the biggest absolute percent change
  mutate(Abs_Change = abs(Pct_Change)) %>%
  arrange(desc(Abs_Change)) %>%
  head(40) %>%
  
  # Create a category for coloring (Increase vs Decrease)
  mutate(
    Type = ifelse(Pct_Change > 0, "Increase", "Decrease"),
    Species = fct_reorder(Species, Pct_Change) # Order factor for plotting
  )

# 5. Plot the Diverging Bar Chart
TDL_MHW_percent_change <- ggplot(pct_change_data, aes(x = Species, y = Pct_Change, fill = Type)) +
  geom_col(width = 0.7) +
  #geom_text(aes(label = paste0(round(Pct_Change, 1), "%")), 
  #          hjust = ifelse(pct_change_data$Pct_Change > 0, -0.2, 1.2), 
  #          size = 3.5) +
  coord_flip() + # Flip to horizontal bars
  scale_fill_manual(values = c("Decrease" = "#D55E00", "Increase" = "#0072B2")) +
  expand_limits(y = c(min(pct_change_data$Pct_Change)*1.2, max(pct_change_data$Pct_Change)*1.2)) +
  labs(
    title = "Top 20 Species by Percent Change (2010 vs 2016 MHW)",
    subtitle = "Relative change in total absolute biomass/flow",
    y = "Percent Change (%)",
    x = "Species",
    fill = "Direction"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 10, face = "bold"),
    panel.grid.major.y = element_blank() # Clean up horizontal grid lines
  )

ggsave(
  "FIGURES/TDL_MHW_percent_change.png",
  TDL_MHW_percent_change,
  width = 6, 
  height = 8,
  bg = "transparent"
)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Percent change oil 1990 vs 2000 ####

# Absolute Data
data_oil <- data_abs %>% 
  filter(!Species %in% "Total")

# Filter and Calculate Totals
# We sum across all Trophic Levels to get the total biomass/flow per species per year
pct_change_data_oil <- data_oil %>%
  filter(Year %in% c("1990", "2000")) %>%
  group_by(Species, Year) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Total_Value, values_fill = 0) %>%
  
  # Percent Change
  # Formula: (New - Old) / Old * 100
  mutate(
    Diff = `2000` - `1990`,
    Pct_Change = (Diff / `1990`) * 100
  ) %>%
  
  # Remove species with 0 value in year 2000 (div by zero) or negligible change
  filter(`2000` > 0.001) %>%
  filter(!is.na(Pct_Change)) %>%
  
  # Sort and Filter for Top Changes
  # Let's keep the Top 20 species with the biggest absolute percent change
  mutate(Abs_Change = abs(Pct_Change)) %>%
  arrange(desc(Abs_Change)) %>%
  head(40) %>%
  
  # Create a category for coloring (Increase vs Decrease)
 mutate(
   Type = ifelse(Pct_Change > 0, "Increase", "Decrease"),
   Species = fct_reorder(Species, Pct_Change) # Order factor for plotting
 )

# 5. Plot the Diverging Bar Chart
TDL_oil_percent_change <- ggplot(pct_change_data_oil, aes(x = Species, y = Pct_Change, fill = Type)) +
  geom_col(width = 0.7) +
  #geom_text(aes(label = paste0(round(Pct_Change, 1), "%")), 
   #         hjust = ifelse(pct_change_data_oil$Pct_Change > 0, -0.2, 1.2), 
    #        size = 3.5) +
  coord_flip() + # Flip to horizontal bars
  scale_fill_manual(values = c("Decrease" = "#D55E00", "Increase" = "#0072B2")) +
  expand_limits(y = c(min(pct_change_data_oil$Pct_Change)*1.2, max(pct_change_data_oil$Pct_Change)*1.2)) +
  labs(
    title = "Top 20 Species by Percent Change (1990 vs 2000)",
    subtitle = "Relative change in total absolute biomass/flow",
    y = "Percent Change (%)",
    x = "Species",
    fill = "Direction"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 10, face = "bold"),
    panel.grid.major.y = element_blank() # Clean up horizontal grid lines
  )

ggsave(
  "FIGURES/TDL_oil_percent_change.png",
  TDL_oil_percent_change,
  width = 6, 
  height = 8,
  bg = "transparent"
)

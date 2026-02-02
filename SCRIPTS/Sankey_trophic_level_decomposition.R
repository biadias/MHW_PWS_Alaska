# Install packages if you haven't already
# install.packages(c("ggplot2", "ggalluvial", "dplyr", "readr"))

library(ggplot2)
library(ggalluvial)
library(dplyr)
library(readr)

# 1. Load the data
data <- read_csv("DATA/sankey_data_clean.csv")
data_abs <- read_csv("DATA/sankey_data_absolute.csv")


# 2. Define the plotting function
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
    "1990" = "#2166ac",      # Cold (Blue)
    "2000" = "#4393c3",      # Cold (Blue)
    "2010" = "#92c5de",      # Cold (Blue)
    "1997 MHW" = "#b2182b",  # Warm (Red)
    "2005 MHW" = "#d6604d",  # Warm (Orange)
    "2016 MHW" = "#f4a582",  # Warm (Salmon)
    "2019 MHW" = "#fddbc7"   # Warm (Peach)
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

# ==========================================
# 3. Usage Examples
# ==========================================

# Example A: Plot a single species (All Years)
p1 <- plot_species_sankey(data_abs, "Transient_orca", years_to_include = c("1990", "2019 MHW"))
print(p1)

plot_species_sankey(data_abs, "Sleeper_sharks", years_to_include = c("2010", "2016 MHW"))

# Example B: Plot a single species (Specific Years Only)
# Comparing 1990 to 2019
p2 <- plot_species_sankey(data, "Halibut", years_to_include = c("1990", "2019 MHW"))
print(p2)

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
      p <- plot_species_sankey(data, sp, years_to_include = years)
      print(p) # Print to PDF
    }, error = function(e) {
      print(paste("Skipping", sp, "due to missing data or error."))
    })
  }
  
  dev.off()
  print(paste("Done! Saved to", filename))
}

# Run the bulk save:
# save_all_species_to_pdf(data, "My_Sankey_Plots.pdf")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Install packages if missing
# install.packages(c("tidyverse", "scales"))

library(tidyverse)
library(scales)

# 1. Load the Absolute Data
# Make sure "sankey_data_absolute.csv" is in your working directory
data <- read_csv("DATA/sankey_data_absolute.csv")

# 2. Filter and Calculate Totals
# We sum across all Trophic Levels to get the total biomass/flow per species per year
pct_change_data <- data %>%
  filter(Year %in% c("2010", "2016 MHW")) %>%
  group_by(Species, Year) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Total_Value, values_fill = 0) %>%
  
  # 3. Calculate Percent Change
  # Formula: (New - Old) / Old * 100
  mutate(
    Diff = `2016 MHW` - `2010`,
    Pct_Change = (Diff / `2010`) * 100
  ) %>%
  
  # Remove species with 0 value in year 2000 (div by zero) or negligible change
  filter(`2010` > 0.001) %>%
  filter(!is.na(Pct_Change)) %>%
  
  # 4. Sort and Filter for Top Changes
  # Let's keep the Top 20 species with the biggest absolute percent change
  mutate(Abs_Change = abs(Pct_Change)) %>%
  arrange(desc(Abs_Change)) %>%
  head(30) %>%
  
  # Create a category for coloring (Increase vs Decrease)
  mutate(
    Type = ifelse(Pct_Change > 0, "Increase", "Decrease"),
    Species = fct_reorder(Species, Pct_Change) # Order factor for plotting
  )

# 5. Plot the Diverging Bar Chart
ggplot(pct_change_data, aes(x = Species, y = Pct_Change, fill = Type)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(Pct_Change, 1), "%")), 
            hjust = ifelse(pct_change_data$Pct_Change > 0, -0.2, 1.2), 
            size = 3.5) +
  coord_flip() + # Flip to horizontal bars
  scale_fill_manual(values = c("Decrease" = "#b2182b", "Increase" = "#2166ac")) +
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

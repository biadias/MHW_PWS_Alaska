library(tidyverse)
library(readr)
library(here)



data <- read_csv("DATA/energy_transfer_data_raw.csv")

# Extract Baseline
baseline_row <- data %>% filter(Year == "Baseline")
base_prod <- baseline_row$`from primary producers`
base_detr <- baseline_row$`from dentritus`
base_total <- baseline_row$`total efficiency`

# Calculate Percent Change
scenarios <- data %>% 
  filter(Year != "Baseline") %>%
  mutate(Year = as.character(Year)) %>%
  mutate(
    dp_producer = (`from primary producers` - base_prod) / base_prod * 100,
    dp_detritus = (`from dentritus` - base_detr) / base_detr * 100,
    dp_total    = (`total efficiency` - base_total) / base_total * 100
  )

# Long Format
scenarios_long <- scenarios %>%
  select(Year, dp_producer, dp_detritus, dp_total) %>%
  pivot_longer(
    cols = starts_with("dp_"),
    names_to = "scenario_code",
    values_to = "dp"
  ) %>%
  mutate(
    scenario = recode(scenario_code,
                      dp_producer = "From Primary Producers",
                      dp_detritus = "From Detritus",
                      dp_total    = "Total Efficiency"
    )
  )


scenarios_long$Year <- factor(scenarios_long$Year, 
                              levels = sort(unique(scenarios_long$Year), decreasing = TRUE))
#
# Overlay Logic
# Inside each year, we want to draw the biggest bar first to avoid hidding smaller ones.
scenarios_long <- scenarios_long %>%
  group_by(Year) %>%
  arrange(desc(abs(dp)), .by_group = TRUE) %>% 
  ungroup()

#Plot
p_ordered <- ggplot(scenarios_long, aes(x = dp, y = Year, fill = scenario)) +
  
  # 'identity' allows overlay, dogde will make the bars appear side by side.
  geom_col(position = "identity", width = 0.7, alpha = 0.9) +
  scale_x_continuous(
    limits = c(min(scenarios_long$dp) * 1.2, max(scenarios_long$dp) * 1.2),
    breaks = scales::pretty_breaks(n = 10)
  ) +
  
  scale_fill_manual(values = c(
    "From Primary Producers" = "#009e73",
    "From Detritus" = "grey30",
    "Total Efficiency" = "grey"
  )) +
  
  labs(
    x = expression("Percent change in efficiency from baseline (%)"),
    y = "Year",
    fill = "Source",
    title = " ",
    subtitle = " "
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "grey80"),
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "right"
  )

print(p_ordered)

ggsave(
  "FIGURES/transfer_efficiency.png",
  p_ordered,
  width = 6, 
  height = 8,
  bg = "transparent"
)

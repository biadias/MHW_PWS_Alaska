#------------------------------------------------------------------------------#
#AUTHORS: Dias, B.S.
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#------------------------------------------------------------------------------#
library(tidyverse)
library(here)

Ecosim_net <- read.csv("DATA/Refined PWS_v8_roms_graph_ascendency_lossprod.csv")

data_summary <- Ecosim_net %>%
  group_by(Year) %>%
  summarise(
    AverageAscendency = mean(Ascendency.on.flow),
    AverageLossOfProduction = mean(Loss.of.production..relative.)
  ) %>%
  ungroup() %>% 
  filter(Year < 2023)
#mutate(YearMonth = interaction(Year, Month, sep = "-")) # create a Year-Month interaction variable

# Plot for Ascendency

Ascendency_year <- data_summary %>%
  #group_by(Month) %>%
  ggplot(aes(x = Year, y = AverageAscendency)) +
  theme_transparent() +
  geom_line(colour = "#5fa2ce", linewidth = 2) +
  #geom_smooth()+
  annotate(
    "rect",
    xmin = 2014,
    xmax = 2016,
    ymin = -Inf,
    ymax = Inf,
    fill = "gray80",
    alpha = 0.4
  ) +
  annotate(
    "rect",
    xmin = 2019.01,
    xmax = 2019.90,
    ymin = -Inf,
    ymax = Inf,
    fill = "gray80",
    alpha = 0.4
  ) +
  annotate(
    "rect",
    xmin = 1989,
    xmax = 1994,
    ymin = -Inf,
    ymax = Inf,
    fill = "darkred",
    alpha = 0.15
  ) +
  geom_vline(
    xintercept = 1989,
    color = "darkred",
    linetype = "dashed",
    linewidth = 1, 
    alpha = 0.4
  ) +
  
  theme(
    axis.text.x =  element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.ticks.x = element_line(colour = "darkgrey"),
    axis.line.x = element_line(colour = "darkgrey"),
    axis.ticks.y = element_line(colour = "darkgrey"),
    axis.line.y = element_line(colour = "darkgrey"),
    legend.text = element_text(size = 12, color = "darkgrey"),
    legend.title = element_text(size = 12, color = "darkgrey")
  ) +
  labs(y = "Ascendency")


ggsave(
  "FIGURES/Ascendency_year.png",
  Ascendency_year,
  width = 8,
  height = 4.6,
  bg = "transparent"
)



# Plot for Loss of production
Lossprod_year <- data_summary %>%
  #group_by(Month) %>%
  ggplot(aes(x = Year, y = AverageLossOfProduction)) +
  theme_transparent() +
  geom_line(colour = "#c85200", linewidth = 2) +
  annotate(
    "rect",
    xmin = 2014,
    xmax = 2016,
    ymin = -Inf,
    ymax = Inf,
    fill = "gray80",
    alpha = 0.4
  ) +
  annotate(
    "rect",
    xmin = 2019.01,
    xmax = 2019.90,
    ymin = -Inf,
    ymax = Inf,
    fill = "gray80",
    alpha = 0.4
  ) +
  annotate(
    "rect",
    xmin = 1989,
    xmax = 1994,
    ymin = -Inf,
    ymax = Inf,
    fill = "darkred",
    alpha = 0.15
  ) +
  geom_vline(
    xintercept = 1989,
    color = "darkred",
    linetype = "dashed",
    linewidth = 1, 
    alpha = 0.4
  ) +
  theme(
    axis.text.x =  element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.ticks.x = element_line(colour = "darkgrey"),
    axis.line.x = element_line(colour = "darkgrey"),
    axis.ticks.y = element_line(colour = "darkgrey"),
    axis.line.y = element_line(colour = "darkgrey"),
    legend.text = element_text(size = 12, color = "darkgrey"),
    legend.title = element_text(size = 12, color = "darkgrey")
  ) +
  labs(y = "Production loss (Biomass mt/km2)")

ggsave(
  "FIGURES/Lossprod_year.png",
  Lossprod_year,
  width = 8,
  height = 4.6,
  bg = "transparent"
)




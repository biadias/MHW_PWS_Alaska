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


# Overlaid plots
# MHW analysis with heatwaveR package

OISST_data <- readRDS("DATA/OISST_dt.Rds", refhook = NULL) %>% 
  rename(t=time, temp=sst)
OISST_NGA <- OISST_data %>%
  group_by(t) %>%
  summarize(temp = mean(temp, na.rm = TRUE))

head(OISST_NGA)
library(heatwaveR)

ts_nga <- ts2clm(OISST_NGA, climatologyPeriod = c("1982-01-01", "2023-05-08"))
mhw_nga <- detect_event(ts_nga)

mhw_nga$event$Year <- as.numeric(format(mhw_nga$event$date_peak, "%Y"))

Lossprod_year_mhw <- data_summary %>%
  ggplot(aes(x = Year, y = AverageLossOfProduction)) +
  theme_transparent() +
  geom_line(colour = "#c85200", linewidth = 2) +
  
  # Now add geom_lolli() with the correct usage:
  geom_lolli(
    data = mhw_nga$event,                  # or whatever your event data is called
    aes(x = Year, y = intensity_max), # you must supply aes()
    color = "orange",
    show.legend = FALSE
    # optionally: fill = "grey70", n = 3, etc.
  ) +
  
  # Then your annotations:
  annotate(
    "rect",
    xmin = 2013.5,
    xmax = 2016.5,
    ymin = -Inf,
    ymax = Inf,
    fill = "gray80",
    alpha = 0.4
  ) +
  annotate(
    "rect",
    xmin = 2018.5,
    xmax = 2019.5,
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
  scale_y_continuous(
  name = "Production loss (Biomass mt/km2)",
  sec.axis = dup_axis(name = "Heatwave intensity (°C)"))+
    theme(
      axis.text.x =  element_text(size = 12, color = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      axis.ticks.x = element_line(colour = "darkgrey"),
      axis.line.x = element_line(colour = "darkgrey"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 12, color = "black"),
      axis.ticks.y = element_line(colour = "darkgrey"),
      axis.line.y = element_line(colour = "darkgrey"),
      axis.text.y.right = element_text(size = 12, color = "darkorange"),
      axis.title.y.right = element_text(size = 12, color = "darkorange"),
      axis.ticks.y.right = element_line(colour = "darkorange"),
      axis.line.y.right = element_line(colour = "darkorange"),
      legend.text = element_text(size = 12, color = "darkgrey"),
      legend.title = element_text(size = 12, color = "darkgrey")
    ) 

Lossprod_year_mhw

ggsave(
  "FIGURES/Lossprod_year_mhw.png",
  Lossprod_year_mhw,
  width = 8,
  height = 4.6,
  bg = "transparent"
)

#------------------------------------------------------------------------------#
#AUTHORS: Dias, B.S.
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#------------------------------------------------------------------------------#
library(tidyverse)
library(here)
library(ggpubr)
library(patchwork)

Ecosim_net <- read.csv("DATA/Refined PWS_v8_roms_graph_ascendency_lossprod.csv")

data_summary <- Ecosim_net %>%
  group_by(Year) %>%
  summarise(
    AverageAscendency = mean(Ascendency.on.flow),
    AverageLossOfProduction = mean(Loss.of.production..relative.)
  ) %>%
  mutate(CumulativeLoss=cumsum(AverageLossOfProduction)) %>% 
  ungroup() %>% 
  filter(Year < 2023)
#mutate(YearMonth = interaction(Year, Month, sep = "-")) # create a Year-Month interaction variable

# Plot for Ascendency####

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
    xmin = 2018.50,
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



# Plot for Loss of production ####
Lossprod_year <- data_summary %>%
  #group_by(Month) %>%
  ggplot(aes(x = Year, y = AverageLossOfProduction)) +
  theme_transparent() +
  geom_line(colour = "#c85200", linewidth = 2) +
  xlim(1988, 2022)+
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
    xmax = 2019.9,
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
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.ticks.x = element_line(colour = "darkgrey"),
    axis.line.x = element_line(colour = "darkgrey"),
    axis.ticks.y = element_line(colour = "darkgrey"),
    axis.line.y = element_line(colour = "darkgrey"),
    legend.text = element_text(size = 12, color = "darkgrey"),
    legend.title = element_text(size = 12, color = "darkgrey")
  ) +
  labs(y = expression(paste("Production loss (","mt" , ~km^{-2},")" )))

ggsave(
  "FIGURES/Lossprod_year.png",
  Lossprod_year,
  width = 8,
  height = 4.6,
  bg = "transparent"
)

# Plot for Culmulative Loss of production ####

CumulativeLossprod_year <- data_summary %>%
  ggplot(aes(x = Year, y = CumulativeLoss)) +
  theme_transparent() +
  geom_line(color = "#c85200", linewidth = 2) +
  xlim(1988, 2022) +
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
    xmax = 2019.9,
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
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.ticks.x = element_line(colour = "darkgrey"),
    axis.line.x = element_line(colour = "darkgrey"),
    axis.ticks.y = element_line(colour = "darkgrey"),
    axis.line.y = element_line(colour = "darkgrey"),
    legend.text = element_text(size = 12, color = "darkgrey"),
    legend.title = element_text(size = 12, color = "darkgrey")
  ) +
  labs(y = expression(paste("Cumulative production loss (","mt" , ~km^{-2},")" )))


# Marine heatwave

OISST_data <- readRDS("DATA/OISST_dt.Rds", refhook = NULL) %>% 
  rename(t=time, temp=sst)
OISST_NGA <- OISST_data %>%
  group_by(t) %>%
  summarize(temp = mean(temp, na.rm = TRUE))

head(OISST_NGA)
library(heatwaveR)


ts_nga <- ts2clm(OISST_NGA, climatologyPeriod = c("1989-01-01", "2023-05-08"))
mhw_nga <- detect_event(ts_nga)
mhw_nga$event$Year <- as.numeric(format(mhw_nga$event$date_peak, "%Y"))

# View just a few metrics
nga_events <- mhw_nga$event %>% 
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start,date_end, date_peak, intensity_max, intensity_cumulative) %>% 
  dplyr::arrange(-intensity_cumulative) 

mhwloli_intensity <- ggplot() +
  geom_lolli(
    data = mhw_nga$event,
    # or whatever your event data is called
    aes(x = Year, y = intensity_max),
    # you must supply aes()
    color = "orange",
    show.legend = FALSE
    # optionally: fill = "grey70", n = 3, etc.
  ) +
  xlim(1988, 2022) +
  theme_transparent() +
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
    xmax = 2019.9,
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
  scale_y_continuous(name = "Intensity (°C)") +
  theme(
    axis.text.x =  element_text(size = 12, color = "black"),
    axis.title.x = element_blank(),
    axis.ticks.x = element_line(colour = "darkgrey"),
    axis.line.x = element_line(colour = "darkgrey"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.ticks.y = element_line(colour = "darkgrey"),
    axis.line.y = element_line(colour = "darkgrey"),
    legend.text = element_text(size = 12, color = "darkgrey"),
    legend.title = element_text(size = 12, color = "darkgrey")
  ) 



mhwloli_duration <- ggplot() +
  geom_lolli(
    data = mhw_nga$event,
    # or whatever your event data is called
    aes(x = Year, y = duration),
    # you must supply aes()
    color = "orange",
    show.legend = FALSE
    # optionally: fill = "grey70", n = 3, etc.
  ) +
  xlim(1988, 2022) +
  theme_transparent() +
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
    xmax = 2019.9,
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
  scale_y_continuous(name = "Heatwave duration (days)") +
  theme(
    axis.text.x =  element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.ticks.x = element_line(colour = "darkgrey"),
    axis.line.x = element_line(colour = "darkgrey"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.ticks.y = element_line(colour = "darkgrey"),
    axis.line.y = element_line(colour = "darkgrey"),
    legend.text = element_text(size = 12, color = "darkgrey"),
    legend.title = element_text(size = 12, color = "darkgrey")
  ) 

plot_lossprod_mhw <- Lossprod_year / mhwloli_intensity/ mhwloli_duration

#grid::grid.draw(grid::textGrob(ylab,x=0.02, rot=90))
plot_lossprod_mhw_v1 <- plot_lossprod_mhw+
  plot_annotation(tag_levels= list(c("A ","B ","C ")))

#ggsave("PLOTS/plot_a_exp.svg",plot_all_exp, width = 7.16, height = 10)

ggsave("FIGURES/plot_lossprod_mhw_v1.png",plot_lossprod_mhw_v1, width = 5.8, height = 10)



# Overlaid MHW plots_double Y-axis #### 
# MHW analysis with heatwaveR package
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




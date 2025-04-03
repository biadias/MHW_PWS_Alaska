#------------------------------------------------------------------------------#
#AUTHORS: Dias, B.S.
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#------------------------------------------------------------------------------#
# Libraries ####

library(here)
library(hrbrthemes)
library(tidyverse)
library(lubridate)
library(janitor)
library(reshape2)
library(scales)
library(ggpubr)
library(gghighlight)
library(MetBrewer)
library(grDevices)

## Null model_ time series only ####
Mod_null <- read.csv("DATA/Refined PWS_v8_roms_graph_Relative biomass_v2.csv",
                     header = T) %>%
  clean_names()
Mod_null <- na.omit(Mod_null)
Mod_null$data <- date_decimal(Mod_null$data)
#SC1 <- SC1 %>% clean_names()
colnames(Mod_null) <- c(
  "year",
  "Transient_orca",
  "Salmon_sharks",
  "Resident_orca",
  "Sleeper_sharks",
  "Halibut",
  "Pinnipeds",
  "Porpoise",
  "Lingcod",
  "Arrowtooth_L",
  "Salmon_L",
  "Pacific_cod",
  "Sablefish",
  "Arrowtooth_S",
  "Spiny_dogfish",
  "Avian_raptors",
  "Octopods",
  "Seabirds",
  "Deep_demersals",
  "Pollock_L",
  "Rockfish",
  "Baleen_whales",
  "Salmon_fry_S",
  "Nshore_demersal",
  "Squids",
  "Eulachon",
  "Sea_otters",
  "Deep_epibenthos",
  "Capelin",
  "Herring_L",
  "Pollock_S",
  "Invert_eat_seaduck",
  "Oystercatchers",
  "Sandlance",
  "Sunflower_stars",
  "Pisaster_Evasterias",
  "Leather_stars",
  "Sea_cucumbers",
  "Urchins",
  "Helmet_crab",
  "Herring_S",
  "Jellies",
  "Deep_infauna_S",
  "Zoopl_near_omniv",
  "Zoop_omniv",
  "Shallow_infauna_S",
  "Meiofauna",
  "Deep_infauna_L",
  "Snail_crust_S",
  "Mussels",
  "Barnacles",
  "Shallow_infauna_clams",
  "Zoopl_near_herb",
  "Zoopl_herb",
  "Phyto_near",
  "Phyto_off",
  "Fucus",
  "Subtidal_kelps",
  "Macroalgae_other",
  "Eelgrass",
  "Nekton_falls",
  "Inshore_detritus",
  "Offshore_detritus"
)

Mod_null_long <- Mod_null %>% pivot_longer(!year, names_to = "Functional_groups", values_to =
                                             "value")

Mod_null_dt <- Mod_null_long %>% mutate(Year= year(year), month=month(year)) %>% 
  group_by(Year, Functional_groups) %>% 
  filter(Year<2031) %>% 
  summarize(n= n(), mean_var=mean(value), sd=sd(value))

Mod_null_dt_pre_mhw <- Mod_null_long %>% mutate(Year = year(year), month = month(year)) %>%
  group_by(Year, Functional_groups) %>%
  filter(Year <= 2010 & Year < 2014) %>%
  group_by(Functional_groups) %>%
  summarize(n = n(),
            mean_var = mean(value),
            sd = sd(value))

Mod_null_dt_during_mhw <- Mod_null_long %>% mutate(Year = year(year), month = month(year)) %>%
  group_by(Year, Functional_groups) %>%
  filter(Year <= 2014 & Year < 2017) %>%
  group_by(Functional_groups) %>%
  summarize(n = n(),
            mean_var = mean(value),
            sd = sd(value))


Mod_null_dt_post_mhw <- Mod_null_long %>% mutate(Year = year(year), month = month(year)) %>%
  group_by(Year, Functional_groups) %>%
  filter(Year > 2020 & Year <= 2024) %>%
  group_by(Functional_groups) %>%
  summarize(n = n(),
            mean_var = mean(value),
            sd = sd(value))

options(scipen=999)
comp_mhw <- Mod_null_dt_pre_mhw %>% 
left_join(Mod_null_dt_during_mhw, by= "Functional_groups", suffix= c("_pre_mhw", "_dur_mhw")) %>% 
  group_by(Functional_groups) %>%
  mutate(diff = (mean_var_dur_mhw- mean_var_pre_mhw)) 

qnt <- quantile(comp$diff, c(.10, .50, .90))
fil_data_posi_impact_mhw <- filter(comp_mhw, diff >= qnt[3]) # .90 percentile
fil_data_neg_impact_mhw <- filter(comp_mhw, diff <= qnt[1]) # .10 percentile
groups_posi_mhw <- fil_data_posi_impact_mhw$Functional_groups
groups_neg_mhw <- fil_data_neg_impact_mhw$Functional_groups



comp <- Mod_null_dt_pre_mhw %>% 
  left_join(Mod_null_dt_post_mhw, by= "Functional_groups", suffix= c("_pre_mhw", "_post_mhw")) %>% 
  group_by(Functional_groups) %>%
  mutate(diff = (mean_var_post_mhw- mean_var_pre_mhw)) 



# Here I am finding which groups where affected during the marine heatwave with one year lag 2015-2020
#neg_values <- comp$diff[comp$diff < 0]  # subset of x that is negative
#pos_values <- comp$diff[comp$diff > 0]  # subset of x that is positive
#
#neg_90 <- quantile(neg_values, probs = 0.10)
#pos_90 <- quantile(pos_values, probs = 0.90)


qnt <- quantile(comp$diff, c(.10, .50, .90))
fil_data_posi_impact <- filter(comp, diff >= qnt[3]) # .90 percentile
fil_data_neg_impact <- filter(comp, diff <= qnt[1]) # .10 percentile
groups_posi <- fil_data_posi_impact$Functional_groups
groups_neg <- fil_data_neg_impact$Functional_groups



Mod_null_dt2 <- Mod_null_dt%>% 
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean_var + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = mean_var - qt((1-0.95)/2, n - 1) * sem) %>% 
  left_join(comp, select_at(Functional_groups, diff), by="Functional_groups")

#--------------------------------------------------------------------

## Plots facet####
### Color palettes

library(RColorBrewer)
#n <- 3
#colrs <- brewer.pal.info[brewer.pal.info$colorblind == TRUE, ]
#col_vec = unlist(mapply(brewer.pal, colrs$maxcolors, rownames(colrs)))
#col <- sample(col_vec, n)

# Palettes ####
palettebd_gray <- gray.colors(62, start = 0.9, end = 1)
palettebd <- rev(met.brewer("Hiroshige", 62)) #Yellow
palettebd_Yellow <- rev(met.brewer("Greek", 62)) #Yellow
palettebd_green <- met.brewer("VanGogh3", 62) #Green
palettebd_blue <- rev(met.brewer("Hokusai2", 62)) #Blue
#paletteebd_blue2 <- blues.colors
cbbPalette <- c(
  "#661100",
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)

palBlues <- colorRampPalette(c("#56B4E9", "#0072B2"))(62)
palOrange <- colorRampPalette(c("#E69F00", "#D55E00"))(62)
#colscale <- scale_colour_manual(name="groups", values=palettebd)



#I had to create this by hand, I have to figure out how to make this better. And also automate this


groups_posi <- c(
  "Capelin",
  "Helmet_crab",
  "Leather_stars",
  "Mussels",
  "Pisaster_Evasterias",
  "Rockfish",
  "Snail_crust_S"
)

groups_neg <- c(
  "Herring_S",
  "Invert_eat_seaduck",
  "Pinnipeds",
  "Resident_orca",
  "Salmon_sharks",
  "Spiny_dogfish",
  "Transient_orca"
)

# hand drawn palettes- colorblind
#palbd_base <- c("#E76254", "#ED804A","#f39A4F","#F9B35D","#FFd06f", "#ffe0a4", "#d4e1cb")
#palbd_sc2 <- c("#E76254", "#ED804A","#9cd4dd", "#72bcd5", "#599ab6","#447Ba1", "#305e8b")
#palbd_sc3 <- c("#E76254", "#ED804A", "#9cd4dd", "#1e466e")


### Positive effect groups  ####


Positive_effect_groups_plot <-
  ggplot(Mod_null_dt2, aes(x = Year, y = mean_var)) +
  theme_transparent() +
  ylim(0,2) +
  xlim(1989, 2031) +
  scale_fill_manual(values = palBlues) +
  scale_color_manual(values = palBlues) +
  geom_line(aes(colour = Functional_groups), size = 1.5) +
  #turning the CI on and off.
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill= Functional_groups), alpha =0.3)+
  theme(
    axis.text.x =  element_text(size = 9, color = "black"),
    axis.title.x = element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.y = element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    axis.ticks.x = element_line(colour = "grey"),
    axis.line.x = element_line(colour = "grey"),
    axis.ticks.y = element_line(colour = "grey"),
    axis.line.y = element_line(colour = "grey"),
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(y = "Relative biomass") +
  # add this if I want to highlight groups
  gghighlight(
    Functional_groups %in% groups_posi,
    keep_scales = FALSE,
    use_group_by = FALSE,
    use_direct_label = FALSE,
    #highlighting the species I want to showcase
    unhighlighted_params = list(
      size = 1,
      colour = alpha("grey70", 0.9),
      fill = alpha("grey70", 0.9)
    )
  ) +
  annotate(
    "rect",
    xmin = 2014,
    xmax = 2016,
    ymin = -Inf,
    ymax = Inf,
    fill = "#D55E00",
    alpha = 0.6
  ) +
  annotate(
    "rect",
    xmin = 2019.01,
    xmax = 2019.90,
    ymin = -Inf,
    ymax = Inf,
    fill = "#D55E00",
    alpha = 0.4
  ) +
  facet_wrap(vars(Functional_groups), ncol = 2,
             labeller = labeller(Functional_groups = 
                                   c("Capelin"= "Capelin",
                                    "Helmet_crab"= "Helmet crab",
                                    "Leather_stars"= "Leather stars",
                                    "Mussels"= "Mussels",
                                    "Pisaster_Evasterias"= "Pisaster and Evasterias",
                                    "Rockfish"= "Rockfish",
                                    "Snail_crust_S"= "Snails"
                                   ))) +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "black"))
Positive_effect_groups_plot

ggsave(
  "FIGURES/ecosim_relativeB_plot_positive_groups_v8_ts13_3.png",
  Positive_effect_groups_plot,
  width = 7.16, 
  height = 5,
  bg = "transparent"
)


### Negative effect groups  ####


Negative_effect_groups_plot <-
  ggplot(Mod_null_dt2, aes(x = Year, y = mean_var)) +
  theme_transparent() +
  ylim(0, 2) +
  xlim(1989, 2031) +
  scale_fill_manual(values = palOrange) +
  scale_color_manual(values = palOrange) +
  geom_line(aes(colour = Functional_groups), size = 1.5) +
  #turning the CI on and off.
  #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill= Functional_groups), alpha =0.3)+
  theme(
    axis.text.x =  element_text(size = 9, color = "black"),
    axis.title.x = element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.y = element_text(
      size = 10,
      face = "bold",
      color = "black"
    ),
    axis.ticks.x = element_line(colour = "grey"),
    axis.line.x = element_line(colour = "grey"),
    axis.ticks.y = element_line(colour = "grey"),
    axis.line.y = element_line(colour = "grey"),
    legend.text = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  labs(y = "Relative biomass") +
  # add this if I want to highlight groups
  gghighlight(
    Functional_groups %in% groups_neg,
    keep_scales = FALSE,
    use_group_by = FALSE,
    use_direct_label = FALSE,
    #highlighting the species I want to showcase
    unhighlighted_params = list(
      size = 1,
      colour = alpha("grey70", 0.9),
      fill = alpha("grey70", 0.9)
    )
  ) +
  annotate(
    "rect",
    xmin = 2014,
    xmax = 2016,
    ymin = -Inf,
    ymax = Inf,
    fill = "#D55E00",
    alpha = 0.6
  ) +
  annotate(
    "rect",
    xmin = 2019.01,
    xmax = 2019.90,
    ymin = -Inf,
    ymax = Inf,
    fill = "#D55E00",
    alpha = 0.4
  ) +
  facet_wrap(vars(Functional_groups), ncol = 2,
             labeller = labeller(Functional_groups = 
                                   c("Herring_S"= "Small herring",
                                    "Invert_eat_seaduck"= "Seaducks",
                                    "Pinnipeds"= "Pinnipeds",
                                    "Resident_orca"= "Resident orca",
                                    "Salmon_sharks"= "Salmon sharks",
                                    "Spiny_dogfish"= "Dogfish",
                                    "Transient_orca"= "Transient orca"
                                   ))) +
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "black"))
Negative_effect_groups_plot

ggsave(
  "FIGURES/ecosim_relativeB_plot_negative_groups_v8_ts13_3.png",
  Negative_effect_groups_plot,
  width = 7.16, 
  height = 5,
  bg = "transparent"
)




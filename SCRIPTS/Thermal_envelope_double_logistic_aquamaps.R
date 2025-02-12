
#------------------------------------------------------------------------------#
#AUTHORS: Dias, B.S.
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#------------------------------------------------------------------------------#
## Code:    Query Aquamaps database
## Code modified from https://github.com/SEFSC/IEA-GWEM-DataSynth
##------------------------------------------------------------------------------


library(tidyverse)
library(ggthemes)
library(ggpubr)
library(ggrepel)
library(here)

## -----------------------------------------------------------------------------
##
## Logistic function

doublelogistic <- function(max = 30, steps = 1200, range = 'wide', min_abs, min_prf, max_prf, max_abs){ 
  #j = 42; max = 30; steps = 1200; range = 'nar'; min_abs = fg_pref$SalinityMin[j]; min_prf = fg_pref$SalinityPrefMin[j]; max_prf = fg_pref$SalinityPrefMax[j]; max_abs = fg_pref$SalinityMax[j]
  #j = 39; max = 30; steps = 1200; range = 'wide'; min_abs = fg_pref$DepthMin[j]; min_prf = fg_pref$DepthPrefMin[j]; max_prf = fg_pref$DepthPrefMax[j]; max_abs = fg_pref$DepthMax[j]
  mid_prf <- min_prf + (max_prf - min_prf) / 2 ## Midpoint. Change from increasing to decreasing logistic function
  mid <- ifelse(mid_prf > max, max, mid_prf)
  step_size <-  max / steps
  x1  <- seq(0, mid-step_size, by = step_size)
  x2  <- seq(mid, max, by = step_size)
  r1  <- min_prf - min_abs 
  r2  <- max_abs - max_prf 
  C1  <- min_abs + r1 / 2
  C2  <- max_prf + r2 / 2
  B1  <- ifelse(range == 'wide', 1/sqrt(r1), 1/log10(r1)) ## If range is 'wide', curve shape inversely proportional to range size
  B2  <- ifelse(range == 'wide', 1/sqrt(r2), 1/log10(r2))
  if(B1 < 0) B1 = Inf
  if(B2 < 0) B2 = Inf
  S  = 1; A1 = 0; D1 = 1; A2 = 1
  
  ## Logistic equations
  f1 <- function(x)     1 / (1 + exp(B1*(C1-x)))^S ## Increasing logistic eq.
  f2 <- function(x) 1 - 1 / (1 + exp(B2*(C2-x)))^S ## Decreasing logistic eq.
  ## 1 = value of the horizontal asymptote when x→−∞
  ## 0 = value of the horizontal asymptote when x→+∞
  ## B describes how rapidly the curve makes its transition between the two asymptotes
  ## S describes the asymmetry of the curve. The curve is symmetric when S=1. 
  ## C is a location parameter, which does not have a nice interpretation, 
  ##   unless S=1 when the curve has an inflection point at x=C.
  ##   In the case when S=1, C is the value of x for which f(x) is the midpoint between the two asymptotes
  
  y1 <- f1(x1)
  y2 <- f2(x2)
  out <- data.frame(x = c(x1, x2), y = c(y1, y2))
  out <- out[1:steps, ] 
  return(out)
}

#-----------------------------------------------
plot_pref_func <- function(p1, p2, p3, p4, fg_num, fg_name,
                           max = 400, xmin = -1, scale_xaxis = 'y', range = 'wide', driver = '') {
  pref_func <- doublelogistic(max = max, steps = max, range = range, p1, p2, p3, p4)
  xlim <- c(0,max)
  xmax <- ifelse(scale_xaxis == 'y', xlim, max)
  xmin <- ifelse(scale_xaxis == 'y', p1 - p1 * 0.15, 0)
  
  labels_df <- data.frame(x = c(p1, p2, p3, p4), y = rep(1.1, 4), label = c(p1, p2, p3, p4))
  
  gg <- ggplot(pref_func, aes(x, y)) +
    geom_line(linewidth=1.5) +
    labs(
      title = paste(driver, fg_num, fg_name),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) + 
    xlim(xmin, xmax) +
    ylim(0, 1.2) +
    scale_x_continuous(breaks = c(p1, p2, p3, p4), guide = guide_axis(n.dodge = 1)) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    geom_vline(xintercept = c(p1, p2, p3, p4), col = c("#FFC20A", "#0C7BDC", "#0C7BDC", "#FFC20A"), linetype = "dashed", linewidth= 1)
  
  return(gg)
}

fg_pref <- read.csv("DATA/fg-env-preference-parameters_v3_final.csv")

name_lookup <- c(
  "Salmon_sharks"= "Salmon shark", 
  "Halibut"= "Halibut", 
  "Pinnipeds"= "Pinnipeds", 
  "Porpoise"= "Porpoise", 
  "Lingcod"= "Lingcod",
  "Salmon_L"= "Salmon", 
  "Pacific_cod"= "Pacific cod", 
  "Sablefish" = "Sablefish", 
  "Spiny_dogfish"= "Dogfish", 
  "Pollock"= "Pollock",
  "Rockfish"= "Rockfish", 
  "Nshore_demersal"= "Nearshore demersal", 
  "Eulachon"= "Eulachon", 
  "Sea_otters"= "Sea otters", 
  "Deep _epibenthos"= "Epibenthos",
  "Herring"= "Herring"
)

plots <- list()
for (j in seq_len(nrow(fg_pref))) {
  
  fg_num  <- fg_pref$EwE_num[j]
  # retrieve original name
  fg_name_old <- fg_pref$EwE_name[j]
  
  # rename on the fly if it exists in the lookup, otherwise keep old
  fg_name_new <- if (fg_name_old %in% names(name_lookup)) {
    name_lookup[fg_name_old]
  } else {
    fg_name_old
  }
  
  driver_text <- ifelse(fg_pref$flagdem[j] == 1, "Demersal", "Pelagic")
  
  gg <- plot_pref_func(
    fg_pref$TempMin[j],
    fg_pref$TempPrefMin[j],
    fg_pref$TempPrefMax[j],
    fg_pref$TempMax[j],
    fg_num  = fg_num,
    fg_name = fg_name_new,  # pass the NEW name here
    max        = 34,
    scale_xaxis= 'y',
    range      = 'wide',
    driver     = driver_text
  )
  
  plots[[j]] <- gg
}


# Combine all plots
library(gridExtra)
library(grid)  # Load the grid package

#grid.arrange(grobs = plots, nrow = 7) 


# Split the plots list into two halves
half_length <- ceiling(length(plots) / 2)
first_half <- plots[1:half_length]
second_half <- plots[(half_length+1):length(plots)]


#pelagic_plots  <- plots[fg_pref$flagdem == 0]
#demersal_plots <- plots[fg_pref$flagdem == 1]

# Save the layout into an object
first_half_grob <- arrangeGrob(grobs = first_half, nrow = 4)
# To view the saved plot layout
grid.draw(first_half_grob)
dev.off()
# If you want to save it to a file
ggsave("FIGURES/Aquamaps_doublelogistic_temp1.png", first_half_grob, width = 10, height = 8)


# Save the layout into an object
second_half_grob <- arrangeGrob(grobs = second_half, nrow = 4)
# To view the saved plot layout
grid.draw(second_half_grob)
dev.off()
# If you want to save it to a file
ggsave("FIGURES/Aquamaps_doublelogistic_temp2.png", second_half_grob, width = 10, height = 8)


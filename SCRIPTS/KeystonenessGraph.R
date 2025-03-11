#------------------------------------------------------------------------------#
#AUTHORS: Dias, B.S.
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# Function to create keystonenes (sensu Libralato, et al. 2006) plots
#------------------------------------------------------------------------------#

#Libraries

library(zoo)
library(RODBC) 
library(tidyverse)
library(here)
library(gridExtra)
library(ggthemes)
library(janitor)
library(ggrepel)
library(patchwork)
library(cowplot)

keystone_index <-  read.csv("DATA/Keystoneness_1996_2016_vfinal.csv") 
keystone_index <- keystone_index %>%  clean_names()
######## Publication plot ##############
mytableSM <- read.csv("DATA/Keystoneness_legend3.csv", header=F)
mytableSM<-sapply(mytableSM,as.character)
mytableSM[is.na(mytableSM)]<-""
#grid.table(mytableSM) this is to make the grob
min2<-ttheme_minimal(core=list(fg_params=list(hjust=0,x=0.05,cex=0.9, lineheight=0.5)),
                     base_size = 7, padding = unit(c(1, 2), "mm")) # padding to change the spacing between rows
mytableSM2<-tableGrob(mytableSM, theme = min2, cols=NULL, rows=NULL)


#Keystoneness plot ####

# Define the years for which you want to create plots

years <- c(1996, 2000, 2010, 2025)
plots <- list()

for (yr in years) {
  
  # column names as strings
  x_var <- paste0("relative_total_impact_", yr)
  y_var <- paste0("keystone_index_1_", yr)
  size_var <- paste0("biomass_", yr)
  
  p <- ggplot(keystone_index, aes_string(x = x_var, y = y_var, 
                                         fill = paste0("-", x_var),
                                         size = size_var, 
                                         label = "node", 
                                         alpha=0.2)) +
    geom_point(show.legend = FALSE, pch = 21) +
    scale_fill_gradient(low = "#5f9ed1", high = "#a2c8ec") +
    scale_alpha(guide = "none") +
    scale_size_continuous(range = c(0.007, 20.13)) +
    geom_text_repel(size = 3, color = "black", family = 'sans',
                    box.padding = unit(0.35, 'lines'),
                    point.padding = unit(0.5, 'lines'),
                    segment.color = "#999999",
                    segment.size = 0.25,
                    force = 2,
                    max.overlaps = Inf) +
    theme_economist_white(base_size = 4, base_family = "sans", gray_bg = FALSE) +
    theme(axis.ticks.x = element_line(colour = "#999999", linewidth = 0.25),
          axis.line.x = element_line(colour = "#999999", linewidth = 0.25),
          panel.grid.major.y = element_line(linewidth = 0.25),
          legend.title = element_blank(),
          legend.text = element_blank(),
          legend.key = element_blank(),
          axis.text.x=  element_text(size = 12, color="black"),
          axis.title.x= element_text(size= 12, color="black"),
          axis.text.y= element_text(size = 12, color="black"),
          axis.title.y= element_text(size= 12,color="black"))+
    scale_y_continuous(limits = c(-3.5, 0.2)) +
    geom_hline(yintercept = 0.0, linewidth = 0.25) +
    labs(x = paste0("\n Relative total impact ", yr),
         y = expression(paste("Keystone index #1 (Libralato ", italic("et al."), " 2006)"))) 
  #+
  #  annotation_custom(mytableSM2,
  #                    xmin = 0.55, xmax = 1,
  #                    ymin = -3.2, ymax = -1)
  
  # Store the plot in the list using the year as the name
  plots[[as.character(yr)]] <- p
  
  # Optionally, store it as a separate object in your global environment too
  assign(paste0("plot", yr), p)
}


## Save each plot using ggsave
#for (yr in names(plots)) {
#  file_name <- paste0("FIGURES/keystonenessplot", yr, ".png")
#  ggsave(filename = file_name, plot = plots[[yr]], width = 8, height = 4.6, dpi = 300)
#}

# MHW Years ####

MHWyears <- c(2005,2016, 2019)
MHWplots <- list()

for (MHWyr in MHWyears) {
  
  # column names as strings
  x_var <- paste0("relative_total_impact_", MHWyr)
  y_var <- paste0("keystone_index_1_", MHWyr)
  size_var <- paste0("biomass_", MHWyr)
  
  pMHW <- ggplot(keystone_index, aes_string(x = x_var, y = y_var, 
                                         fill = paste0("-", x_var),
                                         size = size_var, 
                                         label = "node", 
                                         alpha=0.2)) +
    geom_point(show.legend = FALSE, pch = 21) +
    scale_fill_gradient(low = "#E79805", high = "#ffd06f") +
    scale_alpha(guide = "none") +
    scale_size_continuous(range = c(0.007, 20.13)) +
    geom_text_repel(size = 3, color = "black", family = 'sans',
                    box.padding = unit(0.35, 'lines'),
                    point.padding = unit(0.5, 'lines'),
                    segment.color = "#999999",
                    segment.size = 0.25,
                    force = 2,
                    max.overlaps = Inf) +
    theme_economist_white(base_size = 4, base_family = "sans", gray_bg = FALSE) +
    theme(axis.ticks.x = element_line(colour = "#999999", linewidth = 0.25),
          axis.line.x = element_line(colour = "#999999", linewidth = 0.25),
          panel.grid.major.y = element_line(linewidth = 0.25),
          legend.title = element_blank(),
          legend.text = element_blank(),
          legend.key = element_blank(),
          axis.text.x=  element_text(size = 12, color="black"),
          axis.title.x= element_text(size= 12, color="black"),
          axis.text.y= element_text(size = 12, color="black"),
          axis.title.y= element_text(size= 12,color="black"))+
    scale_y_continuous(limits = c(-3.5, 0.2)) +
    geom_hline(yintercept = 0.0, linewidth = 0.25) +
    labs(x = paste0("\n Relative total impact ", MHWyr),
         y = expression(paste("Keystone index #1 (Libralato ", italic("et al."), " 2006)"))) +
    annotation_custom(mytableSM2,
                      xmin = 0.55, xmax = 1,
                      ymin = -3.2, ymax = -1)
  
  # Store the plot in the list using the year as the name
  MHWplots[[as.character(MHWyr)]] <- pMHW
  
  # Optionally, store it as a separate object in your global environment too
  assign(paste0("plot", MHWyr), pMHW)
}


## Save each plot using ggsave
#for (MHWyr in names(MHWplots)) {
#  file_name <- paste0("FIGURES/keystonenessMHWplot", MHWyr, ".png")
#  ggsave(filename = file_name, plot = MHWplots[[MHWyr]], width = 8, height = 4.6, dpi = 300)
#}


plot_keystoneness <- plot2010 / plot2016 

#grid::grid.draw(grid::textGrob(ylab,x=0.02, rot=90))
plot_keystoneness <- plot_keystoneness+
  plot_annotation(tag_levels= list(c("A ","B ")))

ggsave("FIGURES/plot_Keystoneness_2010_2016.png",plot_keystoneness, width = 8, height = 10)





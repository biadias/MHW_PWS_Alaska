#------------------------------------------------------------------------------#
#AUTHORS: Dias, B.S.
#AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
#E-MAIL OF CORRESPONDENCE AUTHOR: bia.dias@noaa.gov
#
# Function to create niche overlap plots
#------------------------------------------------------------------------------#

library(tidyverse)
library(ggrepel)
library(ggthemes)
library(patchwork)


plot_all_niche_overlap_files <- function(folder_path, output_folder = "FIGURES", plot_suffix = "_plot") {
  # List all files ending with _processed.csv in the given folder
  processed_files <- list.files(path = folder_path, pattern = "_processed\\.csv$", full.names = TRUE)
  
  if (length(processed_files) == 0) {
    stop("No processed files ending with '_processed.csv' found in the folder.")
  }
  
  # Create the output folder path based on the provided argument
  out_folder_path <- file.path(folder_path, output_folder)
  if (!dir.exists(out_folder_path)) {
    dir.create(out_folder_path)
  }
  
  for (file in processed_files) {
    # Read the processed CSV file
    data <- read_csv(file)
    
    # Filter the data to only include rows where both Predator_overlap and Prey_overlap are > 0.5
    data <- data %>% 
      filter(Predator_overlap > 0.5, Prey_overlap > 0.5)
    
    # Create the ggplot object. Assumes the processed data contains:
    # - 'Predator_overlap' for the x-axis,
    # - 'Prey_overlap' for the y-axis,
    # - 'Labels2' for the text labels.
    plot_obj <- ggplot(data, aes(x = Predator_overlap, 
                                 y = Prey_overlap, 
                                 color = Predator_overlap, 
                                 size = Predator_overlap, 
                                 label = Nodes)) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_size_continuous(range = c(1, 5))+
      scale_colour_gradient(low = "#aadce0", high = "#6FB2C1") +
      geom_text_repel(size = 4, 
                      color = "black", 
                      family = "sans",
                      box.padding = unit(0.1, 'lines'),
                      point.padding = unit(0.25, 'lines'),
                      segment.color = "darkgrey",
                      segment.size = 0.6,
                      force = 5) +
      theme_economist_white(base_size = 10, base_family = "sans", gray_bg = FALSE) +
      theme(axis.ticks.x = element_line(colour = "darkgrey", size = 0.25),
            axis.line.x = element_line(colour = "darkgrey", size = 0.25),
            axis.text.x = element_text(colour = "black", size = 12),
            axis.title.x = element_text(colour = "black", size = 12),
            axis.text.y = element_text(colour = "black", size = 12),
            axis.title.y = element_text(colour = "black", size = 12),
            panel.grid.major.y = element_line(size = 0.25),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", colour = NA)) +
      scale_y_continuous(limits = c(0.5, 1)) +
      scale_x_continuous(limits = c(0.5, 1)) +
      labs(x = "\n Predator overlap index",
           y = expression(paste("Prey overlap index")))
    
    # Construct output file names based on the processed file name.
    base_name <- tools::file_path_sans_ext(basename(file))
    #output_svg <- file.path(out_folder_path, paste0(base_name, plot_suffix, ".svg"))
    #output_pdf <- file.path(out_folder_path, paste0(base_name, plot_suffix, ".pdf"))
    #output_png <- file.path(out_folder_path, paste0(base_name, plot_suffix, ".png"))
    
    #PNG
    #ggsave(
    #  output_png,
    #  plot = plot_obj,
    #  width = 7.2,
    #  height = 5,
    #  bg = "transparent"
    #)
    
    # Save the plot as SVG using ggsave
    #ggsave(filename = output_svg, plot = plot_obj, bg = "transparent")
    
    # Save as PDF
    #pdf(file = output_pdf, width = 7.2, height = 5, family = "Helvetica")
    #print(plot_obj)
    #dev.off()
    
    # Save as TIFF
    #tiff(filename = output_tiff, width = 7.2, height = 5, units = "in", compression = "lzw", res = 300)
    #print(plot_obj)
    #dev.off()
    
    message("Processed plot saved for file: ", file)
    
    # Assign the plot to an object in the global environment
    assign(base_name, plot_obj, envir = .GlobalEnv)
    
  }
}
#plot_all_niche_overlap_files("DATA/Niche_Overlap/", output_folder = "FIGURES/")


plot_all_niche_overlap_files_MHW <- function(folder_path, output_folder = "FIGURES", plot_suffix = "_plot") {
  # List all files ending with _processed.csv in the given folder
  processed_files <- list.files(path = folder_path, pattern = "MHW_graph_Niche[[:space:]_]?overlap_processed\\.csv$", full.names = TRUE)
  
  if (length(processed_files) == 0) {
    stop("No processed files ending with '_processed.csv' found in the folder.")
  }
  
  # Create the output folder path based on the provided argument
  out_folder_path <- file.path(folder_path, output_folder)
  if (!dir.exists(out_folder_path)) {
    dir.create(out_folder_path)
  }
  
  for (file in processed_files) {
    # Read the processed CSV file
    data <- read_csv(file)
    
    # Filter the data to only include rows where both Predator_overlap and Prey_overlap are > 0.5
    data <- data %>% 
      filter(Predator_overlap > 0.5, Prey_overlap > 0.5)
    
    # Create the ggplot object. Assumes the processed data contains:
    # - 'Predator_overlap' for the x-axis,
    # - 'Prey_overlap' for the y-axis,
    # - 'Labels2' for the text labels.
    plot_obj <- ggplot(data, aes(x = Predator_overlap, 
                                 y = Prey_overlap, 
                                 color = Predator_overlap, 
                                 size = Predator_overlap, 
                                 label = Nodes)) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_size_continuous(range = c(1, 5))+
      scale_colour_gradient(low = "#ffd06f", high = "#E79805") +
      geom_text_repel(size = 4, 
                      color = "black", 
                      family = "sans",
                      box.padding = unit(0.1, 'lines'),
                      point.padding = unit(0.25, 'lines'),
                      segment.color = "darkgrey",
                      segment.size = 0.6,
                      force = 5) +
      theme_economist_white(base_size = 10, base_family = "sans", gray_bg = FALSE) +
      theme(axis.ticks.x = element_line(colour = "darkgrey", size = 0.25),
            axis.line.x = element_line(colour = "darkgrey", size = 0.25),
            axis.text.x = element_text(colour = "black", size = 12),
            axis.title.x = element_text(colour = "black", size = 12),
            axis.text.y = element_text(colour = "black", size = 12),
            axis.title.y = element_text(colour = "black", size = 12),
            panel.grid.major.y = element_line(size = 0.25),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", colour = NA)) +
      scale_y_continuous(limits = c(0.5, 1)) +
      scale_x_continuous(limits = c(0.5, 1)) +
      labs(x = "\n Predator overlap index",
           y = expression(paste("Prey overlap index")))
    
    # Construct output file names based on the processed file name.
    base_name <- tools::file_path_sans_ext(basename(file))
    #output_svg <- file.path(out_folder_path, paste0(base_name, plot_suffix, ".svg"))
    #output_pdf <- file.path(out_folder_path, paste0(base_name, plot_suffix, ".pdf"))
    #output_png <- file.path(out_folder_path, paste0(base_name, plot_suffix, ".png"))
    
    #PNG
    #ggsave(
    #  output_png,
    #  plot = plot_obj,
    #  width = 7.2,
    #  height = 5,
    #  bg = "transparent"
    #)
    
    # Save the plot as SVG using ggsave
    #ggsave(filename = output_svg, plot = plot_obj, bg = "transparent")
    
    # Save as PDF
    #pdf(file = output_pdf, width = 7.2, height = 5, family = "Helvetica")
    #print(plot_obj)
    #dev.off()
    
    # Save as TIFF
    #tiff(filename = output_tiff, width = 7.2, height = 5, units = "in", compression = "lzw", res = 300)
    #print(plot_obj)
    #dev.off()
    
    message("Processed plot saved for file: ", file)
    
    # Assign the plot to an object in the global environment
    assign(base_name, plot_obj, envir = .GlobalEnv)
    
  }
}
plot_all_niche_overlap_files_MHW("DATA/Niche_Overlap/", output_folder = "FIGURES/")
Niche_overlap_1996 <- `Refined PWS_v8_roms_Base1990_graph_Niche overlap_v2_processed`
Niche_overlap_2010 <- `Refined PWS_v8_roms_2010_graph_Niche overlap_processed`
Niche_overlap_2016 <- `Refined PWS_v8_roms_2016_MHW_graph_Niche overlap_processed`
Niche_overlap_2019 <- `Refined PWS_v8_roms_2019MHW_graph_Niche overlap_processed`
Niche_overlap_2025 <- `Refined PWS_v8_roms_graph_2025_graph_Niche overlap_processed`

plot_niche_overlap <- Niche_overlap_2010 / Niche_overlap_2016 

#grid::grid.draw(grid::textGrob(ylab,x=0.02, rot=90))
plot_niche_overlap <- plot_niche_overlap+
  plot_annotation(tag_levels= list(c("A ","B ")))

ggsave("FIGURES/plot_niche_overlap_2010_2016.png",plot_niche_overlap, width = 8, height = 10)



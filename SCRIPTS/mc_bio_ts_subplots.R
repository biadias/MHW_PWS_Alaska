# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(here)

# Set plotting parameters
theme_set(theme_classic(base_family = "Arial"))
theme_update(text = element_text(size = 12, family = "Arial"))

process_data <- function(fname_in) {
  # Read the file content
  text <- readLines(fname_in)
  
  idx_s <- 342  #180 Starting index (adjust if needed) ignore all the lines until the first trial
  n <- length(text) #The total number of lines in the file.
  
  data_dict <- list() #Will store the processed results for each species.
  group_leg <- list() #Will store species names or labels.
  
  # Adjusted the loop to range from 1 to 59 to match the number of species(exclude the detritus groups)
  for (k in 1:59) {
    #59
    count <- 0
    data_out <- list()
    data_table <- FALSE
    data_table_start <- 0
    data_float <- list()
    idx_out <- as.character(k)
    
    for (i in idx_s:n) {
      line <- text[i]
      
      # Check for "Trial number" in the line
      if (grepl("Trial number", line)) {
        split_line <- strsplit(line, ",")[[1]]
        trial_id <- str_trim(split_line[[length(split_line)]])
        count <- count + 1
      }
      
      # Check for the start of the data table: "Ecosim biomass"
      if (grepl("Ecosim biomass", line)) {
        data_table_start <- i
        data_table <- TRUE
      }
      
      # Check for the end of the data table: "Ecosim catch"
      if (grepl("Ecosim catch", line)) {
        data_table <- FALSE
      }
      
      # Process data only if we are within the data table
      if (data_table && i > data_table_start) {
        line_split <- strsplit(str_trim(line), ",")[[1]]
        if (length(line_split) >= 2) {
          datai <- c(trial_id, line_split[1:2])
          
          if (datai[2] == idx_out) {
            group_leg[[k]] <- gsub('"', '', datai[length(datai)])
            
            temp_line <- line_split[-(1:2)]
            # Select every 12th value starting from the first
            temp <- temp_line[seq(1, length(temp_line), by = 12)]
            data_out[[length(data_out) + 1]] <- c(datai, temp)
            data_float[[length(data_float) + 1]] <- as.numeric(temp)
          }
        }
      }
    }
    
    if (length(data_float) > 0) {
      data_float_matrix <- do.call(rbind, data_float)
      n_samples <- nrow(data_float_matrix)
      
      # Calculate statistics
      data_mean <- colMeans(data_float_matrix, na.rm = TRUE)
      data_std <- apply(data_float_matrix, 2, sd, na.rm = TRUE)
      data_min <- apply(data_float_matrix, 2, min, na.rm = TRUE)
      data_max <- apply(data_float_matrix, 2, max, na.rm = TRUE)
      se <- data_std / sqrt(n_samples)
      ci_lower <- data_mean - qt(0.975, df = n_samples - 1) * se
      ci_upper <- data_mean + qt(0.975, df = n_samples - 1) * se
      
      # Store data using character keys for consistency
      data_dict[[as.character(k)]] <- list(
        mean = data_mean,
        std = data_std,
        min = data_min,
        max = data_max,
        ci = rbind(ci_lower, ci_upper),
        mean_p_std = data_mean + data_std,
        mean_n_std = data_mean - data_std
      )
    } else {
      data_dict[[as.character(k)]] <- list()
    }
  }
  
  return(list(data_dict = data_dict, group_leg = group_leg))
}

subplots <- function(data,
                     group_leg,
                     shaded_ranges = NULL,
                     vlines = NULL) {
  # Species names
  # leg_text = c("Phytoplankton", "Bacteria", "Microzooplankton", "Copepod-S", "Copepod-L", "Gelatinous zooplankton", "Micronekton", "Macrobenthos polychaetae", "Macrobenthos crustaceans", "Macrobenthos mollusks", "Other macrobenthos", "Megabenthos filters", "Other megabenthos", "Shrimp", "Mesopelagics", "Atlantic herring", "Alosine", "Atlantic mackerel", "Squid", "Butterfish", "Small pelagics", "Bluefish", "Striped bass", "Dogfish-S", "Dogfish-L", "Atlantic Cod-S", "Atlantic Cod-M", "Atlantic Cod-L", "Haddock", "Hake-S", "Hake-L", "Yellow flounder", "Summer flounder", "Skate", "Demersal benthivores", "Demersal piscivores", "Demersal omnivores", "Medium pelagics", "Pelagic sharks", "Highly migratory large pelagics", "Pinnipeds", "Baleen whales", "Odontocetes", "Seabirds")
  leg_text <- c(
    "Transient_orca" ,
    "Salmon_sharks",
    "Resident_orca" ,
    "Sleeper_sharks",
    "Halibut" ,
    "Pinnipeds",
    "Porpoise" ,
    "Lingcod",
    "Arrowtooth_L" ,
    "Salmon_L",
    "Pacific_cod" ,
    "Sablefish",
    "Arrowtooth_S" ,
    "Spiny_dogfish",
    "Avian_raptors" ,
    "Octopods",
    "Seabirds" ,
    "Deep_demersals",
    "Pollock_L" ,
    "Rockfish",
    "Baleen_whales" ,
    "Salmon_fry_S",
    "Nshore_demersal" ,
    "Squids",
    "Eulachon" ,
    "Sea_otters",
    "Deep _epibenthos" ,
    "Capelin",
    "Herring_L" ,
    "Pollock_S",
    "Invert_eat_seaduck" ,
    "Oystercatchers",
    "Sandlance" ,
    "Sunflower_stars",
    "Pisaster_Evasterias" ,
    "Leather_stars",
    "Cucumbers" ,
    "Urchins",
    "Helmet_crab" ,
    "Herring_S",
    "Jellies" ,
    "Deep_infauna_S",
    "Zoopl_near_onmiv" ,
    "Zoop_omniv",
    "Shallow_infauna_S" ,
    "Meiofauna",
    "Deep_infauna_L" ,
    "Snail_crust_S",
    "Mussels" ,
    "Barnacles",
    "Shallow_infauna_clams" ,
    "Zoopl_near_herb",
    "Zoopl_herb" ,
    "Phyto_near",
    "Phyto_off"
    ,
    "Fucus",
    "Subtidal_kelps" ,
    "Macroalgae_other",
    "Eelgrass"
  )
  leg_text2 <- setNames(leg_text, 1:55)
  
  # Colors and scenario names
  co <- c('#E69F00', '#56B4E9', 'black')
  scenario_names <- c('Scenario 1', 'Scenario 2', 'Scenario 3')
  
  # Determine the length of the time series
  # Find the first species with data to get the length
  mean_length <- NULL
  for (i in seq_along(data[[1]])) {
    if (!is.null(data[[1]][[i]]$mean)) {
      mean_length <- length(data[[1]][[i]]$mean)
      break
    }
  }
  if (is.null(mean_length)) {
    stop("No data available for plotting.")
  }
  
  year0 <- 1989
  x <- year0 + seq_len(mean_length) - 1
  
  ncol <- 2
  nrow <- 4
  
  idx_special <- c(29, 40, 1, 3, 21)
  idx_temp <- setdiff(1:55, idx_special)
  idx_all <- c(idx_special, idx_temp)
  
  total_plots <- length(idx_all)
  total_pages <- ceiling(total_plots / (ncol * nrow))
  
  # Create a directory to save plots
  if (!dir.exists("FIGURES")) {
    dir.create("FIGURES")
  }
  
  for (k in seq_len(total_pages)) {
    plot_list <- list()
    start_idx <- (k - 1) * ncol * nrow + 1
    end_idx <- min(start_idx + ncol * nrow - 1, total_plots)
    idx_page <- idx_all[start_idx:end_idx]
    
    for (idx in idx_page) {
      species_name <- leg_text2[as.character(idx)]
      df_list <- list()
      data_available <- FALSE  # Flag to check if data is available
      
      for (i in 1:length(data)) {
        plot_data <- data[[i]][[as.character(idx)]]
        if (length(plot_data) == 0 || is.null(plot_data$mean))
          next
        data_available <- TRUE
        df <- data.frame(
          x = x,
          mean = plot_data$mean,
          ci_lower = plot_data$ci[1, ],
          ci_upper = plot_data$ci[2, ],
          scenario = scenario_names[i]
        )
        df_list[[length(df_list) + 1]] <- df
      }
      
      if (!data_available) {
        # Skip plotting if no data is available for this species
        next
      }
      
      df_all <- bind_rows(df_list)
      
      p <- ggplot(df_all, aes(
        x = x,
        y = mean,
        color = scenario,
        fill = scenario
      ))
      
      # Add shaded ranges first, so they appear behind everything else
      if (!is.null(shaded_ranges)) {
        for (range_vals in shaded_ranges) {
          p <- p +
            annotate(
              "rect",
              xmin = range_vals[1],
              xmax = range_vals[2],
              ymin = -Inf,
              ymax = Inf,
              fill = "gray80",
              alpha = 0.4
            )
        }
      }
      # 2) Add vertical lines (if any)
      if (!is.null(vlines)) {
        for (vline_val in vlines) {
          p <- p + 
            geom_vline(
              xintercept = vline_val,
              color = "darkred",
              linetype = "dashed",
              alpha=0.4
            )
        }
      }
      
      # Now add ribbons and lines on top of the shaded areas
      p <- p +
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                    alpha = 0.2,
                    colour = NA) +
        geom_line(linewidth = 1) +
        labs(title = species_name,
             x = 'Year',
             y = expression(Biomass ~ (mt ~ "\u00B7" ~ km ^ {
               -2
             }))) +
        theme_classic() +
        theme(
          legend.position = 'none',
          plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10)
        ) +
        scale_color_manual(values = co) +
        scale_fill_manual(values = co)
      
      
      plot_list[[length(plot_list) + 1]] <- p
    }
    
   # # Add shaded ranges first, so they appear behind everything else
   # if (!is.null(shaded_ranges)) {
   #   for (range_vals in shaded_ranges) {
   #     p <- p +
   #       annotate(
   #         "rect",
   #         xmin = range_vals[1],
   #         xmax = range_vals[2],
   #         ymin = -Inf,
   #         ymax = Inf,
   #         fill = "gray80",
   #         alpha = 0.4
   #       )
   #   }
   # }
    
    if (length(plot_list) > 0) {
      # Arrange the plots in a grid
      grid_plots <- marrangeGrob(plot_list, nrow = nrow, ncol = ncol, top = NULL)
      # Save the plots to PDF
      ggsave(
        filename = sprintf("FIGURES/MCTrial_ci_%02d.pdf", k),
        grid_plots,
        device = cairo_pdf,
        width = 9,
        #ncol * 3,
        height = 10 #nrow * 2.5
      )
    }
  }
}

# List of files to process
flist <- c('DATA/MonteCarloTrials.csv')

# Initialize data list
data <- list()
for (i in seq_along(flist)) {
  result <- process_data(flist[i])
  data[[i]] <- result$data_dict
  if (i == 1) {
    group_leg <- result$group_leg
  }
}

# Generate subplots
subplots(data, group_leg, shaded_ranges = list(c(2014, 2016), c(2019.01, 2019.75)), vlines= 1989)

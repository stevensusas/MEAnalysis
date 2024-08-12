# Load required libraries
library(ggplot2)
library(dplyr)
library(patchwork)
library(readr)
library(stringr)

# Source the MEA Analysis functions
source("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/MEA Analysis.R")

# Hard-coded mapping for file names to DIV and minute labels
file_mapping <- c(
  "6-04-24 nxn DIV 11 PLATE 3(001)(000).csv" = "DIV 11",
  "6-05-24 nxn DIV 12 PLATE 3(000)(000).csv" = "DIV 12",
  "6-06-24 nxn DIV 13 PLATE 3(000)(000).csv" = "DIV 13",
  "div 14 nxn 6-7-24 plate 3(000)(000).csv" = "DIV 14",
  "ra nxn div 15 plate 3(002)(000).csv" = "DIV 15",
  "ra nxn div 16 plate 3(000)(000).csv" = "DIV 16",
  "ra nxn div 18 plate 3 spontaneou 5 min(001)(000).csv" = "DIV 18",
  "ra nxn div 20 plate3 spontaneous 5min(000)(000).csv" = "DIV 20",
  "ra nxn div 21 plate 3 spontaneous 5min(000)(000).csv" = "DIV 21",
  "ra nxn div 22 plate3 spontaneaous 5min(000)(000).csv" = "DIV 22",
  "ra nxn plate3 spontaneous 5min(000)(000).csv" = "DIV 24",
  "ra nxn plate3 spontaneous 5min(000)(000).csv" = "DIV 25"
)

# Function to process a single file
process_file <- function(file_path, metric) {
  df <- read_csv(file_path)
  treatment_averages <- find_treatment_averages(df)
  avg_col <- paste0(metric, " - Avg")
  std_col <- paste0(metric, " - Std")
  if (avg_col %in% rownames(treatment_averages) && std_col %in% rownames(treatment_averages)) {
    data.frame(
      Sample = colnames(treatment_averages),
      Avg = as.numeric(treatment_averages[avg_col, ]),
      Std = as.numeric(treatment_averages[std_col, ]),
      FileName = basename(file_path)
    )
  } else {
    NULL
  }
}

# Function to plot time comparison for multiple conditions
time_comparison_plots <- function(data_list, conditions, metric, control) {
  # Combine all dataframes
  combined_df <- bind_rows(data_list)
  
  # Filter for the specified conditions
  filtered_df <- combined_df %>% filter(Sample %in% conditions)
  
  # Reorder the Sample factor levels to put control on top
  filtered_df$Sample <- factor(filtered_df$Sample, 
                               levels = c(control, setdiff(conditions, control)))
  
  # Calculate the maximum y value for all conditions
  max_y <- max(filtered_df$Avg + filtered_df$Std, na.rm = TRUE)
  
  # Create the plot using facet_wrap
  p <- ggplot(filtered_df, aes(x = FileName, y = Avg, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge", fill = "grey80", color = "black", size = 0.5) +
    geom_errorbar(aes(ymin = Avg - Std, ymax = Avg + Std), width = 0.2, position = position_dodge(0.9), color = "black", size = 0.5) +
    theme_classic() +
    labs(x = "", y = metric) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 9.8, face = "bold"),
      axis.title.x = element_text(size = 8.4),
      axis.title.y = element_text(size = 8.4),
      axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 7),
      strip.background = element_blank(),
      strip.text = element_text(size = 8.4, face = "bold"),
      panel.border = element_blank(),
      axis.line = element_line(size = 1)
    ) +
    facet_wrap(~ Sample, ncol = 1, scales = "fixed") +
    coord_cartesian(ylim = c(0, max_y * 1.1))  # Set y-axis limit with 10% padding
  
  # Ensure x-axis labels are in the correct order
  p <- p + scale_x_discrete(limits = unique(filtered_df$FileName))
  
  return(p)
}

# Main function to run the analysis
run_mea_analysis <- function(file_paths, conditions, metric, control) {
  # Process all files
  data_list <- lapply(file_paths, function(path) {
    data <- process_file(path, metric)
    if (!is.null(data)) {
      data$FileName <- file_mapping[basename(path)]
    }
    return(data)
  })
  
  # Remove any NULL entries (in case the metric wasn't found in some files)
  data_list <- data_list[!sapply(data_list, is.null)]
  
  # Combine all data
  combined_data <- bind_rows(data_list)
  
  # Ensure FileName is a factor with levels in the correct order
  combined_data$FileName <- factor(combined_data$FileName, levels = unique(file_mapping))
  
  # Generate and return the plot
  plot <- time_comparison_plots(list(combined_data), conditions, metric, control)
  return(plot)
}

# File paths for Plate 3 (5 minute recordings)
file_paths <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 3/6-04-24 nxn DIV 11 PLATE 3(001)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-5-24/Plate 3/6-05-24 nxn DIV 12 PLATE 3(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-6-24/Plate 3/6-06-24 nxn DIV 13 PLATE 3(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-7-24/Plate 3/div 14 nxn 6-7-24 plate 3(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-8-24/Plate 3/ra nxn div 15 plate 3(002)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-9-24/Plate 3/ra nxn div 16 plate 3(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/ra nxn div 18 plate 3 spontaneou 5 min(001)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/ra nxn div 20 plate3 spontaneous 5min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/ra nxn div 21 plate 3 spontaneous 5min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/ra nxn div 22 plate3 spontaneaous 5min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/ra nxn plate3 spontaneous 5min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/ra nxn plate3 spontaneous 5min(000)(000).csv"
)


conditions <- c('U', 'NEG Control-HET')

# Specify the metric you want to analyze
metrics <- c("Number of Bursts", 'Weighted Mean Firing Rate (Hz)', 'Number of Network Bursts', 'Number of Spikes') 

# Specify the control condition
control <- "U"

for (metric in metrics) {
  # Run the analysis
  run_mea_analysis(file_paths, conditions, metric, control)
  ggsave(paste("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/Results/Time Course Plots Bucketed/Plate 3/5min/", paste(metric,'.png')), width = 8, height =8, units = "in")
  
}
# Load necessary libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

# Read the CSV file with more robust parsing
df <- read_csv("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Emily's Files/052124_DIV18_baseline(000)(000).csv")

# Define the function to find the first occurrence of a substring
find_first_occurrence <- function(df, substring_to_find) {
  # Convert the dataframe to a character vector (combine all columns into one string per row)
  df_char <- apply(df, 1, paste, collapse = " ")
  
  # Find the line number of the first occurrence of the substring
  line_number <- which(grepl(substring_to_find, df_char))[1]
  
  # Return the line number
  if (!is.na(line_number)) {
    return(line_number)
  } else {
    return(NA) # Return NA if the substring is not found
  }
}

# Define the function to subset the dataframe by range
subset_by_range <- function(df, start_row, end_row) {
  # Ensure the start_row and end_row are within the valid range
  if (start_row < 1 || end_row > nrow(df) || start_row > end_row) {
    stop("Invalid row range")
  }
  
  # Subset the dataframe
  subset_df <- df[start_row:end_row, ]
  return(subset_df)
}

find_sample_assignments <- function(df) {
# Use the functions to find the relevant rows and subset the dataframe
start_row <- find_first_occurrence(df, "Well Information") + 1
end_row <- find_first_occurrence(df, "Additional Information")
df_sample_assignments <- subset_by_range(df, start_row, end_row)

# Convert the subsetted dataframe back to a normal dataframe based on comma delimitation
# Split each cell by commas and create a new dataframe
new_df_sample_assignments <- df_sample_assignments %>%
  separate(`Investigator:`, into = paste0("V", 1:max(lengths(strsplit(
    df_sample_assignments$`Investigator:`, ",")))), sep = ",", fill = "right")

return(new_df_sample_assignments)

}

df_sample_assigments <- find_sample_assignments(df)

find_treatment_averages <- function(df) {

  start_row <- find_first_occurrence(df, "Treatment Averages")
  end_row <- find_first_occurrence(df, "Well Averages")
  
  df_treatment_averages <- subset_by_range(df, start_row, end_row)
  
  new_df_treatment_averages <- df_treatment_averages %>%
    separate(`Investigator:`, into = paste0("V", 1:max(lengths(strsplit(
      df_treatment_averages$`Investigator:`, ",")))), sep = ",", fill = "right")
  
  new_df_treatment_averages <- new_df_treatment_averages %>%
    select_if(~ all(!is.na(.)))
  
  return(new_df_treatment_averages)
  
}
  
df_treatment_averages <- find_treatment_averages(df)

find_well_averages <- function(df) {
  
  start_row <- find_first_occurrence(df, "Well Averages")
  end_row <- find_first_occurrence(df, "Measurement") - 1
  
  df_well_averages <- subset_by_range(df, start_row, end_row)
  
  new_df_well_averages <- df_well_averages %>%
    separate(`Investigator:`, into = paste0("V", 1:max(lengths(strsplit(
      df_well_averages$`Investigator:`, ",")))), sep = ",", fill = "right")
  
  return(new_df_well_averages)
  
}

df_well_averages <- find_well_averages(df)


get_treatment_list <- function(df) {
  treatment_row <- df_sample_assigments[df$V1 == 'Treatment', ]
  
  # Print the specific row
  specific_row_array <- as.vector(unlist(treatment_row))
  
  treatment_array <- unique(na.omit(specific_row_array))
  
  treatment_array <- treatment_array[treatment_array != ""]
  
  treatment_array <- treatment_array[treatment_array != "Treatment"]
  
  return(treatment_array)
}


find_electrode_averages <- function(df) {
  
  start_row <- find_first_occurrence(df, "Measurement")
  end_row <- nrow(df)
  
  df_electrode_averages <- subset_by_range(df, start_row, end_row)
  
  new_df_electrode_averages <- df_electrode_averages %>%
    separate(`Investigator:`, into = paste0("V", 1:max(lengths(strsplit(
      df_electrode_averages$`Investigator:`, ",")))), sep = ",", fill = "right")
  
  return(new_df_electrode_averages)
  
}

df_electrode_average <- find_electrode_averages(df)

spikes_count_treatment_average <- function(df) {
  
  samples <- get_treatment_list(df_sample_assigments)
  spikes_count_averages <- as.numeric(df[3, ])
  spikes_count_std <- as.numeric(df[4,])
  spikes_count_averages <- spikes_count_averages[-1]
  spikes_count_std <- spikes_count_std[-1]
  
  plot_data <- data.frame(Sample = samples, Avg = spikes_count_averages, Std = spikes_count_std)
  
  # Create the bar plot with error bars
  ggplot(plot_data, aes(x = Sample, y = Avg, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Avg - Std, ymax = Avg + Std), width = 0.2, position = position_dodge(0.9)) +
    theme_minimal() +
    labs(title = "Number of Spikes by Sample", x = "Sample", y = "Number of Spikes (Avg ± Std)")
}


mean_firing_rate_treatment_average <- function(df) {
  samples <- get_treatment_list(df_sample_assigments)
  mean_firing_rate <- as.numeric(df[5, ])
  mean_firing_std <- as.numeric(df[6,])
  mean_firing_rate <- mean_firing_rate[-1]
  mean_firing_std <- mean_firing_std[-1]
  
  mean_firing_rate <- na.omit(mean_firing_rate)
  
  # Convert to a numeric vector (na.omit returns a "na.omit" class object)
  mean_firing_rate <- as.vector(mean_firing_rate)
  mean_firing_std <- na.omit(mean_firing_std)
  
  # Convert to a numeric vector (na.omit returns a "na.omit" class object)
  mean_firing_std <- as.vector(mean_firing_std)
  
  plot_data <- data.frame(Sample = samples, Avg = mean_firing_rate, Std = mean_firing_std)
  
  # Create the bar plot with error bars
  ggplot(plot_data, aes(x = Sample, y = Avg, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Avg - Std, ymax = Avg + Std), width = 0.2, position = position_dodge(0.9)) +
    theme_minimal() +
    labs(title = "Mean Firing Rate by Sample", x = "Sample", y = "Mean Firing Rate (Hz ± Std)")
  
}

# Function to create the combined plot
combined_metrics_plot <- function(df, samples) {
  # Get the sample list from assignments
  samples <- get_treatment_list(samples)
  
  # Extract data for spikes count
  spikes_count_averages <- as.numeric(df[3, ])
  spikes_count_std <- as.numeric(df[4, ])
  spikes_count_averages <- spikes_count_averages[-1]
  spikes_count_std <- spikes_count_std[-1]
  
  spikes_data <- data.frame(
    Sample = samples,
    Metric = "Number of Spikes (Avg ± Std)",
    Avg = spikes_count_averages,
    Std = spikes_count_std
  )
  
  # Extract data for mean firing rate
  mean_firing_rate <- as.numeric(df[5, ])
  mean_firing_std <- as.numeric(df[6, ])
  mean_firing_rate <- mean_firing_rate[-1]
  mean_firing_std <- mean_firing_std[-1]
  
  mean_firing_rate <- na.omit(mean_firing_rate)
  mean_firing_std <- na.omit(mean_firing_std)
  
  mean_firing_data <- data.frame(
    Sample = samples,
    Metric = "Mean Firing Rate (Hz ± Std)",
    Avg = mean_firing_rate,
    Std = mean_firing_std
  )
  
  # Combine both data frames
  combined_data <- rbind(spikes_data, mean_firing_data)
  
  # Create the combined plot
  ggplot(combined_data, aes(x = Sample, y = Avg)) +
    geom_bar(stat = "identity", position = "dodge", fill = "grey80", color = "black", size = 1) +
    geom_errorbar(aes(ymin = Avg - Std, ymax = Avg + Std), width = 0.2, position = position_dodge(0.9), color = "black", size = 1) +
    theme_classic() +
    labs(title = "Metrics by Sample", x = "Sample", y = "Value") +
    facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 10),
      strip.background = element_blank(),
      strip.text = element_text(size = 12, face = "bold"),
      panel.border = element_blank(),
      axis.line = element_line(size = 1)
    )
}

combined_metrics_plot(df_treatment_averages, df_sample_assigments)

metrics <- c(
  "Number of Spikes - Avg",
  "Number of Spikes - Std",
  "Mean Firing Rate (Hz) - Avg",
  "Mean Firing Rate (Hz) - Std",
  "Number of Active Electrodes - Avg",
  "Number of Active Electrodes - Std",
  "Weighted Mean Firing Rate (Hz) - Avg",
  "Weighted Mean Firing Rate (Hz) - Std",
  "Number of Bursts - Avg",
  "Number of Bursts - Std",
  "Burst Duration - Avg (s)",
  "Burst Duration - Std (s)",
  "Inter-Burst Interval - Avg (s)",
  "Inter-Burst Interval - Std (s)",
  "Burst Frequency - Avg (Hz)",
  "Burst Frequency - Std (Hz)",
  "Normalized Duration IQR - Avg",
  "Normalized Duration IQR - Std",
  "IBI Coefficient of Variation - Avg",
  "IBI Coefficient of Variation - Std",
  "Burst Percentage - Avg",
  "Burst Percentage - Std",
  "Number of Network Bursts - Avg",
  "Number of Network Bursts - Std",
  "Network Burst Frequency - Avg (Hz)",
  "Network Burst Frequency - Std (Hz)",
  "Network Burst Duration - Avg (sec)",
  "Network Burst Duration - Std (sec)",
  "Number of Elecs Participating in Burst - Avg",
  "Number of Elecs Participating in Burst - Std",
  "Network Burst Percentage - Avg",
  "Network Burst Percentage - Std",
  "Network IBI Coefficient of Variation - Avg",
  "Network IBI Coefficient of Variation - Std",
  "Network Normalized Duration IQR - Avg",
  "Network Normalized Duration IQR - Std",
  "Area Under Normalized Cross-Correlation - Avg",
  "Area Under Normalized Cross-Correlation - Std",
  "Area Under Cross-Correlation - Avg",
  "Area Under Cross-Correlation - Std",
  "Resistance - Avg (kΩ)",
  "Resistance - Std (kΩ)",
  "Number of Covered Electrodes - Avg",
  "Number of Covered Electrodes - Std",
  "Weighted Mean Resistance - Avg (kΩ)",
  "Weighted Mean Resistance - Std (kΩ)"
)

# Remove " - Avg" and " - Std" and any strings after them
metrics_clean <- gsub(" - Avg.*| - Std.*", "", metrics)

# Remove duplicates
metrics_unique <- unique(metrics_clean)

treatment_averages_plot <- function(df) {
  
  for (metric in metrics_unique) {
    
  }
  
  i = 3
  
  while (i < 49)
    
  samples <- get_treatment_list(df_sample_assigments)
  mean_firing_rate <- as.numeric(df[i, ])
  mean_firing_std <- as.numeric(df[i+1,])
  mean_firing_rate <- mean_firing_rate[-1]
  mean_firing_std <- mean_firing_std[-1]
  
  mean_firing_rate <- na.omit(mean_firing_rate)
  
  # Convert to a numeric vector (na.omit returns a "na.omit" class object)
  mean_firing_rate <- as.vector(mean_firing_rate)
  mean_firing_std <- na.omit(mean_firing_std)
  
  # Convert to a numeric vector (na.omit returns a "na.omit" class object)
  mean_firing_std <- as.vector(mean_firing_std)
  
  plot_data <- data.frame(Sample = samples, Avg = mean_firing_rate, Std = mean_firing_std)
  
  # Create the bar plot with error bars
  ggplot(plot_data, aes(x = Sample, y = Avg, fill = Sample)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Avg - Std, ymax = Avg + Std), width = 0.2, position = position_dodge(0.9)) +
    theme_minimal() +
    labs(title = "Mean Firing Rate by Sample", x = "Sample", y = "Mean Firing Rate (Hz ± Std)")
  
}
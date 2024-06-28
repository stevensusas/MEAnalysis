# Load necessary libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(roxygen2)
library(ggpubr)

# Read the CSV file with more robust parsing
df <- read_csv("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Emily's Files/052124_DIV18_baseline(000)(000).csv")

# Helper: Define the function to find the first occurrence of a substring
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

# Helper: Define the function to subset the dataframe by range
subset_by_range <- function(df, start_row, end_row) {
  # Ensure the start_row and end_row are within the valid range
  if (start_row < 1 || end_row > nrow(df) || start_row > end_row) {
    stop("Invalid row range")
  }
  
  # Subset the dataframe
  subset_df <- df[start_row:end_row, ]
  return(subset_df)
}

# Helper: Find sample assignments
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
  
  new_df_sample_assignments <- as.data.frame(new_df_sample_assignments)
  rownames(new_df_sample_assignments) <- new_df_sample_assignments[, 1]
  new_df_sample_assignments <- new_df_sample_assignments[, -1]
  colnames(new_df_sample_assignments) <- new_df_sample_assignments[1, ]
  new_df_sample_assignments <- new_df_sample_assignments[-1, ]
  
  return(new_df_sample_assignments)
}

#' Find treatment averages
#'
#' @param df A dataframe containing the data.
#' @return A dataframe of treatment averages.
#' @export
find_treatment_averages <- function(df) {
  start_row <- find_first_occurrence(df, "Treatment Averages")
  end_row <- find_first_occurrence(df, "Well Averages") - 1
  
  df_treatment_averages <- subset_by_range(df, start_row, end_row)
  
  new_df_treatment_averages <- df_treatment_averages %>%
    separate(`Investigator:`, into = paste0("V", 1:max(lengths(strsplit(
      df_treatment_averages$`Investigator:`, ",")))), sep = ",", fill = "right")
  
  new_df_treatment_averages <- new_df_treatment_averages %>%
    select_if(~ all(!is.na(.)))
  
  new_df_treatment_averages <- as.data.frame(new_df_treatment_averages)
  rownames(new_df_treatment_averages) <- new_df_treatment_averages[, 1]
  new_df_treatment_averages <- new_df_treatment_averages[, -1]
  colnames(new_df_treatment_averages) <- new_df_treatment_averages[1, ]
  new_df_treatment_averages <- new_df_treatment_averages[-1, ]
  
  return(new_df_treatment_averages)
}

#' Find well averages
#'
#' @param df A dataframe containing the data.
#' @return A dataframe of well averages.
#' @export
find_well_averages <- function(df) {
  start_row <- find_first_occurrence(df, "Well Averages")
  end_row <- find_first_occurrence(df, "Measurement") - 1
  
  df_well_averages <- subset_by_range(df, start_row, end_row)
  
  new_df_well_averages <- df_well_averages %>%
    separate(`Investigator:`, into = paste0("V", 1:max(lengths(strsplit(
      df_well_averages$`Investigator:`, ",")))), sep = ",", fill = "right")
  
  new_df_well_averages <- as.data.frame(new_df_well_averages)
  rownames(new_df_well_averages) <- new_df_well_averages[, 1]
  new_df_well_averages <- new_df_well_averages[, -1]
  colnames(new_df_well_averages) <- new_df_well_averages[1, ]
  new_df_well_averages <- new_df_well_averages[-1, ]
  
  return(new_df_well_averages)
}

#' Find electrode averages
#'
#' @param df A dataframe containing the data.
#' @return A dataframe of electrode averages.
#' @export
find_electrode_averages <- function(df) {
  start_row <- find_first_occurrence(df, "Measurement")
  end_row <- nrow(df)
  
  df_electrode_averages <- subset_by_range(df, start_row, end_row)
  
  new_df_electrode_averages <- df_electrode_averages %>%
    separate(`Investigator:`, into = paste0("V", 1:max(lengths(strsplit(
      df_electrode_averages$`Investigator:`, ",")))), sep = ",", fill = "right")
  
  new_df_electrode_averages <- as.data.frame(new_df_electrode_averages)
  rownames(new_df_electrode_averages) <- new_df_electrode_averages[, 1]
  new_df_electrode_averages <- new_df_electrode_averages[, -1]
  colnames(new_df_electrode_averages) <- new_df_electrode_averages[1, ]
  new_df_electrode_averages <- new_df_electrode_averages[-1, ]
  
  return(new_df_electrode_averages)
}

#' Plot spikes count treatment averages
#'
#' @param df A dataframe containing the data.
#' @return A ggplot object displaying spikes count treatment averages.
#' @export
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

#' Plot mean firing rate treatment averages
#'
#' @param df A dataframe containing the data.
#' @return A ggplot object displaying mean firing rate treatment averages.
#' @export
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

#' Plot treatment averages
#'
#' @param df A dataframe containing the treatment averages data.
#' @return A ggplot object displaying treatment averages.
#' @export
treatment_averages_plot <- function(df) {
  samples <- get_treatment_list(df_sample_assigments)
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
  
  create_combined_plot(df, metrics, samples)
}

#' Perform t-tests on treatment averages
#'
#' @param df A dataframe containing the treatment averages data.
#' @param control_group A character string specifying the control group.
#' @return A dataframe of t-test results.
#' @export
perform_t_tests <- function(df, control_group) {
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
  
  results <- data.frame(Metric = character(), Treatment = character(), P.Value = numeric(), stringsAsFactors = FALSE)
  
  all_groups <- names(df)
  treatment_groups <- all_groups[all_groups != control_group]
  
  for (i in seq(1, length(metrics), 2)) {
    if (i + 1 <= length(metrics)) {
      metric_name <- gsub(" - Avg| - Std", "", metrics[i])
      control_mean <- as.numeric(df[metrics[i], control_group])
      control_sd <- as.numeric(df[metrics[i + 1], control_group])
      
      for (treatment in treatment_groups) {
        treatment_mean <- as.numeric(df[metrics[i], treatment])
        treatment_sd <- as.numeric(df[metrics[i + 1], treatment])
        
        n <- strtoi(df["Total Wells", treatment])
        
        # Calculate standard error
        control_se <- control_sd / sqrt(n)
        treatment_se <- treatment_sd / sqrt(n)
        
        # Perform t-test
        t_stat <- (control_mean - treatment_mean) / sqrt(control_se^2 + treatment_se^2)
        df_ttest <- (control_se^2 + treatment_se^2)^2 / ((control_se^2 / (n - 1)) + (treatment_se^2 / (n - 1)))
        p_value <- 2 * pt(-abs(t_stat), df = df_ttest)
        
        # Store the result
        results <- rbind(results, data.frame(Metric = metric_name, Treatment = treatment, P.Value = p_value, stringsAsFactors = FALSE))
      }
    }
  }
  
  return(results)
}

#' Plot treatment averages with t-test results
#'
#' @param df A dataframe containing the treatment averages data.
#' @return A ggplot object displaying treatment averages with t-test results.
#' @export
treatment_averages_t_test_plot <- function(df) {
  samples <- get_treatment_list(df_sample_assigments)
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
  
  create_combined_t_test_plot(df, metrics, samples, t_test_results)
}

# Call the plotting function
treatment_averages_t_test_plot(df_treatment_averages)

library(devtools)
devtools::document()
devtools::check()

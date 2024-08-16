source('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/MEAAnalysisClass.R')

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(scales)
library(forcats)

BatchMEAnalysis <- R6Class(
  "BatchMEAnalysis",
  inherit = MEAnalysis,  # Inherit from MEAnalysis

  public = list(
    file_paths = NULL,  # Define a public field
    metrics = c(
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
    ),
    initialize = function(data_list) {  # Define the initialize method
      self$file_paths <- data_list
    },

    process_file = function(file_path, metric) {  # Function to process a single file
      df <- read_csv(file_path)
      treatment_averages <- self$get_treatment_averages(df)
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
    },

    time_comparison_plots = function(data_list, conditions, metric, control_condition = NULL, title = NULL) {  # Function to plot time comparison for multiple conditions
      combined_df <- bind_rows(data_list)
      filtered_df <- combined_df %>% filter(Sample %in% conditions)

      # Reorder the samples to have the control condition at the top, if specified
      if (!is.null(control_condition) && control_condition %in% filtered_df$Sample) {
        filtered_df$Sample <- factor(filtered_df$Sample, levels = c(control_condition, setdiff(conditions, control_condition)))
      }

      # Remove the ".csv" suffix from the FileName
      filtered_df$FileName <- gsub("\\.csv$", "", filtered_df$FileName)

      if (is.null(title)) {
        title = ""
      }
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
        facet_wrap(~ Sample, ncol = 1)+
        ggtitle(title)

      p <- p + scale_x_discrete(limits = unique(filtered_df$FileName))
      print(p)
      return(p)
    },

    run_mea_analysis = function(conditions, metric, control_condition = NULL, title = NULL) {  # Main function to run the analysis
      data_list <- lapply(self$file_paths, function(path) {
        data <- self$process_file(path, metric)
        return(data)
      })

      data_list <- data_list[!sapply(data_list, is.null)]
      combined_data <- bind_rows(data_list)
      plot <- self$time_comparison_plots(list(combined_data), conditions, metric, control_condition, title)
      print(plot)
    },


    generate_significance_overview = function(control_group, groups_to_include = NULL, metrics_to_visualize = NULL, title) {
      file_list = self$file_paths

      # Function to process a single file
      process_file <- function(file) {
        tryCatch({
          df <- read_csv(file, show_col_types = FALSE)
          df_treatment_averages <- self$get_treatment_averages(df)
          samples <- self$get_treatment_list(df)
          if (is.null(groups_to_include)) {
            samples_to_analyze <- samples
          } else {
            # Include only the specified groups, including the control group if it's in the list
            samples_to_analyze <- intersect(samples, groups_to_include)
          }

          # Filter the treatment averages by the samples to analyze
          df_treatment_averages <- df_treatment_averages[, c(samples_to_analyze)]
          print(df_treatment_averages)

          # Filter metrics if specified
          if (!is.null(metrics_to_visualize)) {
            metric_lookup <- setNames(
              gsub(" - Avg| - Std", "", rownames(df_treatment_averages)),
              rownames(df_treatment_averages)
            )

            # Filter rows based on the lookup table
            rows_to_keep <- names(metric_lookup)[metric_lookup %in% metrics_to_visualize]
            rows_to_keep <- c(rows_to_keep, 'Total Wells')
            df_treatment_averages <- df_treatment_averages[rows_to_keep, ]
          }

          t_test_results <- self$perform_t_tests(df_treatment_averages, control_group)
          # Exclude the control group from the results
          t_test_results <- t_test_results %>% filter(Treatment != control_group)
          print(t_test_results)
          file_name <- basename(file)
          t_test_results$File <- file_name
          return(t_test_results)
        }, error = function(e) {
          warning(paste("Error processing file", file, ":", e$message))
          return(NULL)
        })
      }

      all_results <- map_df(file_list, process_file)

      if (nrow(all_results) == 0) {
        stop("No valid results were obtained from any of the input files.")
      }

      significance_table <- all_results %>%
        mutate(
          Significance = P.Value * sign(Treatment_Mean - Control_Mean),
          Heat = ((-log10(P.Value)) * sign(Treatment_Mean - Control_Mean))
        ) %>%
        mutate(Mean_Difference = Treatment_Mean - Control_Mean)

      # Apply metric filter if provided
      if (!is.null(metrics_to_visualize)) {
        significance_table <- significance_table %>% filter(Metric %in% metrics_to_visualize)
      }

      significance_table <- significance_table %>%
        select(File, Treatment, Metric, Significance, Heat, Control_Mean, Treatment_Mean, Mean_Difference) %>%
        pivot_wider(
          names_from = File,
          values_from = c(Significance, Heat, Control_Mean, Treatment_Mean, Mean_Difference),
          names_glue = "{File}_{.value}"
        ) %>%
        arrange(Treatment, Metric)

      # Prepare data for heatmap (excluding the control group)
      heatmap_data <- significance_table %>%
        pivot_longer(
          cols = contains("_Heat"),
          names_to = "File",
          values_to = "Heat"
        ) %>%
        mutate(
          File = sub("_Heat$", "", File),
          File = sub("\\.csv$", "", File),  # Remove the .csv suffix
          Heat = map_dbl(Heat, ~ ifelse(is.list(.x), unlist(.x)[1], as.numeric(.x)))
        )

      # Reorder the Treatment levels to exclude the control group
      heatmap_data <- heatmap_data %>%
        filter(Treatment != control_group) %>%
        mutate(Treatment = factor(Treatment,
                                  levels = setdiff(unique(Treatment), control_group)))

      # Create heatmap
      heatmap <- ggplot(heatmap_data, aes(x = File, y = fct_rev(interaction(Treatment, Metric)), fill = Heat)) +
        geom_tile() +
        scale_fill_gradient2(
          low = "blue",
          mid = "white",
          high = "red",
          midpoint = 0,
          limits = c(-max(abs(heatmap_data$Heat), na.rm = TRUE), max(abs(heatmap_data$Heat), na.rm = TRUE)),
          na.value = "grey50",
          name = "Heat\n(-log10(p-value))"
        ) +
        geom_text(aes(label = case_when(
          abs(Heat) >= 3 ~ "***",
          abs(Heat) >= 2 ~ "**",
          abs(Heat) >= 1.30103 ~ "*",
          TRUE ~ ""
        )),
        size = 3) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        labs(title = title)

      print(heatmap)

      return(significance_table)
    }

  )
)

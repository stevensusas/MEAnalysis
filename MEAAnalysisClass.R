library(R6)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

MEAnalysis <- R6Class(
  "MEAnalysis",
  public = list(
    raw_df = NULL,
    sample_assignments = NULL,
    treatment_averages = NULL,
    well_averages = NULL,
    treatment_list = NULL,
    electrode_averages = NULL,
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

    initialize = function(path_to_file) {
      self$raw_df <- read_csv(path_to_file)

      # Define helper functions
      find_first_occurrence <- function(df, substring_to_find) {
        df_char <- apply(df, 1, paste, collapse = " ")
        line_number <- which(grepl(substring_to_find, df_char))[1]
        if (!is.na(line_number)) {
          return(line_number)
        } else {
          return(NA)
        }
      }

      subset_by_range <- function(df, start_row, end_row) {
        if (start_row < 1 || end_row > nrow(df) || start_row > end_row) {
          stop("Invalid row range")
        }
        subset_df <- df[start_row:end_row, ]
        return(subset_df)
      }

      find_sample_assignments <- function(df) {
        start_row <- find_first_occurrence(df, "Well Information") + 1
        end_row <- find_first_occurrence(df, "Additional Information")
        df_sample_assignments <- subset_by_range(df, start_row, end_row)

        new_df_sample_assignments <- df_sample_assignments %>%
          separate(
            `Investigator:`,
            into = paste0("V", 1:max(lengths(
              strsplit(df_sample_assignments$`Investigator:`, ",")
            ))),
            sep = ",",
            fill = "right"
          )

        new_df_sample_assignments <- as.data.frame(new_df_sample_assignments)

        rownames(new_df_sample_assignments) <- new_df_sample_assignments[, 1]
        new_df_sample_assignments <- new_df_sample_assignments[, -1]
        colnames(new_df_sample_assignments) <- new_df_sample_assignments[1, ]
        new_df_sample_assignments <- new_df_sample_assignments[-1, ]

        return(new_df_sample_assignments)
      }

      self$sample_assignments <- find_sample_assignments(self$raw_df)

      get_treatment_list <- function(df_samples) {
        treatment_row <- df_samples['Treatment', ]
        specific_row_array <- as.vector(unlist(treatment_row))
        treatment_array <- unique(na.omit(specific_row_array))
        treatment_array <- treatment_array[treatment_array != ""]
        treatment_array <- treatment_array[treatment_array != "Treatment"]
        return(treatment_array)
      }

      self$treatment_list <- get_treatment_list(self$sample_assignments)

      find_treatment_averages <- function(df) {
        start_row <- find_first_occurrence(df, "Treatment Averages")
        end_row <- find_first_occurrence(df, "Well Averages") - 1

        df_treatment_averages <- subset_by_range(df, start_row, end_row)

        new_df_treatment_averages <- df_treatment_averages %>%
          separate(
            `Investigator:`,
            into = paste0("V", 1:max(lengths(
              strsplit(df_treatment_averages$`Investigator:`, ",")
            ))),
            sep = ",",
            fill = "right"
          )

        new_df_treatment_averages <- new_df_treatment_averages %>%
          select_if(~ all(!is.na(.)))

        new_df_treatment_averages <- as.data.frame(new_df_treatment_averages)

        rownames(new_df_treatment_averages) <- new_df_treatment_averages[, 1]
        new_df_treatment_averages <- new_df_treatment_averages[, -1]
        colnames(new_df_treatment_averages) <- new_df_treatment_averages[1, ]
        new_df_treatment_averages <- new_df_treatment_averages[-1, ]

        return(new_df_treatment_averages)
      }

      self$treatment_averages <- find_treatment_averages(self$raw_df)

      find_well_averages <- function(df) {
        start_row <- find_first_occurrence(df, "Well Averages")
        end_row <- find_first_occurrence(df, "Measurement") - 1

        df_well_averages <- subset_by_range(df, start_row, end_row)

        new_df_well_averages <- df_well_averages %>%
          separate(
            `Investigator:`,
            into = paste0("V", 1:max(lengths(
              strsplit(df_well_averages$`Investigator:`, ",")
            ))),
            sep = ",",
            fill = "right"
          )

        new_df_well_averages <- as.data.frame(new_df_well_averages)

        rownames(new_df_well_averages) <- new_df_well_averages[, 1]
        new_df_well_averages <- new_df_well_averages[, -1]
        colnames(new_df_well_averages) <- new_df_well_averages[1, ]
        new_df_well_averages <- new_df_well_averages[-1, ]

        return(new_df_well_averages)
      }

      self$well_averages <- find_well_averages(self$raw_df)

      find_electrode_averages <- function(df) {
        start_row <- find_first_occurrence(df, "Measurement")
        end_row <- nrow(df)

        df_electrode_averages <- subset_by_range(df, start_row, end_row)

        new_df_electrode_averages <- df_electrode_averages %>%
          separate(
            `Investigator:`,
            into = paste0("V", 1:max(lengths(
              strsplit(df_electrode_averages$`Investigator:`, ",")
            ))),
            sep = ",",
            fill = "right"
          )

        new_df_electrode_averages <- as.data.frame(new_df_electrode_averages)

        rownames(new_df_electrode_averages) <- new_df_electrode_averages[, 1]
        new_df_electrode_averages <- new_df_electrode_averages[, -1]
        colnames(new_df_electrode_averages) <- new_df_electrode_averages[1, ]
        new_df_electrode_averages <- new_df_electrode_averages[-1, ]

        return(new_df_electrode_averages)
      }

      self$electrode_averages <- find_electrode_averages(self$raw_df)
    },

    create_combined_t_test_plot = function(df, control, groups_to_plot) {
      perform_t_tests <- function(df, control_group) {
        results <- data.frame(
          Metric = character(),
          Treatment = character(),
          P.Value = numeric(),
          Control_Mean = numeric(),
          Treatment_Mean = numeric(),
          stringsAsFactors = FALSE
        )

        all_groups <- names(df)
        treatment_groups <- all_groups[all_groups != control_group]

        for (i in seq(1, length(self$metrics), 2)) {
          if (i + 1 <= length(self$metrics)) {
            metric_name <- gsub(" - Avg| - Std", "", self$metrics[i])
            control_mean <- as.numeric(df[self$metrics[i], control_group])
            control_sd <- as.numeric(df[self$metrics[i + 1], control_group])
            control_n <- as.integer(df["Total Wells", control_group])

            for (treatment in treatment_groups) {
              treatment_mean <- as.numeric(df[self$metrics[i], treatment])
              treatment_sd <- as.numeric(df[self$metrics[i + 1], treatment])
              treatment_n <- as.integer(df["Total Wells", treatment])

              # Calculate standard error
              control_se <- control_sd / sqrt(control_n)
              treatment_se <- treatment_sd / sqrt(treatment_n)

              # Perform Welch's t-test
              t_stat <- (treatment_mean - control_mean) / sqrt(control_se^2 + treatment_se^2)

              # Calculate degrees of freedom using Welch–Satterthwaite equation
              df_ttest <- (control_se^2 + treatment_se^2)^2 /
                ((control_se^2)^2 / (control_n - 1) + (treatment_se^2)^2 / (treatment_n - 1))

              # Calculate p-value
              p_value <- 2 * pt(-abs(t_stat), df = df_ttest)

              # Store the result
              results <- rbind(
                results,
                data.frame(
                  Metric = metric_name,
                  Treatment = treatment,
                  P.Value = p_value,
                  Control_Mean = control_mean,
                  Treatment_Mean = treatment_mean,
                  stringsAsFactors = FALSE
                )
              )
            }
          }
        }

        return(results)
      }

      t_test_results <- perform_t_tests(df, control)

      combined_data <- data.frame()
      for (i in seq(1, length(self$metrics), 2)) {
        if (i + 1 <= length(self$metrics)) {
          metric_avg <- as.numeric(df[self$metrics[i],])
          metric_std <- as.numeric(df[self$metrics[i + 1],])

          metric_name <- gsub(" - Avg| - Std", "", self$metrics[i])

          plot_data <- data.frame(
            Sample = names(df),
            Avg = metric_avg,
            Std = metric_std,
            Metric = metric_name
          )
          combined_data <- rbind(combined_data, plot_data)
        }
      }

      # Merge with t-test results
      combined_data <- merge(
        combined_data,
        t_test_results,
        by.x = c("Sample", "Metric"),
        by.y = c("Treatment", "Metric"),
        all.x = TRUE
      )

      combined_data$Sample <- factor(combined_data$Sample, levels = c(control, setdiff(unique(combined_data$Sample), control)))

      combined_data <- combined_data %>% filter(Sample %in% c(control, groups_to_plot))

      p <- ggplot(combined_data, aes(x = Sample, y = Avg, fill = Sample)) +
        geom_bar(
          stat = "identity",
          position = "dodge",
          fill = "grey80",
          color = "black",
          size = 0.5
        ) +
        geom_errorbar(
          aes(ymin = Avg - Std, ymax = Avg + Std),
          width = 0.2,
          position = position_dodge(0.9),
          color = "black",
          size = 0.5
        ) +
        geom_text(
          aes(
            y = Avg + Std,
            label = ifelse(Sample != control, sprintf("%.3f", P.Value), "")
          ),
          vjust = -1,
          position = position_dodge(0.9),
          size = 2.1
        ) +
        theme_classic() +
        labs(x = "", y = "") +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 9.8, face = "bold"),
          axis.title.x = element_text(size = 8.4),
          axis.title.y = element_text(size = 8.4),
          axis.text.x = element_text(
            size = 7,
            angle = 45,
            hjust = 1
          ),
          axis.text.y = element_text(size = 7),
          strip.background = element_blank(),
          strip.text = element_text(size = 8.4, face = "bold"),
          panel.border = element_blank(),
          axis.line = element_line(size = 1)
        ) +
        facet_wrap(~ Metric, ncol = 6, nrow = 4, scales = "free_y")

      return(p)
    },

    treatment_averages_t_test_plot = function(control, groups_to_plot) {
      df <- self$treatment_averages
      return(self$create_combined_t_test_plot(df, control, groups_to_plot))
    },

    get_sample_assignments = function() {
      return(self$sample_assignments)
    },

    get_treatment_averages = function() {
      return(self$treatment_averages)
    },

    get_well_averages = function() {
      return(self$well_averages)
    },

    get_treatment_list = function() {
      return(self$treatment_list)
    },

    get_electrode_averages = function() {
      return(self$electrode_averages)
    }

  )
)

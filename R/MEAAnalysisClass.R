# Load necessary libraries
library(R6)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(roxygen2)

#' @title MEAnalysis Class
#' @name MEAnalysis
#' @description This class processes Microelectrode Array (MEA) data, including
#' sample assignments, treatment averages, well averages, and electrode averages.
#' It also provides methods for performing statistical analysis and creating plots.
#' @import R6
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import ggplot2
#' @import roxygen2
#' @export
MEAnalysis <- R6Class(
  "MEAnalysis",
  public = list(
    #' @field raw_df A data frame containing the raw MEA data.
    raw_df = NULL,
    
    #' @field sample_assignments A data frame containing the sample assignments.
    sample_assignments = NULL,
    
    #' @field treatment_averages A data frame containing the treatment averages.
    treatment_averages = NULL,
    
    #' @field well_averages A data frame containing the well averages.
    well_averages = NULL,
    
    #' @field treatment_list A list of unique treatment names.
    treatment_list = NULL,
    
    #' @field electrode_averages A data frame containing the electrode averages.
    electrode_averages = NULL,
    
    #' @field metrics A character vector of metrics to be analyzed.
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
    
    #' @description
    #' Initialize an MEAnalysis Object
    #'
    #' This method initializes the MEAnalysis class with a specified file path.
    #' It reads and processes the MEA data, including sample assignments, treatment averages,
    #' well averages, and electrode averages.
    #' @param path_to_file The path to the file to be processed.
    #' @return An object of class MEAnalysis.
    #' @export
    initialize = function(path_to_file) {
      self$raw_df <- readr::read_csv(path_to_file)
      self$sample_assignments <- self$get_sample_assignments(self$raw_df)
      self$treatment_list <- self$get_treatment_list(self$raw_df)
      self$treatment_averages <- self$get_treatment_averages(self$raw_df)
      self$well_averages <- self$get_well_averages(self$raw_df)
      self$electrode_averages <- self$get_electrode_averages(self$raw_df)
    },
    
    #' @description
    #' Find First Occurrence
    #'
    #' This method finds the first occurrence of a specific substring in the data frame.
    #' @param df The data frame to search in.
    #' @param substring_to_find The substring to search for.
    #' @return The row number of the first occurrence or NA if not found.
    #' @export
    find_first_occurrence = function(df, substring_to_find) {
      df_char <- apply(df, 1, paste, collapse = " ")
      line_number <- which(grepl(substring_to_find, df_char))[1]
      if (!is.na(line_number)) {
        return(line_number)
      } else {
        return(NA)
      }
    },
    
    #' @description
    #' Subset by Range
    #'
    #' This method subsets a data frame based on the specified row range.
    #' @param df The data frame to subset.
    #' @param start_row The starting row number.
    #' @param end_row The ending row number.
    #' @return A subset of the data frame.
    #' @export
    subset_by_range = function(df, start_row, end_row) {
      if (start_row < 1 || end_row > nrow(df) || start_row > end_row) {
        stop("Invalid row range")
      }
      subset_df <- df[start_row:end_row, ]
      return(subset_df)
    },
    
    #' @description
    #' Perform T-Tests
    #'
    #' This method performs Welch's t-tests between the control group and other treatments.
    #' @param df The data frame containing the treatment averages.
    #' @param control_group The control group name.
    #' @return A data frame with the t-test results.
    #' @export
    perform_t_tests = function(df, control_group) {
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
    },
    
    #' @description
    #' Create Combined T-Test Plot
    #'
    #' This method creates a combined t-test plot for specified groups and metrics.
    #' @param df The data frame containing the treatment averages.
    #' @param control The control group name.
    #' @param groups_to_plot A vector of group names to include in the plot.
    #' @param title The title of the plot (optional).
    #' @return A ggplot object representing the t-test results.
    #' @export
    create_combined_t_test_plot = function(df, control, groups_to_plot, title) {
      if (is.null(title)) {
        title = ""
      }
      t_test_results <- self$perform_t_tests(df, control)
      combined_data <- data.frame()
      for (i in seq(1, length(self$metrics), 2)) {
        if (i + 1 <= length(self$metrics)) {
          metric_avg <- as.numeric(df[self$metrics[i], ])
          metric_std <- as.numeric(df[self$metrics[i + 1], ])
          metric_n <- as.numeric(df["Total Wells", ])
          
          # Calculate Standard Error (SE)
          metric_se <- metric_std / sqrt(metric_n)
          
          metric_name <- gsub(" - Avg| - Std", "", self$metrics[i])
          
          plot_data <- data.frame(
            Sample = names(df),
            Avg = metric_avg,
            SE = metric_se,
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
          aes(ymin = Avg - 2 * SE, ymax = Avg + 2 * SE),
          width = 0.2,
          position = position_dodge(0.9),
          color = "black",
          size = 0.5
        ) +
        geom_text(
          aes(
            y = Avg + 2 * SE,
            label = ifelse(Sample != control, sprintf("%.3f", P.Value), "")
          ),
          vjust = -1,
          position = position_dodge(0.9),
          size = 2.1
        ) +
        ggtitle(title) +
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
    
    #' @description
    #' Treatment Averages T-Test Plot
    #' 
    #' This method generates a t-test plot for treatment averages.
    #' @param control The control group name.
    #' @param groups_to_plot A vector of group names to include in the plot.
    #' @param title The title of the plot (optional).
    #' @return A ggplot object representing the t-test results for treatment averages.
    #' @export
    treatment_averages_t_test_plot = function(control, groups_to_plot, title = NULL) {
      df <- self$treatment_averages
      return(self$create_combined_t_test_plot(df, control, groups_to_plot, title))
    },
    
    #' @description
    #' Get Sample Assignments
    #'
    #' This method extracts the sample assignments from the MEA data.
    #' @param df The data frame containing the raw MEA data.
    #' @return A data frame with sample assignments.
    #' @export
    get_sample_assignments = function(df) {
      if (!is.null(self$sample_assignments)) {
        return(self$sample_assignments)
      } else {
        start_row <- self$find_first_occurrence(df, "Well Information") + 1
        end_row <- self$find_first_occurrence(df, "Additional Information")
        df_sample_assignments <- self$subset_by_range(df, start_row, end_row)
        
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
    },
    
    #' @description
    #' Get Treatment Averages
    #'
    #' This method extracts the treatment averages from the MEA data.
    #' @param df The data frame containing the raw MEA data.
    #' @return A data frame with treatment averages.
    #' @export
    get_treatment_averages = function(df) {
      if (!is.null(self$treatment_averages)) {
        return(self$treatment_averages)
      } else {
        start_row <- self$find_first_occurrence(df, "Treatment Averages")
        end_row <- self$find_first_occurrence(df, "Well Averages") - 1
        
        df_treatment_averages <- self$subset_by_range(df, start_row, end_row)
        
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
    },
    
    #' @description
    #' Get Well Averages
    #'
    #' This method extracts the well averages from the MEA data.
    #' @param df The data frame containing the raw MEA data.
    #' @return A data frame with well averages.
    #' @export
    get_well_averages = function(df) {
      if (!is.null(self$well_averages)) {
        return(self$well_averages)
      } else {
        start_row <- self$find_first_occurrence(df, "Well Averages")
        end_row <- self$find_first_occurrence(df, "Measurement") - 1
        
        df_well_averages <- self$subset_by_range(df, start_row, end_row)
        
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
    },
    
    #' @description
    #' Get Treatment List
    #'
    #' This method returns a list of unique treatment names from the sample assignments.
    #' @param df The data frame containing the raw MEA data.
    #' @return A vector of unique treatment names.
    #' @export
    get_treatment_list = function(df) {
      if (!is.null(self$treatment_list)) {
        return(self$treatment_list)
      } else {
        df_samples = self$get_sample_assignments(df)
        treatment_row <- df_samples['Treatment', ]
        specific_row_array <- as.vector(unlist(treatment_row))
        treatment_array <- unique(na.omit(specific_row_array))
        treatment_array <- treatment_array[treatment_array != ""]
        treatment_array <- treatment_array[treatment_array != "Treatment"]
        return(treatment_array)
      }
    },
    
    #' @description
    #' Get Electrode Averages
    #'
    #' This method extracts the electrode averages from the MEA data.
    #' @param df The data frame containing the raw MEA data.
    #' @return A data frame with electrode averages.
    #' @export
    get_electrode_averages = function(df) {
      if (!is.null(self$electrode_averages)) {
        return(self$electrode_averages)
      } else {
        start_row <- self$find_first_occurrence(df, "Measurement")
        end_row <- nrow(df)
        
        df_electrode_averages <- self$subset_by_range(df, start_row, end_row)
        
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
    },
    
    #' @description
    #' Remove a well
    #'
    #' This method reomves a well's data from a given treatment.
    #' @param well The well to be removed.
    #' @param treatment The treatment to remove the well from.
    #' @return None
    #' @export
    remove_well = function(well, treatment) {
      # 1. Find the column belonging to the treatment parameter in the treatment dataframe
      treatment_col <- as.numeric(self$treatment_averages[[treatment]])
      
      avg_elements <- grep(" - Avg", rownames(self$well_averages), value = TRUE)
      std_elements <- grep(" - Std", rownames(self$well_averages), value = TRUE)
      
      set_B <- c(avg_elements, std_elements)
      set_A <- setdiff(self$metrics, set_B)
      
      for (i in seq_len(nrow(self$treatment_averages))) {
        row_name <- rownames(self$treatment_averages)[i]
        
        if (row_name == "Total Wells") {
          next  # Skip "Total Wells"
        }
        
        if (grepl(" - Avg", row_name) && any(grepl(paste(set_A, collapse = "|"), row_name))) {
          well_col <- as.numeric(self$well_averages[[well]])
          base_row_name <- gsub(" - Avg", "", row_name)
          base_row_value <- as.numeric(self$well_averages[base_row_name, well])
          
          total_wells <- as.numeric(self$treatment_averages["Total Wells", treatment])
          
          if (!is.na(total_wells) && total_wells > 1) {
            updated_avg <- (treatment_col[i] * total_wells - base_row_value) / (total_wells - 1)
            self$treatment_averages[i, treatment] <- updated_avg
          }
          
        } else if (grepl(" - Avg", row_name) && any(grepl(paste(set_B, collapse = "|"), row_name))) {
          well_col <- as.numeric(self$well_averages[[well]])
          base_row_value <- as.numeric(self$well_averages[row_name, well])
          
          total_wells <- as.numeric(self$treatment_averages["Total Wells", treatment])
          if (!is.na(total_wells) && total_wells > 1) {
            updated_avg <- (treatment_col[i] * total_wells - base_row_value) / (total_wells - 1)
            self$treatment_averages[i, treatment] <- updated_avg
          }
          
        } else if (grepl(" - Std", row_name) && any(grepl(paste(set_A, collapse = "|"), row_name))) {
          well_col <- as.numeric(self$well_averages[[well]])
          base_row_name <- gsub(" - Std", "", row_name)
          base_row_value <- as.numeric(self$well_averages[base_row_name, well])
          
          total_wells <- as.numeric(self$treatment_averages["Total Wells", treatment])
          if (total_wells == 2) {
            updated_std <- 0  # Set the standard deviation to 0
          } else if (!is.na(total_wells) && total_wells > 2) {
            updated_std <- sqrt((treatment_col[i]^2 * (total_wells - 1) - base_row_value^2) / (total_wells - 2))
          }
          self$treatment_averages[i, treatment] <- updated_std
          
        } else if (grepl(" - Std", row_name) && any(grepl(paste(set_B, collapse = "|"), row_name))) {
          well_col <- as.numeric(self$well_averages[[well]])
          base_row_value <- as.numeric(self$well_averages[row_name, well])
          
          total_wells <- as.numeric(self$treatment_averages["Total Wells", treatment])
          if (total_wells == 2) {
            updated_std <- 0  # Set the standard deviation to 0
          } else if (!is.na(total_wells) && total_wells > 2) {
            updated_std <- sqrt((treatment_col[i]^2 * (total_wells - 1) - base_row_value^2) / (total_wells - 2))
          }
          self$treatment_averages[i, treatment] <- updated_std
        }
      }
      
      # Update the "Total Wells" value after processing all the rows
      self$treatment_averages["Total Wells", treatment] <- as.numeric(self$treatment_averages["Total Wells", treatment]) - 1
      
      return(NULL)
    }
    
  )
)

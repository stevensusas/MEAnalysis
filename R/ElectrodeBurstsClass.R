# Load necessary libraries
library(R6)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(scales)
library(forcats)
library(ggpubr)
library(roxygen2)

#' @title ElectrodeBursts Class
#' @name ElectrodeBursts
#' @description This class processes MEA data related to electrode bursts, including
#' sample assignments and burst data. It provides methods to create raster plots
#' and comparison plots across different treatments.
#' @import R6
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import readr
#' @import ggplot2
#' @import scales
#' @import forcats
#' @import roxygen2
#' @export
ElectrodeBursts <- R6Class(
  "ElectrodeBursts",
  inherit = MEAnalysis,  # Inherit from MEAnalysis
  
  public = list(
    #' @field data A data frame containing the processed MEA data.
    data = NULL,
    
    #' @field assignments A data frame containing the sample assignments.
    assignments = NULL,
    
    #' @description
    #' Initialize an ElectrodeBursts Object
    #'
    #' This method initializes the ElectrodeBursts class with a specified file path.
    #' It reads and processes the MEA data and assignments from the file.
    #' @param filepath The path to the file to be processed.
    #' @return An object of class ElectrodeBursts.
    #' 
    initialize = function(filepath) {
      df <- readr::read_csv(filepath)
      
      # Process assignments
      start_row <- self$find_first_occurrence(df, "Well Information") + 1
      end_row <- self$find_first_occurrence(df, "Concentration")
      assignments <- self$subset_by_range(df, start_row, end_row)
      last_column_name <- names(assignments)[ncol(assignments)]
      
      # Split the last column into multiple columns
      split_columns <- strsplit(assignments[[last_column_name]], ",")
      max_length <- max(sapply(split_columns, length))
      split_df <- as.data.frame(do.call(rbind, lapply(split_columns, `length<-`, max_length)))
      
      # Generate new column names
      new_col_names <- paste0(last_column_name, "_", seq_len(max_length))
      colnames(split_df) <- new_col_names
      
      # Remove the original last column and add the new split columns
      assignments <- assignments[, -ncol(assignments)]
      assignments <- cbind(assignments, split_df)
      colnames(assignments) <- c(colnames(assignments)[1:(ncol(assignments)-max_length)], new_col_names)
      
      # Set the first row as column names and remove it from the data
      colnames(assignments) <- assignments[1, ]
      assignments <- assignments[-1, ]
      
      # Reset row names
      rownames(assignments) <- NULL
      
      self$assignments <- assignments
      
      start_row <- 1
      end_row <- self$find_first_occurrence(df, "Well Information") - 1
      data <- self$subset_by_range(df, start_row, end_row)
      data <- data[, -c(1, 2)]
      self$data <- data
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
    #' Get Sample Assignments
    #'
    #' This method returns the processed sample assignments.
    #' @return A data frame with sample assignments.
    #' 
    get_sample_assignment = function() {
      return(self$assignments)
    },
    
    #' @description
    #' Get MEA Data
    #'
    #' This method returns the processed MEA data.
    #' @return A data frame with MEA data.
    #' 
    get_data = function() {
      return(self$data)
    },
    
    #' @description
    #' Create Raster Plot
    #'
    #' This method creates a raster plot for a specified treatment.
    #' @param treatment The treatment name to be plotted.
    #' @return A ggplot object representing the raster plot.
    #' 
    create_raster_plot = function(treatment) {
      if (!"Treatment" %in% self$assignments$Well) {
        stop("'Treatment' row not found in assignments data frame.")
      }
      
      # Get the treatment row
      treatment_row <- self$assignments[self$assignments$Well == "Treatment", ]
      
      # Find wells with the specified treatment
      treatment_wells <- names(treatment_row)[treatment_row == treatment]
      
      if (length(treatment_wells) == 0) {
        stop(paste("Treatment '", treatment, "' not found in assignments data."))
      }
      
      # Prepare the data
      plot_data <- self$data %>%
        dplyr::mutate(
          Well = sub("_.*", "", Electrode),
          Electrode = sub(".*_", "", Electrode),
          `Time (s)` = as.numeric(`Time (s)`),
          `Size (spikes)` = as.numeric(`Size (spikes)`),
          `Duration (s)` = as.numeric(`Duration (s)`)
        ) %>%
        dplyr::filter(Well %in% treatment_wells)
      
      if (nrow(plot_data) == 0) {
        stop("No data found for the specified treatment wells.")
      }
      
      # Create the plot
      ggplot2::ggplot(plot_data, ggplot2::aes(x = `Time (s)`, y = Electrode)) +
        ggplot2::geom_tile(ggplot2::aes(width = `Duration (s)`, height = 0.8, fill = `Size (spikes)`)) +
        ggplot2::facet_grid(Well ~ ., scales = "free_y", space = "free_y") +
        ggplot2::scale_fill_gradient(low = "red", high = "black") +
        ggplot2::scale_y_discrete(limits = rev(unique(plot_data$Electrode))) +
        ggplot2::labs(x = "Time (s)", y = "Electrode", fill = "Spike Size") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 6),
          strip.text = ggplot2::element_text(size = 10, face = "bold"),
          panel.spacing = ggplot2::unit(1, "lines"),
          panel.background = ggplot2::element_rect(fill = "white", color = NA),
          plot.background = ggplot2::element_rect(fill = "white", color = NA)
        )
    },
    
    #' @description
    #' Create Raster
    #'
    #' This method creates a raster plot for specified wells and treatment.
    #' @param wells The wells to be included in the plot.
    #' @param treatment The treatment name for the plot title.
    #' @param x_lim The x-axis limits for the plot.
    #' @return A ggplot object representing the raster plot.
    #' 
    create_raster = function(wells, treatment, x_lim) {
      plot_data <- self$data %>%
        dplyr::mutate(
          Well = sub("_.*", "", Electrode),
          Electrode = sub(".*_", "", Electrode),
          `Time (s)` = as.numeric(`Time (s)`),
          `Size (spikes)` = as.numeric(`Size (spikes)`),
          `Duration (s)` = as.numeric(`Duration (s)`)
        ) %>%
        dplyr::filter(Well %in% wells)
      
      if (nrow(plot_data) == 0) {
        warning(paste("No data found for treatment:", treatment))
        return(NULL)
      }
      
      ggplot2::ggplot(plot_data, ggplot2::aes(x = `Time (s)`, y = Electrode)) +
        ggplot2::geom_tile(ggplot2::aes(width = `Duration (s)`, height = 0.8, fill = `Size (spikes)`)) +
        ggplot2::facet_grid(Well ~ ., scales = "free_y", space = "free_y") +
        ggplot2::scale_fill_gradient(low = "red", high = "black") +
        ggplot2::scale_y_discrete(limits = rev(unique(plot_data$Electrode))) +
        ggplot2::scale_x_continuous(limits = x_lim) +
        ggplot2::labs(title = treatment, x = NULL, y = "Electrode", fill = "Spike Size") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 6),
          strip.text = ggplot2::element_text(size = 10, face = "bold"),
          panel.spacing = ggplot2::unit(1, "lines"),
          panel.background = ggplot2::element_rect(fill = "white", color = NA),
          plot.background = ggplot2::element_rect(fill = "white", color = NA),
          plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold")
        )
    },
    
    #' @description
    #' Create Comparison Raster Plot
    #'
    #' This method creates a comparison raster plot for multiple treatments
    #' including a control group.
    #' @param control_group The control group to be included in the comparison.
    #' @param treatments_array An array of treatment names to compare.
    #' @param plot_title The title of the plot.
    #' @return A ggpubr object representing the combined raster plots.
    #' 
    create_comparison_raster_plot = function(control_group, treatments_array, plot_title) {
      if (!"Treatment" %in% self$assignments$Well) {
        warning("'Treatment' row not found in assignments data frame.")
        return(NULL)
      }
      
      # Get the treatment row
      treatment_row <- self$assignments[self$assignments$Well == "Treatment", ]
      
      # Ensure control group is included in treatments_array
      if (!(control_group %in% treatments_array)) {
        treatments_array <- c(control_group, treatments_array)
      } else {
        treatments_array <- c(control_group, setdiff(treatments_array, control_group))
      }
      
      # Filter out treatments that are not present in the data
      valid_treatments <- treatments_array[treatments_array %in% unlist(treatment_row)]
      if (length(valid_treatments) == 0) {
        warning("No valid treatments found in the data.")
        return(NULL)
      }
      
      # Prepare data for all valid treatments
      all_data <- lapply(valid_treatments, function(treatment) {
        wells <- names(treatment_row)[treatment_row == treatment]
        if (length(wells) == 0) {
          warning(paste("Treatment '", treatment, "' not found in assignments data. Skipping."))
          return(NULL)
        }
        self$data %>%
          dplyr::mutate(
            Well = sub("_.*", "", Electrode),
            `Time (s)` = as.numeric(`Time (s)`)
          ) %>%
          dplyr::filter(Well %in% wells)
      })
      
      # Remove NULL entries from all_data
      all_data <- all_data[!sapply(all_data, is.null)]
      valid_treatments <- valid_treatments[!sapply(all_data, is.null)]
      
      if (length(all_data) == 0) {
        warning("No valid data for any treatments.")
        return(NULL)
      }
      
      # Calculate overall x-axis limits
      x_min <- min(sapply(all_data, function(df) min(df$`Time (s)`, na.rm = TRUE)))
      x_max <- max(sapply(all_data, function(df) max(df$`Time (s)`, na.rm = TRUE)))
      x_lim <- c(x_min, x_max)
      
      # Create individual plots for each treatment
      plot_list <- mapply(function(treatment, data) {
        wells <- names(treatment_row)[treatment_row == treatment]
        self$create_raster(wells, treatment, x_lim)
      }, valid_treatments, all_data, SIMPLIFY = FALSE)
      
      # Remove NULL entries from plot_list
      plot_list <- plot_list[!sapply(plot_list, is.null)]
      
      if (length(plot_list) == 0) {
        warning("No valid plots created.")
        return(NULL)
      }
      
      # Combine plots vertically
      combined_plot <- ggpubr::ggarrange(
        plotlist = plot_list,
        ncol = 1, 
        nrow = length(plot_list),
        common.legend = TRUE, 
        legend = "right",
        heights = rep(1, length(plot_list))
      
      # Add overall title and x-axis label
      combined_plot <- ggpubr::annotate_figure(combined_plot,
                                               top = ggpubr::text_grob(plot_title, size = 14, face = "bold"),
                                               bottom = ggpubr::text_grob("Time (s)", size = 12))
      
      return(combined_plot)
    }
  )
)
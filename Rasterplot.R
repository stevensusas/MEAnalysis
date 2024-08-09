library(R6)
source("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/MEA Analysis.R")

ElectrodeBursts <- R6Class("ElectrodeBurst",
  public = list(
    data = NULL,
    assignments = NULL,

    # The initialize method is called when a new object is created
    initialize = function(filepath) {
      df = read_csv(filepath)
      # Process assignments
      start_row <- find_first_occurrence(df, "Well Information") + 1
      end_row <- find_first_occurrence(df, "Concentration")
      assignments = subset_by_range(df, start_row, end_row)
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

      self$assignments = assignments


      start_row = 1
      end_row = find_first_occurrence(df, "Well Information") - 1
      data = subset_by_range(df, start_row, end_row)
      data <- data[, -c(1, 2)]
      self$data = data

    },

    get_sample_assignment = function() {
      return(self$assignments)
    },

    get_data = function() {
      return(self$data)
    },

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

    create_comparison_raster_plot = function(control, treatment) {
      if (!"Treatment" %in% self$assignments$Well) {
        stop("'Treatment' row not found in assignments data frame.")
      }

      # Get the treatment row
      treatment_row <- self$assignments[self$assignments$Well == "Treatment", ]

      # Find wells for control and treatment
      control_wells <- names(treatment_row)[treatment_row == control]
      treatment_wells <- names(treatment_row)[treatment_row == treatment]

      if (length(control_wells) == 0) {
        stop(paste("Control '", control, "' not found in assignments data."))
      }
      if (length(treatment_wells) == 0) {
        stop(paste("Treatment '", treatment, "' not found in assignments data."))
      }

      # Function to create a single raster plot
      create_raster <- function(wells, title) {
        plot_data <- self$data %>%
          dplyr::mutate(
            Well = sub("_.*", "", Electrode),
            Electrode = sub(".*_", "", Electrode),
            `Time (s)` = as.numeric(`Time (s)`),
            `Size (spikes)` = as.numeric(`Size (spikes)`),
            `Duration (s)` = as.numeric(`Duration (s)`)
          ) %>%
          dplyr::filter(Well %in% wells)

        # Print data types for debugging
        print(sapply(plot_data, class))

        # Remove rows with NA or non-finite values
        plot_data <- plot_data %>%
          dplyr::filter(!is.na(`Size (spikes)`) & is.finite(`Size (spikes)`))

        if (nrow(plot_data) == 0) {
          stop(paste("No valid data for", title))
        }

        ggplot2::ggplot(plot_data, ggplot2::aes(x = `Time (s)`, y = Electrode)) +
          ggplot2::geom_tile(ggplot2::aes(width = `Duration (s)`, height = 0.8, fill = `Size (spikes)`)) +
          ggplot2::facet_grid(. ~ Well, scales = "free", space = "free") +
          ggplot2::scale_fill_gradient(low = "red", high = "black") +
          ggplot2::scale_y_discrete(limits = rev(unique(plot_data$Electrode))) +
          ggplot2::labs(x = NULL, y = "Electrode", fill = "Spike Size", title = title) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 6),
            strip.text = ggplot2::element_text(size = 10, face = "bold"),
            panel.spacing = ggplot2::unit(1, "lines"),
            panel.background = ggplot2::element_rect(fill = "white", color = NA),
            plot.background = ggplot2::element_rect(fill = "white", color = NA),
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
          )
      }

      # Create individual plots
      control_plot <- create_raster(control_wells, "Control")
      treatment_plot <- create_raster(treatment_wells, "Treatment")

      # Combine plots
      combined_plot <- ggpubr::ggarrange(
        control_plot, treatment_plot,
        ncol = 1, nrow = 2,
        common.legend = TRUE, legend = "right",
        heights = c(1, 1)
      )

      # Add overall x-axis label
      combined_plot <- ggpubr::annotate_figure(combined_plot,
                                               bottom = ggpubr::text_grob("Time (s)", size = 12))

      return(combined_plot)
    }
  )
)

# Creating an object of the class Person
bursts <- ElectrodeBursts$new('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 1/6-04-24 nxn DIV 11 PLATE 1(002)(000)_electrode_burst_list.csv')
data <- bursts$data
assignments <- bursts$assignments


# Generate the raster plot for a specific treatment
raster_plot <- bursts$create_raster_plot("Luciferase-WT")

# Display the plot
print(raster_plot)

comparison_plot <- bursts$create_comparison_raster_plot("NEG Control-WT", 'NEG Control-HET')

print(comparison_plot)

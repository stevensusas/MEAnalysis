source("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/MEA Analysis.R")
library(ggplot2)
library(dplyr)
library(patchwork)
library(tools)

#Challenge1: dealing with file names metadata
#Challenge2: dealing with sample assignment names

#focus on Plate 1 for now

# Hard-coded filename mapping for plate1


plate1_mapping <- c(
  "6-04-24 nxn DIV 11 PLATE 1(002)(001).csv" = "DIV11 5 Min",
  "6-05-24 nxn DIV 12 PLATE 1(001)(001).csv" = "DIV12 5 Min",
  "6-06-24 nxn DIV 13 PLATE 1(000)(001).csv" = "DIV13 5 Min",
  "div 14 nxn 6-7-24 plate 1(000)(000).csv" = "DIV14 5 Min",
  "ra nxn div 15 plate 1(000)(000).csv" = "DIV15 5 Min",
  "ra nxn div 16 plate 1(003)(000).csv" = "DIV16 5 MIn",
  "ra nxn div 17 plate1 spontaneous(001)(000).csv" = "DIV17 5 Min",
  "ra nxn div 18 plate 1 spontaneous 20min(000)(000).csv" = "DIV18 20 Min",
  "ra nxn div 18 plate 1 spontaneous 30min(000)(000).csv" = "DIV18 30 Min",
  "ra nxn div 20 plate1 spontaneous 5min(000)(000).csv" = "DIV20 5 Min",
  "ra nxn div 20 plate1 spontaneous 30min(000)(000).csv" = "DIV20 30 Min",
  "ra nxn div 20 plate1 spontaneous 60min(000)(000).csv" = "DIV20 60 Min",
  "ra nxn div 21 spontaneous 5min(000)(000).csv" = "DIV21 5 Min",
  "ra nxn div 21 spontaneous 30min(000)(000).csv" = "DIV21 30 Min",
  "ra nxn div 21 spontaneous 50min(000)(000).csv" = "DIV21 50 Min",
  "ra nxn div 22 plate1 spontaneous 30min(000)(000).csv" = "DIV22 30 Min",
  "ra nxn div 22 plate1 spontaneous(000)(000).csv" = "DIV22 5 Min",
  "ra nxn div 22 plate1spontaneaous 60min(000)(000).csv" = "DIV22 60 Min",
  "ra nxn plate1cspontaneous 5min(000)(000).csv" = "DIV24 5 Min",
  "ra nxn plate1cspontaneous 30min(000)(000).csv" = "DIV24 30 Min",
  "ra nxn plate1cspontaneous 30min(001)(000).csv" = "DIV24 35 Min",
  "ra nxn plate1cspontaneous 5min(000)(000).csv" = "DIV25 5 Min",
  "ra nxn plate1cspontaneous 30min(000)(000).csv" = "DIV25 30 Min",
  "ra nxn plate1cspontaneous 30min(001)(000).csv" = "DIV25 35 Min"
)

plate3_mapping <- c(
  "6-04-24 nxn DIV 11 PLATE 3(001)(000).csv" = "DIV11 5 Min",
  "6-04-24 nxn DIV 11 PLATE 3(001)(001).csv" = "DIV11 10 Min",
  "6-05-24 nxn DIV 12 PLATE 3(000)(000).csv" = "DIV12 5 Min",
  "6-05-24 nxn DIV 12 PLATE 3(000)(001).csv" = "DIV12 10 Min",
  "6-06-24 nxn DIV 13 PLATE 3(000)(000).csv" = "DIV13 5 Min",
  "6-06-24 nxn DIV 13 PLATE 3(000)(001).csv" = "DIV13 10 Min",
  "div 14 nxn 6-7-24 plate 3(000)(000).csv" = "DIV14 5 Min",
  "ra nxn div 15 plate 3(002)(000).csv" = "DIV15 5 Min",
  "ra nxn div 16 plate 3(000)(000).csv" = "DIV16 5 Min",
  "ra nxn div 16 plate 3(001)(000).csv" = "DIV16 10 Min",
  "ra nxn div 17 plate3 spontaneous 30min in(000)(000).csv" = "DIV17 30 Min",
  "ra nxn div 17 plate3 spontaneous 30min in(001)(000).csv" = "DIV17 35 Min",
  "ra nxn div 18 plate 3 spontaeous 3 30min(000)(000).csv" = "DIV18 30 Min",
  "ra nxn div 18 plate 3 spontaeous 25 min(000)(000).csv" = "DIV18 25 Min",
  "ra nxn div 18 plate 3 spontaneou 5 min(001)(000).csv" = "DIV18 5 Min",
  "ra nxn div 20 plate3 spontaneous 5min(000)(000).csv" = "DIV20 5 Min",
  "ra nxn div 20 plate3 spontaneous 30min(000)(000).csv" = "DIV20 30 Min",
  "ra nxn div 20 plate3 spontaneous 60min(000)(000).csv" = "DIV20 60 Min",
  "ra nxn div 21 plate 3 spontaneous 5min(000)(000).csv" = "DIV21 5 Min",
  "ra nxn div 21 plate 3 spontaneous 30min(000)(000).csv" = "DIV21 30 Min",
  "ra nxn div 21 plate 3 spontaneous 30min(001)(000).csv" = "DIV21 35 Min",
  "ra nxn div 22 plate3 spontaneaous 5min(000)(000).csv" = "DIV22 5 Min",
  "ra nxn div 22 plate3 spontaneaous 5min(001)(000).csv" = "DIV22 10 Min",
  "ra nxn div 22 plate3 spontaneaous 30min(000)(000).csv" = "DIV22 30 Min",
  "ra nxn plate3 spontaneous 5min(000)(000).csv" = "DIV24 5 Min",
  "ra nxn plate3 spontaneous 30min(000)(000).csv" = "DIV24 30 Min",
  "ra nxn plate3 spontaneous 35min(000)(000).csv" = "DIV24 35 Min",
  "ra nxn plate3 spontaneous 5min(000)(000).csv" = "DIV25 5 Min",
  "ra nxn plate3 spontaneous 30min(000)(000).csv" = "DIV25 30 Min",
  "ra nxn plate3 spontaneous 35min(000)(000).csv" = "DIV25 35 Min"
)

# Call the plotting function
#treatment_averages_t_test_plot(df_treatment_averages)
generate_significance_table <- function(file_list, control_group, groups_to_omit = c(), metric_to_filter = NULL, filename_mapping) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(forcats)
  
  # Function to process a single file
  process_file <- function(file) {
    tryCatch({
      df <- read_csv(file, show_col_types = FALSE)
      df_treatment_averages <- find_treatment_averages(df)
      samples <- get_treatment_list(find_sample_assignments(df))
      samples_to_analyze <- setdiff(samples, groups_to_omit)
      df_treatment_averages <- df_treatment_averages[, c(control_group, samples_to_analyze)]
      t_test_results <- perform_t_tests(df_treatment_averages, control_group)
      
      file_name <- basename(file)
      abbreviated_file_name <- filename_mapping[file_name]
      if (is.null(abbreviated_file_name)) {
        warning(paste("No mapping found for file:", file_name))
        abbreviated_file_name <- file_name
      }
      
      t_test_results$File <- abbreviated_file_name
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
  
  # Filter out treatment groups with less than 3 characters, except for "U"
  all_results <- all_results %>%
    filter(nchar(Treatment) >= 3 | Treatment == "U")
  
  significance_table <- all_results %>%
    mutate(
      Significance = case_when(
        P.Value < 0.05 & Treatment_Mean > Control_Mean ~ "***",
        P.Value < 0.08 & Treatment_Mean > Control_Mean ~ "**",
        P.Value < 0.2 & Treatment_Mean > Control_Mean ~ "*",
        P.Value < 0.05 & Treatment_Mean < Control_Mean ~ "-***",
        P.Value < 0.08 & Treatment_Mean < Control_Mean ~ "-**",
        P.Value < 0.2 & Treatment_Mean < Control_Mean ~ "-*",
        TRUE ~ NA_character_
      ),
      Heat = -log10(P.Value) * sign(Treatment_Mean - Control_Mean)
    ) %>%
    mutate(Mean_Difference = Treatment_Mean - Control_Mean)
  
  # Apply metric filter if provided
  if (!is.null(metric_to_filter)) {
    significance_table <- significance_table %>% filter(Metric == metric_to_filter)
  }
  
  significance_table <- significance_table %>%
    select(File, Treatment, Metric, Significance, Heat, Control_Mean, Treatment_Mean, Mean_Difference) %>%
    pivot_wider(
      names_from = File,
      values_from = c(Significance, Heat, Control_Mean, Treatment_Mean, Mean_Difference),
      names_glue = "{File}_{.value}"
    ) %>%
    arrange(Treatment, Metric)
  
  # Prepare data for heatmap
  heatmap_data <- significance_table %>%
    pivot_longer(
      cols = contains("_Heat"),
      names_to = "File",
      values_to = "Heat"
    ) %>%
    mutate(
      File = sub("_Heat$", "", File),
      Heat = map_dbl(Heat, ~ ifelse(is.list(.x), unlist(.x)[1], as.numeric(.x)))
    )
  
  # Reorder the Treatment levels to put control group on top
  heatmap_data <- heatmap_data %>%
    mutate(Treatment = factor(Treatment, 
                              levels = c(control_group, 
                                         setdiff(unique(Treatment), control_group))))
  
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
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(title = "Significance Heatmap")
  
  print(heatmap)
  
  return(significance_table)
}

plate1 <- c(
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 1/ra nxn div 18 plate 1 spontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/ra nxn div 20 plate1 spontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/ra nxn div 21 spontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/ra nxn div 22 plate1 spontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/ra nxn plate1cspontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/ra nxn plate1cspontaneous 30min(000)(000).csv'
)

plate3 <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-10-24/Plate 3/ra nxn div 17 plate3 spontaneous 30min in(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/ra nxn div 18 plate 3 spontaeous 3 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/ra nxn div 20 plate3 spontaneous 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/ra nxn div 21 plate 3 spontaneous 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/ra nxn div 22 plate3 spontaneaous 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/ra nxn plate3 spontaneous 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/ra nxn plate3 spontaneous 30min(000)(000).csv"
)



get_strings_ending_with_WT_and_NEG_HET_and_short <- function(input_array) {
  # Use grep to find strings ending with "WT"
  WT_strings <- grep("WT$", input_array, value = TRUE)
  
  # Find "NEG Control-HET" in the input array
  NEG_HET <- input_array[input_array == "NEG Control-HET"]
  
  # Find strings with less than 3 characters
  short_strings <- input_array[nchar(input_array) < 3]
  
  # Combine WT strings, NEG Control-HET, and short strings
  result <- unique(c(WT_strings, NEG_HET, short_strings))
  
  return(result)
}


get_strings_ending_with_HET_and_NEG_WT_and_short <- function(input_array) {
  # Use grep to find strings ending with "WT"
  WT_strings <- grep("WT$", input_array, value = TRUE)
  
  # Find "NEG Control-HET" in the input array
  NEG_HET <- input_array[input_array == "NEG Control-HET"]
  
  # Find strings with less than 3 characters
  short_strings <- input_array[nchar(input_array) < 3]
  
  # Combine WT strings, NEG Control-HET, and short strings
  result <- unique(c(WT_strings, NEG_HET, short_strings))
  
  return(result)
}

get_array_excluding_values <- function(input_array, exclude_value1, exclude_value2) {
  # Use logical indexing to select elements not equal to either exclude_value
  result <- input_array[input_array != exclude_value1 & input_array != exclude_value2]
  
  return(result)
}




# Define your control group
control_group <- "Luciferase-HET"

# Define groups to omit
groups_to_omit <- get_strings_ending_with_HET_and_NEG_WT_and_short(get_treatment_list(find_sample_assignments(read_csv(plate1[1]))))

groups_to_omit <- append(groups_to_omit, 'U')
#groups_to_omit <- get_array_excluding_values(get_treatment_list(find_sample_assignments(read_csv(plate3[1]))), "U", "NEG Control-HET")

specific_metric <- "Mean Firing Rate (Hz)"

# Example usage with plate3
significance_table <- generate_significance_table(plate3, control_group, groups_to_omit, specific_metric, plate3_mapping)
print(significance_table)

#setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/Results')

#write.csv(significance_table, file = "Plate 3 Luc HET vs Depletion HET Sig Table.csv", row.names = FALSE)

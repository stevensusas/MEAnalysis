library(MEAnalysis)
library(ggplot2)



plate3_5min <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 3/Plate 3 DIV 11 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-5-24/Plate 3/Plate 3 DIV 12 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-6-24/Plate 3/Plate 3 DIV 13 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-7-24/Plate 3/Plate 3 DIV 14 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-8-24/Plate 3/Plate 3 DIV 15 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-9-24/Plate 3/Plate 3 DIV 16 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-10-24/Plate 3/Plate 3 DIV 17 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/Plate 3 DIV 18 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/Plate 3 DIV 20 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/Plate 3 DIV 21 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/Plate 3 DIV 22 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/Plate 3 DIV 24 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/Plate 3 DIV 25 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 26/Plate 3/Plate 3 DIV 26 Min 5.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 27/Plate 3/Plate 3 DIV 27 Min 5.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 28/Plate 3/Plate 3 DIV 28 Min 5.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 29/Plate 3/Plate 3 DIV 29 Min 5.csv"
)

plate3_30min <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/Plate 3 DIV 18 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/Plate 3 DIV 20 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/Plate 3 DIV 21 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/Plate 3 DIV 22 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/Plate 3 DIV 24 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/Plate 3 DIV 25 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 26/Plate 3/Plate 3 DIV 26 Min 30.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 27/Plate 3/Plate 3 DIV 27 Min 30.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 28/Plate 3/Plate 3 DIV 28 Min 30.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 29/Plate 3/Plate 3 DIV 29 Min 30.csv"
)

MEAnalysis_modified <- MEAnalysis$new()


MEAnalysis$set("public", "remove_f4", function() {
  # Number of wells for NEG Control - HET
  num_wells <- as.numeric(self$treatment_averages["Total Wells", "NEG Control-HET"])
  
  # Helper functions
  recalculate_avg <- function(current_avg, f4_value, num_wells) {
    (current_avg * num_wells - f4_value) / (num_wells - 1)
  }
  
  recalculate_std <- function(current_avg, current_std, f4_value, num_wells) {
    sum_squared_dev <- (current_std ^ 2) * num_wells
    f4_dev_squared <- (f4_value - current_avg) ^ 2
    new_sum_squared_dev <- sum_squared_dev - f4_dev_squared
    sqrt(new_sum_squared_dev / (num_wells - 2))
  }
  
  # Get F4 values from well_averages
  f4_values <- self$well_averages %>%
    filter(Well == "F4")
  
  # Use the predefined metrics from the class
  for (metric in self$metrics) {
    if (grepl("- Avg$", metric)) {
      std_metric <- gsub("- Avg$", "- Std", metric)
      
      f4_value <- as.numeric(f4_values[[metric]])
      
      current_avg <- as.numeric(self$treatment_averages[metric, "NEG Control-HET"])
      current_std <- as.numeric(self$treatment_averages[std_metric, "NEG Control-HET"])
      
      new_avg <- recalculate_avg(current_avg, f4_value, num_wells)
      new_std <- recalculate_std(current_avg, current_std, f4_value, num_wells)
      
      self$treatment_averages[metric, "NEG Control-HET"] <- new_avg
      self$treatment_averages[std_metric, "NEG Control-HET"] <- new_std
    }
  }
  
  # Optionally, you can add a message to confirm the operation
  cat("F4 well has been removed from NEG Control-HET averages and standard deviations.\n")
})


plot = MEAnalysis$new("/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 29/Plate 3/Plate 3 DIV 29 Min 30.csv")
control = 'U'
samples = c('U', 'NEG Control-HET')
table = plot$get_treatment_averages()
samples = plot$get_sample_assignments()
wells = plot$get_well_averages()




# 
# for (file in plate3_5min) {
#   tryCatch({
#     plot = MEAnalysis$new(file)
#     control = 'U'
#     samples = c('U', 'NEG Control-HET')
#     title = basename(file)
#     title = sub("\\.csv$", "", title)
#     plot = plot$treatment_averages_t_test_plot(control, samples, title)
#     setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Plate 3 NEG vs HET Rerun')
#     ggsave(paste(title, '.pdf', sep = ""), plot = plot, width = 15, height = 15, dpi = 300)
#   }, error = function(e) {
#     cat("Error in file:", file, "\nMessage:", e$message, "\n")
#   })
# }
# 
# for (file in plate3_30min) {
#   tryCatch({
#     plot = MEAnalysis$new(file)
#     control = "U"
#     samples = c('U', 'NEG Control-HET')
#     title = basename(file)
#     title = sub("\\.csv$", "", title)
#     plot = plot$treatment_averages_t_test_plot(control, samples, title)
#     setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Plate 3 NEG vs HET Rerun')
#     ggsave(paste(title, '.pdf', sep = ""), plot = plot, width = 15, height = 15, dpi = 300)
#   }, error = function(e) {
#     cat("Error in file:", file, "\nMessage:", e$message, "\n")
#   })
# }
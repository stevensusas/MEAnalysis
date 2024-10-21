library(MEAnalysis)
library(ggplot2)


files <- c("DIV11 5m .csv", "DIV12 5m .csv", "DIV13 5m .csv", "DIV14 5m .csv", "DIV15 5m .csv",
           "DIV16 5m .csv", "DIV17 5m .csv", "DIV18 5m .csv", "DIV19 5m .csv", "DIV20 5m .csv",
           "DIV21 5m .csv", "DIV22 5m .csv", "DIV24 5m .csv", "DIV25 5m .csv", "DIV26 5m .csv",
           "DIV27 5m .csv", "DIV28 5m .csv", "DIV29 5m .csv")

file_names <- c(
  "DIV29 5m Burst.csv",
  "DIV28 5m Burst.csv",
  "DIV27 5m Burst.csv",
  "DIV26 5m Burst.csv",
  "DIV25 5m Burst.csv",
  "DIV24 5m Burst.csv",
  "DIV22 5m Burst.csv",
  "DIV21 5m Burst.csv",
  "DIV20 5m Burst.csv",
  "DIV19 5m Burst.csv",
  "DIV18 5m Burst.csv",
  "DIV17 5m Burst.csv",
  "DIV16 5m Burst.csv",
  "DIV15 5m Burst.csv",
  "DIV14 5m Burst.csv",
  "DIV13 5m Burst.csv",
  "DIV12 5m Burst.csv",
  "DIV11 5m Burst.csv"
)

for (file in files) {
  tryCatch({
    setwd('/Users/stevensu/Desktop/Korb Lab/Plate 3 Rerun/Plate3_F4-omitted')
    plot = MEAnalysis$new(file)
    control = 'NEG Control-WT'
    samples = c('NEG Control-WT', 'NEG Control-HET')
    title = paste('Plate 3', basename(file), sep = " ")
    title = sub("\\.csv$", "", title)
    plot = plot$treatment_averages_t_test_plot(control, samples, title)
    setwd('/Users/stevensu/Desktop/Korb Lab/Plate 3 Rerun/Barplots')
    ggsave(paste(title, '.pdf', sep = ""), plot = plot, width = 15, height = 15, dpi = 300)
  }, error = function(e) {
    cat("Error in file:", file, "\nMessage:", e$message, "\n")
  })
}

for (file in file_names) {
  tryCatch({
    setwd('/Users/stevensu/Desktop/Korb Lab/Plate 3 Rerun/Plate3_F4-omitted')
    plot = ElectrodeBursts$new(file)
    control = 'NEG Control-WT'
    samples = c('NEG Control-WT', 'NEG Control-HET')
    title = basename(file)
    title = sub("\\.csv$", "", title)
    
    plot = plot$create_comparison_raster_plot(control, samples, title)
    setwd('/Users/stevensu/Desktop/Korb Lab/Plate 3 Rerun/Bursts')
    ggsave(paste(title, '.pdf', sep = ""), plot = plot, width = 20, height = 10, dpi = 300)
  }, error = function(e) {
    cat("Error in file:", file, "\nMessage:", e$message, "\n")
  })
}

metrics <- c('Number of Bursts', 'Number of Network Bursts', 'Number of Spikes', 'Weighted Mean Firing Rate (Hz)')

for (metric in metrics) {
  setwd('/Users/stevensu/Desktop/Korb Lab/Plate 3 Rerun/Plate3_F4-omitted')
  control = "NEG Control-WT"
  conditions = c("NEG Control-WT", 'NEG Control-HET')
  obj = BatchMEAnalysis$new(files)
  obj$run_mea_analysis(conditions, metric, control, "Plate 3 Min 5")
  setwd('/Users/stevensu/Desktop/Korb Lab/Plate 3 Rerun/Timecourse')
  ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 15, dpi = 300)
  
}


metrics <- c('Number of Bursts', 'Number of Network Bursts', 'Number of Spikes', 'Weighted Mean Firing Rate (Hz)')

setwd('/Users/stevensu/Desktop/Korb Lab/Plate 3 Rerun/Plate3_F4-omitted')
p3min5 = BatchMEAnalysis$new(files)
control = "NEG Control-WT"
conditions = c("NEG Control-WT", 'NEG Control-HET')
table = p3min5$generate_significance_overview(control, conditions, metrics, 'Plate 3 Min 5')
setwd('/Users/stevensu/Desktop/Korb Lab/Plate 3 Rerun/Sig')
ggsave(paste('Plate 3 Min 5', '.pdf', sep = ""), width = 10, height = 10, dpi = 300)
write.csv(table, paste('Plate 3 Min 5', '_sig_table.csv', sep = ""), row.names = TRUE)


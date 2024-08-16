source('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/MEAAnalysisClass.R')
source('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/BatchMEAnalysisClass.R')

plate1_5min <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 1/Plate 1 DIV 11 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-5-24/Plate 1/Plate 1 DIV 12 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-6-24/Plate 1/Plate 1 DIV 13 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-7-24/Plate 1/Plate 1 DIV 14 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-8-24/Plate 1/Plate 1 DIV 15 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-9-24/Plate 1/Plate 1 DIV 16 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-10-24/Plate 1/Plate 1 DIV 17 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/Plate 1 DIV 20 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/Plate 1 DIV 21 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/Plate 1 DIV 22 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/Plate 1 DIV 24 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/Plate 1 DIV 25 5 Min.csv"
)

plate1_30min <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 1/Plate 1 DIV 18 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/Plate 1 DIV 20 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/Plate 1 DIV 21 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/Plate 1 DIV 22 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/Plate 1 DIV 24 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/Plate 1 DIV 25 30 Min.csv"
)

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
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/Plate 3 DIV 25 5 Min.csv"
)

plate3_30min <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/Plate 3 DIV 18 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/Plate 3 DIV 20 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/Plate 3 DIV 21 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/Plate 3 DIV 22 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/Plate 3 DIV 24 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/Plate 3 DIV 25 30 Min.csv"
)

metrics <- c('Number of Bursts', 'Number of Network Bursts', 'Number of Spikes', 'Weighted Mean Firing Rate (Hz)')

for (metric in metrics) {
  control = "NEG Control-WT"
  conditions = c("NEG Control-WT", 'NEG Control-HET')
  obj = BatchMEAnalysis$new(plate1_5min)
  obj$run_mea_analysis(conditions, metric, control, "Plate 1 Min 5")
  setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 1 Time Course 5 Min/NEG v HET')
  ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 15, dpi = 300)

}

for (metric in metrics) {
  control = "Luciferase-WT"
  conditions = c("ASH1L-WT", "CHD8-WT", "DNMT3A-WT", "KDM6B-WT", "KMT2C-WT", "Luciferase-WT", "MBD5-WT", "MED13L-WT", "NSD1-WT", "SETD5-WT", "TBR1-WT")
  obj = BatchMEAnalysis$new(plate1_5min)
  obj$run_mea_analysis(conditions, metric, control, "Plate 1 Min 5")
  setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 1 Time Course 5 Min/LUC WT v DEP WT')
  ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 20, dpi = 300)
}

for (metric in metrics) {
  control = "Luciferase-HET"
  conditions = c("ASH1L-HET", "CHD8-HET", "DNMT3A-HET", "KDM6B-HET", "KMT2C-HET", "Luciferase-HET", "MBD5-HET", "MED13L-HET", "NSD1-HET", "SETD5-HET", "TBR1-HET")
 obj = BatchMEAnalysis$new(plate1_5min)
 obj$run_mea_analysis(conditions, metric, control, "Plate 1 Min 5")
 setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 1 Time Course 5 Min/LUC HET v DEP HET')
 ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 20, dpi = 300)
}


for (metric in metrics) {
  control = "NEG Control-WT"
  conditions = c("NEG Control-WT", 'NEG Control-HET')
  obj = BatchMEAnalysis$new(plate1_30min)
  obj$run_mea_analysis(conditions, metric, control, "Plate 1 Min 30")
  setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 1 Time Course 30 Min/NEG v HET')
  ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 15, dpi = 300)
}

for (metric in metrics) {
  control = "Luciferase-WT"
  conditions = c("ASH1L-WT", "CHD8-WT", "DNMT3A-WT", "KDM6B-WT", "KMT2C-WT", "Luciferase-WT", "MBD5-WT", "MED13L-WT", "NSD1-WT", "SETD5-WT", "TBR1-WT")
  obj = BatchMEAnalysis$new(plate1_30min)
  obj$run_mea_analysis(conditions, metric, control, "Plate 1 Min 30")
  setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 1 Time Course 30 Min/LUC WT v DEP WT')
  ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 20, dpi = 300)
}

for (metric in metrics) {
  control = "Luciferase-HET"
  conditions = c("ASH1L-HET", "CHD8-HET", "DNMT3A-HET", "KDM6B-HET", "KMT2C-HET", "Luciferase-HET", "MBD5-HET", "MED13L-HET", "NSD1-HET", "SETD5-HET", "TBR1-HET")
  obj = BatchMEAnalysis$new(plate1_30min)
  obj$run_mea_analysis(conditions, metric, control, "Plate 1 Min 30")
  setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 1 Time Course 30 Min/LUC HET v DEP HET')
  ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 20, dpi = 300)
}


for (metric in metrics) {
  control = "U"
  conditions = c("U", 'NEG Control-HET')
  obj = BatchMEAnalysis$new(plate3_5min)
  obj$run_mea_analysis(conditions, metric, control, "Plate 3 Min 5")
  setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 3 Time Course 5 Min/NEG v HET')
  ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 15, dpi = 300)

}
  for (metric in metrics) {
    control = "Luciferase-HET"
    conditions = c("ASH1L-HET", "CHD8-HET", "DNMT3A-HET", "KDM6B-HET", "KMT2C-HET", "Luciferase-HET", "MBD5-HET", "MED13L-HET", "NSD1-HET", "SETD5-HET", "TBR1-HET")
    obj = BatchMEAnalysis$new(plate3_5min)
    obj$run_mea_analysis(conditions, metric, control, "Plate 3 Min 5")
    setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 3 Time Course 5 Min/LUC HET v DEP HET')
    ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 20, dpi = 300)
  }

for (metric in metrics) {
  control = "U"
  conditions = c("U", 'NEG Control-HET')
  obj = BatchMEAnalysis$new(plate3_30min)
  obj$run_mea_analysis(conditions, metric, control, "Plate 3 Min 30")
  setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 3 Time Course 30 Min/NEG v HET')
  ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 15, dpi = 300)
}

  for (metric in metrics) {
    control = "Luciferase-HET"
    conditions = c("ASH1L-HET", "CHD8-HET", "DNMT3A-HET", "KDM6B-HET", "KMT2C-HET", "Luciferase-HET", "MBD5-HET", "MED13L-HET", "NSD1-HET", "SETD5-HET", "TBR1-HET")
    obj = BatchMEAnalysis$new(plate3_30min)
    obj$run_mea_analysis(conditions, metric, control, "Plate 3 Min 30")
    setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Plate 3 Time Course 30 Min/LUC HET v DEP HET')
    ggsave(paste(metric, '.pdf', sep = ""), width = 15, height = 20, dpi = 300)
  }

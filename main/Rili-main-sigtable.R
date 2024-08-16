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
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 10/Plate 1/Plate 1 DIV 10 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/Plate 1 DIV 21 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/Plate 1 DIV 22 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/Plate 1 DIV 24 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/Plate 1 DIV 25 5 Min.csv"
)

plate1_30min <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 1/Plate 1 DIV 18 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 10/Plate 1/Plate 1 DIV 10 30 Min.csv",
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
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 10/Plate 3/Plate 3 DIV 10 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/Plate 3 DIV 21 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/Plate 3 DIV 22 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/Plate 3 DIV 24 5 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/Plate 3 DIV 25 5 Min.csv"
)

plate3_30min <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/Plate 3 DIV 18 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 10/Plate 3/Plate 3 DIV 10 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/Plate 3 DIV 21 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/Plate 3 DIV 22 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/Plate 3 DIV 24 30 Min.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/Plate 3 DIV 25 30 Min.csv"
)

metrics <- NULL


p1min5 = BatchMEAnalysis$new(plate1_5min)
control = "NEG Control-WT"
conditions = c("NEG Control-WT", 'NEG Control-HET')
table = p1min5$generate_significance_overview(control, conditions, metrics, 'Plate 1 Min 5')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 1 5 Min NEG v HET Sig')
write.csv(table, paste('Plate 1 Min 5', '_sig_table.csv', sep = ""), row.names = TRUE)


p1min5 = BatchMEAnalysis$new(plate1_5min)
control = "Luciferase-WT"
conditions = c("ASH1L-WT", "CHD8-WT", "DNMT3A-WT", "KDM6B-WT", "KMT2C-WT", "Luciferase-WT", "MBD5-WT", "MED13L-WT", "NSD1-WT", "SETD5-WT", "TBR1-WT")
table = p1min5$generate_significance_overview(control, conditions, metrics, 'Plate 1 Min 5')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 1 5 Min LUC WT v DEP WT Sig')
write.csv(table, paste('Plate 1 Min 5', '_sig_table.csv', sep = ""), row.names = TRUE)

p1min5 = BatchMEAnalysis$new(plate1_5min)
control = "Luciferase-HET"
conditions = c("ASH1L-HET", "CHD8-HET", "DNMT3A-HET", "KDM6B-HET", "KMT2C-HET", "Luciferase-HET", "MBD5-HET", "MED13L-HET", "NSD1-HET", "SETD5-HET", "TBR1-HET")
table = p1min5$generate_significance_overview(control, conditions, metrics, 'Plate 1 Min 5')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 1 5 Min LUC HET v DEP HET Sig')
write.csv(table, paste('Plate 1 Min 5', '_sig_table.csv', sep = ""), row.names = TRUE)


p1min30 = BatchMEAnalysis$new(plate1_30min)
control = "NEG Control-WT"
conditions = c("NEG Control-WT", 'NEG Control-HET')
table = p1min30$generate_significance_overview(control, conditions, metrics, 'Plate 1 Min 30')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 1 30 Min NEG v HET Sig')
write.csv(table, paste('Plate 1 Min 30', '_sig_table.csv', sep = ""), row.names = TRUE)


p1min30 = BatchMEAnalysis$new(plate1_30min)
control = "Luciferase-WT"
conditions = c("ASH1L-WT", "CHD8-WT", "DNMT3A-WT", "KDM6B-WT", "KMT2C-WT", "Luciferase-WT", "MBD5-WT", "MED13L-WT", "NSD1-WT", "SETD5-WT", "TBR1-WT")
table = p1min30$generate_significance_overview(control, conditions, metrics, 'Plate 1 Min 30')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 1 30 Min LUC WT v DEP WT Sig')
write.csv(table, paste('Plate 1 Min 30', '_sig_table.csv', sep = ""), row.names = TRUE)


p1min30 = BatchMEAnalysis$new(plate1_30min)
control = "Luciferase-HET"
conditions = c("ASH1L-HET", "CHD8-HET", "DNMT3A-HET", "KDM6B-HET", "KMT2C-HET", "Luciferase-HET", "MBD5-HET", "MED13L-HET", "NSD1-HET", "SETD5-HET", "TBR1-HET")
table = p1min30$generate_significance_overview(control, conditions, metrics, 'Plate 1 Min 30')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 1 30 Min LUC HET v DEP HET Sig')
write.csv(table, paste('Plate 1 Min 30', '_sig_table.csv', sep = ""), row.names = TRUE)


p3min5 = BatchMEAnalysis$new(plate3_5min)
control = "U"
conditions = c("U", 'NEG Control-HET')
table = p3min5$generate_significance_overview(control, conditions, metrics, 'Plate 3 Min 5')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 3 5 Min NEG v HET Sig')

write.csv(table, paste('Plate 3 Min 5', '_sig_table.csv', sep = ""), row.names = TRUE)

p3min5 = BatchMEAnalysis$new(plate3_5min)
control = "Luciferase-HET"
conditions = c("ASH1L-HET", "CHD8-HET", "DNMT3A-HET", "KDM6B-HET", "KMT2C-HET", "Luciferase-HET", "MBD5-HET", "MED13L-HET", "NSD1-HET", "SETD5-HET", "TBR1-HET")
table = p3min5$generate_significance_overview(control, conditions, metrics, 'Plate 3 Min 5')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 3 5 Min LUC HET v DEP HET Sig')
write.csv(table, paste('Plate 3 Min 5', '_sig_table.csv', sep = ""), row.names = TRUE)

p3min30 = BatchMEAnalysis$new(plate3_30min)
control = "U"
conditions = c("U", 'NEG Control-HET')
table = p3min30$generate_significance_overview(control, conditions, metrics, 'Plate 3 Min 30')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 3 30 Min NEG v HET Sig')
write.csv(table, paste('Plate 3 Min 30', '_sig_table.csv', sep = ""), row.names = TRUE)

p3min30 = BatchMEAnalysis$new(plate3_30min)
control = "Luciferase-HET"
conditions = c("ASH1L-HET", "CHD8-HET", "DNMT3A-HET", "KDM6B-HET", "KMT2C-HET", "Luciferase-HET", "MBD5-HET", "MED13L-HET", "NSD1-HET", "SETD5-HET", "TBR1-HET")
table = p3min30$generate_significance_overview(control, conditions, metrics, 'Plate 3 Min 30')
setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Rili Dataset Final Results/Sigs/Plate 3 30 Min LUC HET v DEP HET Sig')
write.csv(table, paste('Plate 3 Min 30', '_sig_table.csv', sep = ""), row.names = TRUE)



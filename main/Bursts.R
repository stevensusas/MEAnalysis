library(MEAnalysis)
library(readr)
library(dplyr)
library(ggplot2)

plate3_5min <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/Plate 3 DIV 24 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/Plate 3 DIV 24 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/Plate 3 DIV 22 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/Plate 3 DIV 21 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/Plate 3 DIV 20 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/Plate 3 DIV 18 Min 10 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-9-24/Plate 3/Plate 3 DIV 16 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-8-24/Plate 3/Plate 3 DIV 15 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-7-24/Plate 3/Plate 3 DIV 14 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-6-24/Plate 3/Plate 3 DIV 13 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-5-24/Plate 3/Plate 3 DIV 12 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 3/Plate 3 DIV 11 Min 5 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 26/Plate 3/Plate 3 DIV 26 Min 5 Burst.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 27/Plate 3/Plate 3 DIV 27 Min 5 Burst.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 28/Plate 3/Plate 3 DIV 28 Min 5 Burst.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 29/Plate 3/Plate 3 DIV 29 Min 5 Burst.csv"
)

plate3_30min <- c(
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/Plate 3 DIV 24 Min 30 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/Plate 3 DIV 24 Min 30 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/Plate 3 DIV 22 Min 30 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/Plate 3 DIV 21 Min 30 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/Plate 3 DIV 20 Min 30 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/Plate 3 DIV 18 Min 30 Bursts.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 26/Plate 3/Plate 3 DIV 26 Min 30 Burst.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 27/Plate 3/Plate 3 DIV 27 Min 30 Burst.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 28/Plate 3/Plate 3 DIV 28 Min 30 Burst.csv",
  "/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 29/Plate 3/Plate 3 DIV 29 Min 30 Burst.csv"
)



#Generate all the barplots

for (file in plate3_5min) {
  tryCatch({
    plot = ElectrodeBursts$new(file)
    plot$rename_treatment_name('L', 'Control shRNA')
    plot$rename_treatment_name('58', 'Xrn2 shRNA 1')
    plot$rename_treatment_name('61', 'Xrn2 shRNA 2')
    plot$rename_treatment_name('59', 'Xrn2 Exon2 shRNA')
    plot$rename_treatment_name('A4', 'Xrn2 ASD-Linked Isoform shRNA')
    control = 'Control shRNA'
    samples = c('Xrn2 shRNA 1', 'Xrn2 shRNA 2', 'Xrn2 Exon2 shRNA', 'Xrn2 ASD-Linked Isoform shRNA')
    title = basename(file)
    title = sub("\\.csv$", "", title)
    
    plot = plot$create_comparison_raster_plot(control, samples, title)
    setwd('/Users/stevensu/Desktop/Korb Lab/Sean Samples Run/Bursts/Min 5')
    ggsave(paste(title, '.pdf', sep = ""), plot = plot, width = 20, height = 10, dpi = 300)
  }, error = function(e) {
    cat("Error in file:", file, "\nMessage:", e$message, "\n")
  })
}

for (file in plate3_30min) {
  tryCatch({
    plot = ElectrodeBursts$new(file)
    plot$rename_treatment_name('L', 'Control shRNA')
    plot$rename_treatment_name('58', 'Xrn2 shRNA 1')
    plot$rename_treatment_name('61', 'Xrn2 shRNA 2')
    plot$rename_treatment_name('59', 'Xrn2 Exon2 shRNA')
    plot$rename_treatment_name('A4', 'Xrn2 ASD-Linked Isoform shRNA')
    control = 'Control shRNA'
    samples = c('Xrn2 shRNA 1', 'Xrn2 shRNA 2', 'Xrn2 Exon2 shRNA', 'Xrn2 ASD-Linked Isoform shRNA')
    title = basename(file)
    title = sub("\\.csv$", "", title)
    
    plot = plot$create_comparison_raster_plot(control, samples, title)
    setwd('/Users/stevensu/Desktop/Korb Lab/Sean Samples Run/Bursts/Min 30')
    ggsave(paste(title, '.pdf', sep = ""), plot = plot, width = 20, height = 10, dpi = 300)
  }, error = function(e) {
    cat("Error in file:", file, "\nMessage:", e$message, "\n")
  })
}

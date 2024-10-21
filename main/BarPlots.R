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


#Generate all the barplots

for (file in plate3_5min) {
  tryCatch({
    plot = MEAnalysis$new(file)
    plot$rename_treatment('L', 'Control shRNA')
    plot$rename_treatment('58', 'Xrn2 shRNA 1')
    plot$rename_treatment('61', 'Xrn2 shRNA 2')
    plot$rename_treatment('59', 'Xrn2 Exon2 shRNA')
    plot$rename_treatment('A4', 'Xrn2 ASD-Linked Isoform shRNA')
    control = 'Control shRNA'
    samples = c('Xrn2 shRNA 1', 'Xrn2 shRNA 2', 'Xrn2 Exon2 shRNA', 'Xrn2 ASD-Linked Isoform shRNA')
    title = basename(file)
    title = sub("\\.csv$", "", title)
    plot = plot$treatment_averages_t_test_plot(control, samples, title)
    setwd('/Users/stevensu/Desktop/Korb Lab/Sean Samples Run/Barplots/Min 5')
    ggsave(paste(title, '.pdf', sep = ""), plot = plot, width = 15, height = 15, dpi = 300)
  }, error = function(e) {
    cat("Error in file:", file, "\nMessage:", e$message, "\n")
  })
}

for (file in plate3_30min) {
  tryCatch({
    plot = MEAnalysis$new(file)
    plot$rename_treatment('L', 'Control shRNA')
    plot$rename_treatment('58', 'Xrn2 shRNA 1')
    plot$rename_treatment('61', 'Xrn2 shRNA 2')
    plot$rename_treatment('59', 'Xrn2 Exon2 shRNA')
    plot$rename_treatment('A4', 'Xrn2 ASD-Linked Isoform shRNA')
    control = 'Control shRNA'
    samples = c('Xrn2 shRNA 1', 'Xrn2 shRNA 2', 'Xrn2 Exon2 shRNA', 'Xrn2 ASD-Linked Isoform shRNA')
    title = basename(file)
    title = sub("\\.csv$", "", title)
    plot = plot$treatment_averages_t_test_plot(control, samples, title)
    setwd('/Users/stevensu/Desktop/Korb Lab/Sean Samples Run/Barplots/Min 30')
    ggsave(paste(title, '.pdf', sep = ""), plot = plot, width = 15, height = 15, dpi = 300)
  }, error = function(e) {
    cat("Error in file:", file, "\nMessage:", e$message, "\n")
  })
}

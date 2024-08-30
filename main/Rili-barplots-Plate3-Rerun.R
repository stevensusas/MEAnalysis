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

plot = MEAnalysis$new("/Users/stevensu/Desktop/Korb Lab/Steven Supplementary MEA 2/DIV 29/Plate 3/Plate 3 DIV 29 Min 30.csv")
control = 'U'
samples = c('U', 'NEG Control-HET')
plot$remove_well('F4', "NEG Control-HET")
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
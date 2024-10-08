source('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/BatchMEAnalysisClass.R')

file_paths <- c (
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 1/6-04-24 nxn DIV 11 PLATE 1(002)(001).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-5-24/Plate 1/6-05-24 nxn DIV 12 PLATE 1(001)(001).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-6-24/Plate 1/6-06-24 nxn DIV 13 PLATE 1(000)(001).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-7-24/Plate 1/div 14 nxn 6-7-24 plate 1(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-8-24/Plate 1/ra nxn div 15 plate 1(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-9-24/Plate 1/ra nxn div 16 plate 1(003)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-10-24/Plate 1/ra nxn div 17 plate1 spontaneous(001)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 1/ra nxn div 18 plate 1 spontaneous 20min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 1/ra nxn div 18 plate 1 spontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/ra nxn div 20 plate1 spontaneous 5min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/ra nxn div 20 plate1 spontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/ra nxn div 20 plate1 spontaneous 60min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/ra nxn div 21 spontaneous 5min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/ra nxn div 21 spontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/ra nxn div 21 spontaneous 50min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/ra nxn div 22 plate1 spontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/ra nxn div 22 plate1 spontaneous(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/ra nxn div 22 plate1spontaneaous 60min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/ra nxn plate1cspontaneous 5min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/ra nxn plate1cspontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/ra nxn plate1cspontaneous 30min(001)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/ra nxn plate1cspontaneous 5min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/ra nxn plate1cspontaneous 30min(000)(000).csv',
  '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/ra nxn plate1cspontaneous 30min(001)(000).csv'
)

conditions <- c("ASH1L-HET", "CHD8-HET", "DNMT3A-HET", "KDM6B-HET", "KMT2C-HET", "Luciferase-HET", "MBD5-HET", "MED13L-HET", "NSD1-HET", "SETD5-HET", "TBR1-HET")

metric <- "Number of Bursts"

analysis <- BatchMEAnalysis$new(file_paths)

analysis$run_mea_analysis(conditions, metric, "Luciferase-HET", "Hello")

sig_table = analysis$generate_significance_overview('Luciferase-HET', conditions, c('Number of Bursts', 'Mean Firing Rate (Hz)'), "Sig")

args <- commandArgs(trailingOnly = TRUE)
output_file <- args[1]
input_dir   <- args[2]

library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)

# 1. Gather all profile data
input_files <- list.files(path = input_dir, pattern = "_tss_profile.tab$", full.names = TRUE)

combined_data <- lapply(input_files, function(f) {
  # deeptools tab files have metadata in the first few lines, 
  # usually the data starts after the 2nd line
  dt <- fread(f, skip = 2)
  sample_name <- gsub("_tss_profile.tab", "", basename(f))
  
  # The tab file has 2 rows: one for the bins and one for the values
  # We want to transform this into a long format
  values <- as.numeric(dt[1, ])
  bins <- seq(-1000, 999, length.out = length(values)) # Adjusting to 2kb window
  
  data.frame(Sample = sample_name, Bin = bins, Score = values)
}) %>% bind_rows()

# 2. Plotting
p <- ggplot(combined_data, aes(x = Bin, y = Score, color = Sample)) +
  geom_line(size = 0.8, alpha = 0.7) +
  # Adds a vertical line at the exact TSS
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_color_viridis_d(option = "plasma") + 
  theme_minimal(base_size = 12) +
  labs(title = "TSS Enrichment Profile",
       subtitle = "Aggregated signal around Transcription Start Sites",
       x = "Distance from TSS (bp)",
       y = "BPM Normalized Signal",
       color = "Sample") +
  theme_bw()+
  theme(
    legend.position = "right",
    legend.text = element_text(size = 6), # Small text for your long sample names
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# 3. Save high-res
ggsave(output_file, plot = p, width = 10, height = 6, dpi = 300)
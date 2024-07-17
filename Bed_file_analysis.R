pacman::p_load(pacman, rio) 
rio_csv <- import("~/Desktop/Umich/Noncanonical_SNPs/Ribo-seq_ORFs.csv")
(bar <- table(rio_csv[, 17]))
par(mfrow = c(4, 4))
barplot(bar, main = 'Counts of Different Non-canonical Categories', ylab = 'Counts')

percentages <- round(bar / sum(bar) * 100)
labels <- c('doORF', 'dORF', 'intORF', 'lncRNA', 'processed_transcript', 'uoORF', 'uORF')
labels_with_percent <- paste(labels, percentages, "%", sep=" ")
pie(bar, labels = labels_with_percent, main = "Pie Chart with Percentages")

(bar <- rio_csv[, 17])

library(tidyverse)
sum_each_entry <- function(x) {
  # Split the string by commas
  parts <- str_split(x, ",", simplify = TRUE)
  # Convert the parts to integers, ignoring empty strings
  parts <- as.integer(parts[parts != ""])
  # Sum the parts
  sum(parts, na.rm = TRUE)
}

# Apply the function to each entry in the 11th column
sums <- rio_csv[, 11] %>% map_dbl(sum_each_entry)
new <- cbind(bar, sums)


for (type in labels) {
  # Step 1: Filter rows where 'bar' equals 'lncRNA'
  data <- new[new[, 1] == type, ]
  
  # Step 2: Extract the 'sums' column from the filtered data
  data_subset <- as.integer(data[, 2])
  # Step 3: Plot histogram of 'sums_subset'
  hist(data_subset, main = paste('Histogram of ', type), xlab = "NT Count", breaks = 500, freq = F)
  
  curve(dnorm(x, mean = mean(data_subset), sd = sd(data_subset)),
        col = "thistle1",  # Color of curve
        lwd = 2,           # Line width of 2 pixels
        add = TRUE)        # Superimpose on previous graph
  
  boxplot(data_subset, 
          main = paste(type, " Boxplot"),
          col = c("lightblue", "lightgreen", "lightpink"),
          notch = TRUE,        # Add notches to the boxes
          varwidth = TRUE, horizontal = T)  
}

dev.off()
rm(list = ls()) 

  
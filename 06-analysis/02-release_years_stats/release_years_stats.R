library(ggplot2)
library(cowplot)
library(forcats)
library(scales)

# set up data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dataRaw <- read.csv2("../../05-data/data-clean.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = c("-", ""))

df <- dataRaw[,c("TID", "Name", "First.release..Year.", "Latest.release..Year.")]
colnames(df) <- c("TID", "Name", "First.release..Year.", "Latest.release..Year.")
df[df == "N/A"]  <- NA
df[df == "Not available"]  <- NA
df <- na.omit(df)

for(i in 1:nrow(df)){
  df[i,5] <- df[i,4]-df[i,3]
}

colnames(df) <- c("Internal.PID", "Name", "First.release..Year.", "Latest.release..Year.", "Age")


sqrt(var(df$First.release..Year.))
var(df$Latest.release..Year.)
mean(df$Age)

sink("release-stats.txt")
cat("FIRST RELEASE\n")
cat(c("m=", as.character(mean(df$First.release..Year.)), "\n"))
cat(c("std=", as.character(sqrt(var(df$First.release..Year.))), "\n"))
cat("\n")
cat("LATEST RELEASE\n")
cat(c("m=", as.character(mean(df$Latest.release..Year.)), "\n"))
cat(c("std=", as.character(sqrt(var(df$Latest.release..Year.))), "\n"))
cat("\n")
cat("AGE\n")
cat(c("m=", as.character(mean(df$Age)), "\n"))
cat(c("std=", as.character(sqrt(var(df$Age))), "\n"))
cat("\n")
sink()
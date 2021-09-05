library(ggplot2)
library(cowplot)
library(forcats)
library(scales)

# set up data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dataRaw <- read.csv2("../_data/analysis-clean.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = c("-", ""))

df <- dataRaw[,c("TID", "Name", "First.release..Year.", "Latest.release..Year.")]
colnames(df) <- c("TID", "Name", "First.release..Year.", "Latest.release..Year.")
df[df == "N/A"]  <- NA
df[df == "Not available"]  <- NA
df <- na.omit(df)
df[,3] <- as.Date(paste(df[,3], 1, 1, sep="-"))
df[,4] <- as.Date(paste(df[,4], 1, 1, sep="-"))
df <- df[ order(df[,3]), ]
df$toolID <- as.factor(nrow(df):1)
df$projectLength <- round(as.numeric(df[,4]-df[,3])/365, 4)
row.names(df) <- 1:nrow(df)

years<-seq.Date(from=min(df[,3]), to=max(df[,4]), by="5 years")
years<-append(years,as.Date("2021-01-01"))

#library(wesanderson)
p<- ggplot(data = df, aes(x = First.release..Year., y = toolID, )) +
  geom_segment(aes(xend = Latest.release..Year., yend = toolID, color=projectLength), size = 3) +
  scale_colour_gradient2("Age (Years)", low = "#FF0000", mid = "#FFFF00", high = "#00FF00", midpoint=mean(df$projectLength), na.value = NA) +
  scale_x_date(expand = c(0, 0), 
               #breaks=date_breaks("2 years"), 
               breaks = years,
               #breaks = as.Date(c("1970-01-01", "2022-01-01")),
               date_minor_breaks = "1 year", 
               labels = date_format("%Y")) +
  scale_y_discrete(breaks=df$toolID, labels = df$Name) +
  theme(legend.position = c(-0.4, 0.4)) +
  xlab('Year') + 
  ylab('') + 
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title = element_text(size = 11))

outputFile <- "release_years.pdf"
pdf(outputFile)

print(p)
ggsave(outputFile, p)

dev.off()

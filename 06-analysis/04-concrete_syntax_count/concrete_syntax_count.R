library(ggplot2)

# set up data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv2("../../05-data/data-clean.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = c("-", ""))
colnames(data)[12]<- "Concrete.syntax.instances.count"

for(i in 1:nrow(data)){
  data$Concrete.syntax.instances.count[i] <- 
    as.numeric(gsub("([0-9]+).*$", "\\1", data$Concrete.syntax.instances.count[i]))
}
data$Concrete.syntax.instances.count<-as.numeric(data$Concrete.syntax.instances.count)
data<-data[!is.na(data$Concrete.syntax.instances.count),]


countSyntaxTypes = function(df, output){
  return(length(scan(text = df[11], what=character(), sep = ",")))
}
Concrete.syntax.types.count <- apply(data, 1, countSyntaxTypes)
data <- cbind(data, Concrete.syntax.types.count)

countWhat <- "types"
#countWhat <- "instances"

if(countWhat == "types"){
  data$syntaxcount<-data$Concrete.syntax.types.count
}else if(countWhat == "instances"){
  data$syntaxcount<-data$Concrete.syntax.instances.count
}else{
  print("ERROR")
}
  
p<-ggplot(data, aes(x=syntaxcount)) +
  #geom_histogram(color="darkblue", fill="lightblue", binwidth = 1) +
  geom_bar(color="darkblue", fill="lightblue", width = 1, position = position_dodge()) +
  #geom_vline(aes(xintercept=mean(syntaxcount)), color="blue", linetype="dashed", size=1) +
  scale_x_continuous(breaks=seq(min(data$syntaxcount),
                                max(data$syntaxcount), 1))+
  stat_bin(binwidth= 1, geom="text", aes(label=..count..) , vjust = 1.5, size=6) +
  labs(x = paste("Number of syntax ", countWhat, sep = ""), y = "Cases") +
  #geom_text(aes(label=paste(round(mean(syntaxcount),1)," (sd=",round(sd(syntaxcount),1),")",sep=""),
  #              y=0,x=mean(syntaxcount)),
  #              vjust=-28,col="blue",hjust=-0.3, size=5, fontface = "italic") +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = -12))) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(axis.title = element_text(size = 13)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
  theme(axis.ticks.x = element_blank())

outputFile <- "concreteSyntaxCount.pdf"
aspect_ratio <- 3
width <- 3
pdf(outputFile, width= width, height = width*aspect_ratio)

print(p)

dev.off()

library(ggplot2)
library(stringr)
library(plyr)
library(grid)
library(gridExtra)
library(Rmisc)
library(pals)

#remove non-analyzable columns
columnsToBeDropped <- c("Motivation")

# set up data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv2("../../05-data/data-clean.csv", header = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings = c("-", ""))
data <- data[,1:27]
data <- data[ , -which(names(data) %in% columnsToBeDropped)]

countSyntaxTypes = function(df, output){
  return(length(scan(text = df[10], what=character(), sep = ",")))
}
Concrete.syntax.types.count <- apply(data, 1, countSyntaxTypes)
data <- cbind(data, Concrete.syntax.types.count)


#data[is.na(data)] <- "N/A"

#factorColumns = c(7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)
#for(i in factorColumns){
#  data[, i] <- as.factor(data[, i])
#}

###CALCULATED COLUMNS###
collaborationBinary <- data$Collaboration
collaborationBinary[collaborationBinary=="YES-SYNC"] <- "YES"
collaborationBinary[collaborationBinary=="YES-ASYNC"] <- "YES"
data$Collaboration.Binary <- collaborationBinary

icvizBinary <- data$Inconsistency.visualization
icvizBinary[icvizBinary=="INTERNAL"] <- "YES"
icvizBinary[icvizBinary=="EXTERNAL"] <- "YES"
data$Inconsistency.visualization.Binary <- icvizBinary

icmStrategy <- data$Inconsistency.management.type
icmStrategy[icmStrategy=="ON-THE-FLY"] <- "ALLOW-AND-RESOLVE"
icmStrategy[icmStrategy=="ON-DEMAND"] <- "ALLOW-AND-RESOLVE"
data$Inconsistency.management.strategy <- icmStrategy

icmAutomation <- data$Inconsistency.management.automation
icmAutomation[icmAutomation=="PARTIAL"] <- "YES"
icmAutomation[icmAutomation=="AUTOMATIC"] <- "YES"
data$Inconsistency.management.automation <- icmAutomation

concrete.syntax.count.2.more <- data$Concrete.syntax.type.count
concrete.syntax.count.2.more[concrete.syntax.count.2.more>2] <- "YES"
concrete.syntax.count.2.more[concrete.syntax.count.2.more==2] <- "NO"
data$concrete.syntax.count.2.more <- concrete.syntax.count.2.more

startIndex <- 7
stopIndex <- ncol(data)

ignoredColumnNames <- c("First.release", "Latest.release")

data <- data.frame(
  lapply(
    data,
    function(variables){
      if (is.character(variables)){
        return(toupper(variables))
      } else{
        return(variables)
      }
    }),
  stringsAsFactors = FALSE)

gatherValues <- function(index){
  if(!(names(data)[i] %in% ignoredColumnNames) && !is.numeric(data[,index])){
    df <- data.frame(cat = data[,index])
    x<-data.frame(values=unique(str_trim(unlist(strsplit(df$cat, ",")))))
    
    for(i in 1:nrow(x)){
      v <- x[i,1]
      count = 0
      for(j in 1:nrow(df)){
        if(v %in% str_trim(unlist(strsplit(df[j,1], ",")))){
          count <- count+1
        }
      }
      x[i,2] <- count
    }
    
    #rename col 2
    names(x)[2]<-"count"
    
    #remove factors with count = 0
    x <- x[x$count > 0, ]
    
    #pretty print label names and values
    for(i in 1:nrow(x)){
      x[i,1] <- paste(x[i,1], paste("(", x[i,2], ")", sep=""))
    }
    
    #order by count DESC
    x <- x[
      with(x, order(count, decreasing=TRUE)),
    ]
    x$values <- factor(x$values, levels = x$values[order(x$count, decreasing = TRUE)])
    
    #factorize
    #x$values <- as.factor(x$values)
    
    
    return(x)
  }
}


# plotting
plot <- function(index){
  x <- gatherValues(index)
  p <- ggplot(x, aes(x = '', y=count, fill=values))  +
    theme_void() +
    ggtitle(names(data)[index]) +
    geom_bar(stat="identity", position = position_fill(reverse = TRUE), width = 0.6) +
    theme(legend.position="bottom", legend.title = element_text(size=8)) +
    theme(axis.text.x = element_text(colour="black"),
          axis.title.x = element_text(colour="black"),
          plot.title = element_text(size=8),
          legend.key.size = unit(0.2, 'cm'),
          legend.text=element_text(size=8)) +
    guides(fill=guide_legend(title="", ncol=3, reverse = FALSE)) +
    ylab("") +
    scale_fill_manual(values=as.vector(glasbey(27))) +
    coord_flip()
}


outputFile <- "descriptive.pdf"
pdf(outputFile, paper="a4", width=10, height=15)
#l <- layout(matrix(c(1,2,3,4,5,6),ncol=1), widths=c(4,4,4), heights=c(1,1,1,1,1,1), TRUE)

plots <- c()
plotIndex = 1
for(i in startIndex:stopIndex){
  if(!(names(data)[i] %in% ignoredColumnNames) && !is.numeric(data[,i])){
    plotName <- paste(names(data)[i])
    print(paste("Printing plot for ", plotName, "."))
    plots[[plotIndex]]<-plot(i)
    plotIndex <- plotIndex+1
  }
  if(plotIndex %% 9 == 0){
    multiplot(plotlist = plots)
    plots <- c()
    plotIndex = 1
  }
}
if(!is.null(plots)){
  multiplot(plotlist = plots)
}

dev.off()

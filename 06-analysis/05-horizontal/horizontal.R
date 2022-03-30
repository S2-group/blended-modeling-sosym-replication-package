library(splitstackshape)
library(plyr)
library(ggplot2)
library(reshape2)

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

concrete.syntax.count.2.more <- data$Concrete.syntax.types.count
concrete.syntax.count.2.more[concrete.syntax.count.2.more>2] <- "YES"
concrete.syntax.count.2.more[concrete.syntax.count.2.more==2] <- "NO"
data$concrete.syntax.count.2.more <- concrete.syntax.count.2.more

startIndex <- 7
stopIndex <- ncol(data)


ignoredColumnNames <- c("Motivation..22.", "First.release..Year...18.", "Latest.release..Year...19.", "Intent..41.")





dataSource <- data

# separator used in categorical variables with multiple values
separator <- ', '

############# DO NOT CHANGE ANYTHING BELOW THIS LINE ############# 

df <- dataSource

index <- 1
generateCouples <- function() {
  indexes <- c(startIndex:stopIndex)
  var1 <- c()
  var2 <- c()
  for(i in indexes) {
      if(!(names(data)[i] %in% ignoredColumnNames) && (i <= length(names(df)))) {
        var1el <- names(df)[i]
        indexes2 <- c(i:stopIndex)
        for(j in indexes2) {
          if(!(names(data)[j] %in% ignoredColumnNames) && (j <= length(names(df)))) {
            var2el <- names(df)[j]
            if(var1el != var2el) {
              var1 <- append(var1, var1el)
              var2 <- append(var2, var2el)
            }
          }
        }
      }
  }
  result <- data.frame(var1, var2)
  return(result)
} 

result <- generateCouples()
resultLength <- nrow(result)

topScale <- nrow(dataSource)


# PREPARE DATA SOURCE FOR PLOTS
dt2 <- cSplit(df, splitCols=names(df), sep=separator, direction="long", drop=TRUE)
dt2 <- dt2[!duplicated(dt2), ]

ind <- apply(dt2, 1, function(x) all(is.na(x)))
dt2 <- dt2[ !ind, ]

index = 1
for(i in 1:nrow(dt2)) {
  row <- dt2[i,]
  if(is.na(row$Internal.PID)) {
    dt2[[i,1]] <- dt2[[i-1,1]]
  } else {
    index <- i
  }
}

createPlot <- function(plotName, var1, var2) {
  tbl <- table(as.factor(dt2[[as.character(var1)]]), as.factor(dt2[[as.character(var2)]]))
  resultDf <- as.data.frame.matrix(tbl)
  
  
  #v1 <- data[,var1]
  #class(v1)
  #v2 <- data[,var2]
  #class(v2)
  
  #tbl <- table(as.factor(dt2[[as.character(var1)]]), as.factor(dt2[[as.character(var2)]]))
  #tbl <- table(factor(v1), droplevels(factor(v2)))
  #resultDf <- as.data.frame.matrix(tbl)
  #if(nrow(resultDf)>1){
  #  resultDf <- resultDf[rowSums(resultDf[])>0,]
  #}
  #if(!is.null(ncol(resultDf))){
  #  resultDf <- resultDf[, colSums(resultDf != 0) > 0]
  #}
  
  #if(is.null(ncol(resultDf))){
  #  return
  #}
  
  for(x in 1:nrow(tbl)) {
    xName <- rownames(tbl)[x]
    for(y in 1:ncol(tbl)) {
      yName <- colnames(tbl)[y]
      tbl[x,y] <- 0
      for(i in 1:nrow(data)) {
        row <- data[i,]
        if(grepl(xName, row[[as.character(var1)]]) && grepl(yName, row[[as.character(var2)]])) {
          tbl[x,y] <- tbl[x,y] + 1
        }
      }
    }
  }
  
  resultDf <- as.data.frame.matrix(tbl)
  if(nrow(resultDf)>1){
    resultDf <- resultDf[rowSums(resultDf[])>0,]
  }
  if(!is.null(ncol(resultDf))){
    resultDf <- resultDf[, colSums(resultDf != 0) > 0]
  }
  
  #print(is.null(nrow(resultDf)))
  #print(is.null(ncol(resultDf)))
  #if(is.null(nrow(resultDf)) && is.null(ncol(resultDf))){
  #  return
  #}
  
  #SET THIS TO TRUE IF THE CHI-SQUARE TABLE IS TO BE GENERATED
  chiTable = FALSE
  
  legendName <- "Frequency"
  if(chiTable){
    legendName <- "Chi-stat"
  }
  
  if(chiTable){
    t2 <- tbl
    
    for(i in 1:nrow(tbl)){
      for(j in 1:ncol(tbl)){
        o <- tbl[i,j]
        scol <- sum(tbl[,j])
        srow <- sum(tbl[i,])
        e <- (scol*srow)/(sum(tbl))
        t2[i,j] <- round(((o-e)^2)/e,2)
      }
    }
    
    tbl <- t2
  }
  
  #print(nrow(tbl))
  #print(ncol(tbl))
  
  titleColor <- "black"
  
  if(!chiTable){
    chi <- 9999
    p.yates <- 1
    p <- 1
    if(!(is.null(nrow(resultDf)) && is.null(ncol(resultDf)))){
      p.yates <- chisq.test(resultDf)$p.value
      p <- chisq.test(resultDf, correct = FALSE)$p.value
    }
    
    plotName <- paste(plotName, "  (Chi sq w/ Yates p= ", round(p.yates,4), ", p= ", round(p,4), ")", sep="")
    #print(p)

    if(!is.nan(p)){
      if(p < 0.05){
        titleColor <- "green"
      }else if(p < 0.10){
        titleColor <- "orange"
      }else{
        titleColor <- "red"
      }
    }
  }  

  plot <- ggplot(melt(as.factor(tbl)), aes(as.factor(Var2), as.factor(Var1))) +
    geom_tile(data=melt(tbl), aes(fill=as.integer(value)), color="grey") +
    geom_text(data=melt(tbl), aes(label=value), size=4, color="black") +
    theme_bw() + 
    #scale_fill_gradient2(low="blue", high="red", mid="white",name="Frequency", limit=c(0,topScale)) +
    scale_fill_gradient2(low="white", high="red", mid="orange",name=legendName, limit=c(0,max(tbl)), midpoint = max(tbl)/2) +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=11, hjust=1)) +
    coord_equal() + labs(x=var2, y=var1) +
    #ggtitle(plotName, aes(color= "red"))
    labs(title = plotName) +
    theme(plot.title = element_text(color = titleColor, hjust = 0.5))
  
  #if(p < 0.10){
    print(plot)
  #}
}



# PRINT INTO FILE
outputFile <- "horizontal.pdf"
pdf(outputFile, width=10, height=10)
par(mar=c(2, 2, 2, 2))
par(mfrow=c(1, 1))
par(las=1)

for(i in 1:resultLength) {
  plotName <- paste(result[i,]$var1, "_____", result[i,]$var2, sep="")
  print(paste(i, "/", resultLength, " - ", plotName))
  createPlot(plotName, result[i,]$var1, result[i,]$var2)
}

dev.off()
require(caret)
require(lattice)
require(ggplot2)
require(reshape2)

var.percent.df <- data.frame("PC1"=numeric(),"PC2"=numeric(),"PC3"=numeric(),"PC4"=numeric(),"PC5"=numeric(),"PC6"=numeric(),"PC7"=numeric())

folder_path=""

filenames <- c("finger0.0259566433568363331474701843818.csv",
               "finger2.5956358122445411474714870458.csv",
               "finger7.5013869528483421474715090152.csv",
               "finger9.1106809778083181474735572780.csv",
               "finger13.056046974484391474702612446.csv",
               "finger15.5997697881308091474735553701.csv",
               "finger18.0137108255707721474735331083.csv",
               "finger20.79104083638881474728521805.csv",
               "finger23.6721962681719841474735237028.csv",
               "finger25.8853811186243271475101850859.csv",
               "finger28.046247715390911475102239278.csv")

for (i in filenames){  
  filename = i 
  dataset <- read.csv(paste0(folder_path,filename), header=FALSE)
  # log transform 
  number_of_muscles = length(dataset[1,])
  dataset.pca <- prcomp(dataset, retx=TRUE, center=TRUE, scale.=TRUE)
  sd <- dataset.pca$sdev
  loadings <- dataset.pca$rotation
  rownames(loadings) <- colnames(dataset)
  scores <- dataset.pca$x
  var <- sd^2
  var.percent <- var/sum(var) * 100
  #dev.new()
  #barplot(var.percent, xlab="PC", ylab="Percent Variance", main=filename, names.arg=1:length(var.percent), las=1, ylim=c(0,max(var.percent)), col="gray")
  #abline(h=1/ncol(dataset)*100, col="red")
  #print(loadings)
  #print(var.percent)  
  var.percent.df[i,] = var.percent
}

# converts dataframe to appropriate form for stacked bar plot

var.percent.df.melt <- t(var.percent.df)

library(ggplot2)
mainData = read.csv("Data/PlateReader/Intial2490-3CompareClean.csv")


mainData[4,]

mainData$Cycles...Well

rownames(mainData) = mainData$Cycles...Well

mainData[which(mainData[1] == "Mean"),]

meanData = mainData[which(mainData[1] == "Mean"),]
timeData = mainData[which(mainData[1] == "Time [s]"),]
duplicated(timeData) # all time data is the same 
timeData = timeData[1,]
rownames(timeData)= "Time"
timeData = timeData[,-1]

mainData[(which(mainData[1] == "Mean")-3),1]
rownames(meanData) = mainData[(which(mainData[1] == "Mean")-3),1]
meanData = meanData[,-1]

meanData = rbind(timeData, meanData)
meanData2 = data.frame(t(meanData))

plot(meanData2$Time, meanData2$A1)

meanData3 = lapply(FUN = as.numeric, meanData2)
meanData3= data.frame(meanData3)

fields = names(meanData3)
plot <- ggplot( aes(x=Time), data = meanData3)
for (i in 2:length(fields)) { 
  loop_input = paste("geom_point(aes(y=",fields[i],",color='",fields[i],"'))", sep="")
  plot <- plot + eval(parse(text=loop_input))  
}
plot <- plot + guides( color = guide_legend(title = "",) )
plot


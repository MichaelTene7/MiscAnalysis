library(ggplot2)

# -- read the data --
inData = mainData = read.csv("Data/PlateReader/second2490-3CompareClean.csv")

# -- clean the data -- #
timeData = mainData[which(mainData[1] == "Time [s]"),]
timeData = timeData[1,]
rownames(timeData)= "Time"
timeData = timeData[,-1]

meanData = mainData[which(mainData[1] == "Mean"),]
rownames(meanData) = mainData[(which(mainData[1] == "Mean")-3),1]
meanData = meanData[,-1]

cleanData = rbind(timeData, meanData)
cleanData = data.frame(t(cleanData))
cleanData = lapply(FUN = as.numeric, cleanData)
cleanData= data.frame(cleanData)

combinedPlot = function(wells){
  plot <- ggplot( aes(x=Time), data = cleanData)
  for (i in 2:length(wells)) { 
    loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
    plot <- plot + eval(parse(text=loop_input))  
  }
  plot <- plot + guides( color = guide_legend(title = "",) )
  plot
}

# -- plot the data -- 
wellNames = names(cleanData) 


allWells = wellNames
combinedPlot(allWells)

wells2940 = wellNames[c(grep("A", wellNames), grep("E", wellNames))]
combinedPlot(wells2940)
wells2940mm = wellNames[grep("A", wellNames)]
combinedPlot(wells2940mm)

# -------------------- #
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

combinedPlot = function(wells){
plot <- ggplot( aes(x=Time), data = meanData3)
for (i in 2:length(wells)) { 
  loop_input = paste("geom_point(aes(y=",wells[i],",color='",wells[i],"'))", sep="")
  plot <- plot + eval(parse(text=loop_input))  
}
plot <- plot + guides( color = guide_legend(title = "",) )
plot
}

sevenAndEight = names(meanData3)[c(grep(7, names(meanData3)), grep(8, names(meanData3)))]
combinedPlot(sevenAndEight)

notSevenAndEight = names(meanData3)[-c(grep(7, names(meanData3)), grep(8, names(meanData3)),1)]
combinedPlot(notSevenAndEight)


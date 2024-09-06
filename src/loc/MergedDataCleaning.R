mergedData = read.csv("Results/MergedData.csv")

naZoonomias = which(is.na(mergedData$Zoonomia))

for(i in naZoonomias){
  mergedData$Zoonomia = mergedData$FaName[i]
}

write.csv(mergedData, "Results/MergedData.csv", row.names = F)

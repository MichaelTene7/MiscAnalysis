


dataOne = read.csv("Results/ExampleFileOne.csv")
dataTwo = readRDS("Results/ExampleFileTwo.rds")
dataThree = read.csv("Results/ExampleFileThree.csv")
# note an interesting quirk of these .csvs -- they have an index column at the front


mergedData = dataOne            # I can just set this equal because I need all of this data 
print(colnames(dataTwo))        #from this I can see that I don't need the second collumn
dataTwoClean = dataTwo[,c(1,3)] #make a version which only has columns one and three

mergedData = cbind(mergedData, dataTwoClean)

print(colnames(dataThree))                        #I only need one specific collumn from this

which(colnames(dataThree) %in% "goodInfoVector")  #This tells me which column I need given the column name

mergedData = cbind(mergedData, dataThree[,10])
mergedData                                        

#





















# --- Generate the data used for this ---- 
datasetOne = data.frame(101:200, rep("Y", 100))
datasetTwo = data.frame(rep("state", 100), rep("this data isn't helpful, don't include in the merge", 100), rep(FALSE, 100))
badInfoVector = rep("notUseful", 100)
goodInfoVector = rep("youneedThis", 100)
datasetThree = data.frame(badInfoVector, badInfoVector, badInfoVector, badInfoVector, badInfoVector, badInfoVector, badInfoVector, badInfoVector, goodInfoVector, badInfoVector, badInfoVector)


write.csv(datasetOne, "Results/ExampleFileOne.csv")
saveRDS(datasetTwo, "Results/ExampleFileTwo.rds")
write.csv(datasetThree, "Results/ExampleFileThree.csv")
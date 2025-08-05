a=b
zoonomiaName = combinedData$ZoonomiaName
zoonomiaTip = paste0("vs_", zoonomiaName)

combinedData$zoonomiaTip = zoonomiaTip
divider = combinedData$Divider
combinedData = combinedData[,-5]
combinedData$Divider = divider

#write.csv(combinedData, "Results/MergedDataNew.csv", row.names = F)

# ----- 
combinedData = read.csv("Results/MergedDataNew.csv")

combinedData$DerekDiet = classified_df$Diet_Class

#---- 
combinedData = read.csv("Results/MergedDataNew.csv")

colnames(combinedData)

combinedData$Divider2 = combinedData$Divider

colnames(combinedData)

combinedData = combinedData[,c(1:5,12,7,6,8:11,13,14:47)]

colnames(combinedData)

combinedData = combinedData[,c(1:13, 14:23, 47, 24:46)]

combinedData$Divider3 = combinedData$Divider
colnames(combinedData)

combinedData = combinedData[,c(1:24, 39:42, 47, 38, 48, 25:37,43:46)]
colnames(combinedData)

combinedData = combinedData[,c(1:13, 25:31, 14:24, 32:48)]
colnames(combinedData)
#write.csv(combinedData, "Results/MergedDataNew.csv", row.names = F)

# ----- 
combinedData = read.csv("Results/MergedDataNew.csv")
combinedData = combinedData[,-7]
colnames(combinedData)
combinedData = combinedData[,-17]
colnames(combinedData)
#write.csv(combinedData, "Results/MergedDataNew.csv", row.names = F)

combinedData = read.csv("Results/MergedDataNew.csv")
colnames(combinedData)
combinedData$Divider4 = rep("")
combinedData = combinedData[,c(1:16,53,17:28, 54, 47:52, 29:46 )]
colnames(combinedData)
write.csv(combinedData, "Results/MergedDataNew.csv", row.names = F)


combinedData = read.csv("Results/MergedDataNew.csv")
colnames(combinedData)
combinedData = combinedData[,c(1:17, 55, 18:54)]

# ------ 

classified_df <- classify_diet(Elton_full, 70, 50)
write.csv(classified_df, "Results/DerekData70.csv")
classified_df <- classify_diet(Elton_full, 90, 50)
write.csv(classified_df, "Results/DerekData90.csv")

#------

combinedData = read.csv("Results/MergedDataNew.csv")
colnames(combinedData)
combinedData = combinedData[,c(1:17, 56:57, 19:55)]
colnames(combinedData)
colnames(combinedData)[19]= "DerekDietClassification90"
colnames(combinedData)

#write.csv(combinedData, "Results/MergedDataNew.csv", row.names = F)

# --- 

combinedData = read.csv("Results/MergedDataNew.csv")

combinedData$DerekDietClassification70[which(combinedData$DerekDietClassification70 == "Generalist")] = "O-Generalist"
combinedData$DerekDietClassification90[which(combinedData$DerekDietClassification90 == "Generalist")] = "O-Generalist"

combinedData$DerekDietClassification70[which(combinedData$DerekDietClassification70 == "For Examination")] = "O-For Examination"
combinedData$DerekDietClassification90[which(combinedData$DerekDietClassification90 == "For Examination")] = "O-For Examination"

#write.csv(combinedData, "Results/MergedDataNew.csv", row.names = F)

# ------ 
pantheriaValues = combinedData$panTheriaTrophicLevel
pantheriaValues[which(pantheriaValues == 3)] = "Carnivore"
pantheriaValues[which(pantheriaValues == 1)] = "Herbivore"
pantheriaValues[which(pantheriaValues == 2)] = "Omnivore"
pantheriaValues[which(pantheriaValues == -999)] = NA

combinedData$panTheriaTrophicLevelCharacter = pantheriaValues



# ---- 
colnames(combinedData)
combinedData = combinedData[,c(1:19, 56, 20:55)]

#write.csv(combinedData, "Results/MergedDataNew.csv", row.names = F)

# ----- 

meyerTrophicLevel = combinedData$Meyer.Lab.Classification.Compressed

meyerTrophicLevel[which(meyerTrophicLevel == "_Omnivore")] = "Omnivore"
meyerTrophicLevel[which(meyerTrophicLevel == "Piscivore")] = "Carnivore"
meyerTrophicLevel[which(meyerTrophicLevel == "Planktivore")] = "Carnivore"
meyerTrophicLevel[which(meyerTrophicLevel == "Insectivore")] = "Carnivore"
meyerTrophicLevel[which(meyerTrophicLevel == "Ambiguous")] = NA
meyerTrophicLevel[which(meyerTrophicLevel == "Hematophagy")] = NA
unique(meyerTrophicLevel)


combinedData$MeyerTrophicLevel = meyerTrophicLevel


colnames(combinedData)
combinedData = combinedData[,c(1:16, 57, 17:56)]
write.csv(combinedData, "Results/MergedDataNew.csv", row.names = F)


#---- 
combinedData = read.csv("Results/MergedDataNew.csv")
comparisionTable = combinedData

comparisionTable = comparisionTable[-which(is.na(comparisionTable$panTheriaTrophicLevelCharacter)),]
comparisionTable = comparisionTable[-which(is.na(comparisionTable$MeyerTrophicLevel)),]

comparisionTable$MeyerTrophicLevel




data = comparisionTable
col1 = "MeyerTrophicLevel"
col2 = "panTheriaTrophicLevelCharacter"
comparisionTable[[col1]]

compareValues = function(data, col1, col2){
  col1Values = data[[col1]]
  col2Values = data[[col2]]
  
  col1First = substring(col1Values, 1,1)
  col2First = substring(col2Values, 1,1)
  
  matchVal = (col1First == col2First)
  matchVal
}

comparisionTable$meyerVsDerek70 = compareValues(comparisionTable, "MeyerTrophicLevel", "DerekDietClassification70")
comparisionTable$meyerVsDerek90 = compareValues(comparisionTable, "MeyerTrophicLevel", "DerekDietClassification90")
comparisionTable$meyerVsPantheria = compareValues(comparisionTable, "MeyerTrophicLevel", "panTheriaTrophicLevelCharacter")
comparisionTable$derek70VsDerek90 = compareValues(comparisionTable, "DerekDietClassification70", "DerekDietClassification90")
comparisionTable$derek70VsPantheria = compareValues(comparisionTable, "DerekDietClassification70", "panTheriaTrophicLevelCharacter")
comparisionTable$derek90VsPantheria = compareValues(comparisionTable, "DerekDietClassification90", "panTheriaTrophicLevelCharacter")



colnames(comparisionTable)
comparisionTable = comparisionTable[,c(1:2, 12, 15, 17, 18, 19, 21, 22, 58:63, 40, 23:39)]

write.csv(comparisionTable, "Results/DietComparisions.csv")

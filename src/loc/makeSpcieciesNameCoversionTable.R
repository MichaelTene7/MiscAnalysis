library(tools)
HillerZoonomia = read.csv("Data/HillerZoonomiaPhenotypeTable.csv")

#write.csv(HillerZoonomia, "Data/HillerZoonomiaPhenotypeTableBackup.csv")

conversionTable = HillerZoonomia

conversionTable$Binomial = conversionTable$scientific
for(i in 1:length(conversionTable$Binomial)){
  splitString = strsplit(conversionTable$Binomial[i], split = "_")[[1]]
  firstTwo = paste(splitString[1], splitString[2], sep = "_")
  conversionTable$Binomial[i] = firstTwo
}

conversionTable$BinomialPretty = conversionTable$Binomial
conversionTable$BinomialPretty = toTitleCase(conversionTable$BinomialPretty)
conversionTable$BinomialPretty = gsub("_", " ", conversionTable$BinomialPretty)


#write.csv(conversionTable, "Data/speciesNameConversionTable.csv")
#saveRDS(conversionTable, "Data/speciesNameConversionTable.rds")


# ---- Expand to include all zoonomia species ----

manualAnnots = read.csv("Data/manualAnnotationsSheet.csv")

emptyVec = rep(NA, 6)

emptyDF = data.frame(X = rep(NA,120), FaName = rep(NA,120), Zoonomia = rep(NA,120), Common.Name.Or.Group = rep(NA,120),rep(NA,120),rep(NA,120))

conversionTableExtended = conversionTable
conversionTableExtended[c(120:227),] = NA

remainingFaNames = manualAnnots[manualAnnots$FaName %in% conversionTable$Zoonomia,]
conversionTableExtended$Zoonomia = append(conversionTable$Zoonomia, remainingFaNames$FaName)
conversionTableExtended$Common.Name.or.Group = append(conversionTable$Common.Name.or.Group, remainingFaNames$Common.Name.or.Group)

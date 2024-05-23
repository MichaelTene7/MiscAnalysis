pantheriaData = read.table("Data/PantheriaData.tsv", sep = "\t", header = T)
pantheriaData2 = read.table("Data/PantheriaData2.tsv", sep = "\t", header = T)

pantheriaDataUseful = pantheriaData[!pantheriaData$X3.1_AgeatFirstBirth_d == -999,]
pantheriaData2Useful = pantheriaData2[!pantheriaData2$X3.1_AgeatFirstBirth_d == -999,]

pantheriaData$MSW93_Binomial %in% pantheriaData2$MSW05_Binomial
pantheriaData2$MSW05_Binomial %in% pantheriaData$MSW93_Binomial


combinedPantheria = read.csv("Data/CombinedPanTHERIA.csv")
combinedPantheria$scientific = tolower(gsub(" ", "_", combinedPantheria$MSWC_Binomial))

combinedPantheriaUseful = combinedPantheria[!combinedPantheria$X23.1_SexualMaturityAge_d == -999,]
combinedPantheriaUseful = combinedPantheriaUseful[!combinedPantheria$X17.1_MaxLongevity_m == -999,]


combinedPantheriaUseful2 = combinedPantheria[!combinedPantheria$X3.1_AgeatFirstBirth_d == -999,]

combinedPantheriaUseful2$MSWC_Binomial %in% combinedPantheriaUseful$MSWC_Binomial
combinedPantheriaUseful$MSWC_Binomial %in% combinedPantheriaUseful2$MSWC_Binomial

?gsub

HillerZoonomia = read.csv("Data/HillerZoonomiaPhenotypeTable.csv")

length(which(HillerZoonomia$scientific %in% combinedPantheria$scientific))
HillerZoonomia$scientific[which(!HillerZoonomia$scientific %in% combinedPantheria$scientific)]

length(which(HillerZoonomia$scientific %in% combinedPantheriaUseful$scientific))

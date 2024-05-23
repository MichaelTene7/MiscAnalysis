library(RERconverge)

zoonomiaData = readRDS("Data/RemadeTreesAllZoonomiaSpecies.rds")

hillerConversionLocation = "Data/HillerZoonomiaPhenotypeTable.csv"

hillerConverstionTable = read.csv(hillerConversionLocation)


masterTree= zoonomiaData$masterTree

tipsToDrop = masterTree$tip.label[!masterTree$tip.label %in% hillerConverstionTable$Zoonomia]
prunedMaster = drop.tip(masterTree, tipsToDrop)

remainingTips = prunedMaster$tip.label
nameSwap = hillerConverstionTable$FaName[match(remainingTips, hillerConverstionTable$Zoonomia)]

prunedMaster$tip.label = nameSwap

plotTree(prunedMaster)

tipsToFix = hillerConverstionTable$FaName[which(is.na(hillerConverstionTable$Zoonomia)) ]

source("Src/Reu/ZoonomTreeNameToCommon.R")
source("Src/Reu/ZonomNameConvertVector.R")
tipsToFixCommon = ZonomNameConvertVectorCommon(tipsToFix, manualAnnotLocation = hillerConversionLocation)
tipsToFixCommon

newHillerCommon = ZoonomTreeNameToCommon(prunedMaster, manualAnnotLocation = hillerConversionLocation)

#write.tree(prunedMaster, "Results/newHillerMasterTree.txt")
write.tree(newHillerCommon, "Results/newHillerCommonMasterTree.txt")




hillerData = readRDS("Data/mam120aa_trees.rds")

hillerMaster = hillerData$masterTree
ZoonomTreeNameToCommon(hillerMaster, manualAnnotLocation = hillerConversionLocation)

hillerCommon = ZoonomTreeNameToCommon(hillerMaster, manualAnnotLocation = hillerConversionLocation)

par(mfrow = c(1,1))
plotTreeHighlightBranches(hillerMaster, hlspecies = tipsToFix)
plotTreeHighlightBranches(hillerCommon, hlspecies = tipsToFixCommon)


newHiller2 = read.tree(file = 'Results/newHillerMasterTree.txt')
plotTreeHighlightBranches(newHiller2, hlspecies = tipsToFix)


hillerCommon$tip.label[which(!hillerCommon$tip.label %in% newHillerCommon$tip.label)]

length(hillerMaster$tip.label)
length(newHiller2$tip.label)
length(tipsToFix)


which(!hillerMaster$tip.label %in% hillerConverstionTable$FaName)
length(which(is.na(hillerConverstionTable$Zoonomia)))



startingZoonomiaTree = zoonomiaData$masterTree
length(startingZoonomiaTree$tip.label)
tipsToDrop = startingZoonomiaTree$tip.label[!startingZoonomiaTree$tip.label %in% hillerConverstionTable$Zoonomia]
length(tipsToDrop)

extraMissingTipsZo = hillerConverstionTable$Zoonomia[!hillerConverstionTable$Zoonomia %in% startingZoonomiaTree$tip.label & !is.na(hillerConverstionTable$Zoonomia)]
extraMissingTips = hillerConverstionTable$FaName[match(extraMissingTipsZo, hillerConverstionTable$Zoonomia)]
noZoTips = hillerConverstionTable$FaName[which(is.na(hillerConverstionTable$Zoonomia)) ]


tipsToFix = append(noZoTips, extraMissingTips)
plotTreeHighlightBranches(hillerMaster, hlspecies = tipsToFix, useGG = T)

?plotTreeHighlightBranches


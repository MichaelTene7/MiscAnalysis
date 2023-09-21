library(RERconverge)
echolcationTree = readRDS("Data/echolocationUpdate2BinaryTree.rds")
write.tree(echolcationTree, "Results/EcholocationTreeTweaks/EcholocationCombinedTree.tree")

echolocationCombinedTree = read.tree("Results/EcholocationTreeTweaks/EcholocationCombinedTree.tree")
plotTree(echolocationCombinedTree)
echolocationBatTree = read.tree("Results/EcholocationTreeTweaks/EcholocationBatTree.tree")
plotTree(echolocationBatTree)
echolocationCetaceanTree = read.tree("Results/EcholocationTreeTweaks/EcholocationCetaceanTree.tree")
plotTree(echolocationCetaceanTree)

mainTrees = readRDS("../RunRER/Data/RemadeTreesAllZoonomiaSpecies.rds")
RERObject = readRDS("../RunRER/Output/EcholocationUpdate2/EcholocationUpdate2RERFile.rds")

source("Src/Reu/BayesFactorAnalysis.R")

combinationTree = echolocationCombinedTree; subclade1Name = "Bat"; subclade1Tree = echolocationBatTree; subclade2Name = "Cetacean"; subclade2Tree = echolocationCetaceanTree


echolocationBayes = calculateBayesFactor(mainTrees, RERObject, echolocationCombinedTree, "Bat", echolocationBatTree, "Cetacean", echolocationCetaceanTree)
echolocationBayes2 = echolocationBayes
echolocationBayes2$ratio = NA
for(i in 1:nrow(echolocationBayes2)){
  combinationSub1Ratio = echolocationBayes2[i,1]/echolocationBayes2[i,2]
  combinationSub2Ratio = echolocationBayes2[i,1]/echolocationBayes2[i,3]
  minRatio = min( combinationSub1Ratio, combinationSub2Ratio)
  echolocationBayes2[i,4]=minRatio
}

BayesRatio = min()

echolocationCorrelations = readRDS("Data/EcholocationUpdate2CorrelationFile.rds")
PermPval = readRDS("Data/EcholocationUpdate2CombinedPrunedFastAllPermulationsPValue.rds")

echolocationCorrelations = cbind(echolocationCorrelations, PermPval)
echolocationCorrelations = cbind(echolocationCorrelations, echolocationBayes2[,c(2,3,1,4)])


echolocationCorrelationsSort = echolocationCorrelations[order(echolocationCorrelations$PermPval),]
echolocationCorrelationsSort
for(i in c(6,7,8,9)){
echolocationCorrelationsSort[,i] = sapply(X = echolocationCorrelationsSort[,i], FUN = format, digits = 4, scientific = F)
}

echolocationCorrelationsSort
saveRDS(echolocationCorrelationsSort, "Output/EcholocationBayesResults.rds")
write.csv(echolocationCorrelationsSort, "Output/EcholocationUpdate2BayesCorrelations.csv")

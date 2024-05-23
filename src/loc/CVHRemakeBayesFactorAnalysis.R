library(RERconverge)
CVHTree = readRDS("../RunRER/Output/CVHRemake/CVHRemakeBinaryTree.rds")
write.tree(CVHTree, "Results/CVHRemakeTreeTweaks/CVHRemakeCombinedTree.tree")

CVHCombinedTree = read.tree("Results/CVHRemakeTreeTweaks/CVHRemakeCombinedTree.tree")
plotTree(CVHCombinedTree)
CVHCarnivoraTree = read.tree("Results/CVHRemakeTreeTweaks/CVHRemakeCarnivoraTree.tree")
plotTree(CVHCarnivoraTree)
CVHCetaceanTree = read.tree("Results/CVHRemakeTreeTweaks/CVHRemakeCetaceanTree.tree")
plotTree(CVHCetaceanTree)

mainTrees = readRDS("../RunRER/Data/RemadeTreesAllZoonomiaSpecies.rds")
RERObject = readRDS("../RunRER/Output/CVHRemake/CVHRemakeRERFile.rds")

source("Src/Reu/BayesFactorAnalysis.R")

combinationTree = CVHCombinedTree; subclade1Name = "Carnivora"; subclade1Tree = CVHCarnivoraTree; subclade2Name = "Cetacean"; subclade2Tree = CVHCetaceanTree


CVHBayes = calculateBayesFactor(mainTrees, RERObject, CVHCombinedTree, "Carnivora", CVHCarnivoraTree, "Cetacean", CVHCetaceanTree)
CVHBayes2 = CVHBayes
CVHBayes2$ratio = NA
for(i in 1:nrow(CVHBayes2)){
  combinationSub1Ratio = CVHBayes2[i,1]/CVHBayes2[i,2]
  combinationSub2Ratio = CVHBayes2[i,1]/CVHBayes2[i,3]
  minRatio = min( combinationSub1Ratio, combinationSub2Ratio)
  CVHBayes2[i,4]=minRatio
}

BayesRatio = min()

CVHCorrelations = readRDS("../RunRER/Output/CVHRemake/CVHRemakeCorrelationFile.rds")
PermPval = readRDS("../RunRER/Output/CVHRemake/CVHRemakeCombinedPrunedFastAllPermulationsPValue.rds")

CVHCorrelations = cbind(CVHCorrelations, PermPval)
CVHCorrelations = cbind(CVHCorrelations, CVHBayes2[,c(2,3,1,4)])


CVHCorrelationsSort = CVHCorrelations[order(CVHCorrelations$PermPval),]
CVHCorrelationsSort
for(i in c(6,7,8,9)){
  CVHCorrelationsSort[,i] = sapply(X = CVHCorrelationsSort[,i], FUN = format, digits = 4, scientific = F)
}

CVHCorrelationsSort
saveRDS(CVHCorrelationsSort, "Output/CVHBayesResults.rds")
write.csv(CVHCorrelationsSort, "Output/CVHRemakeBayesCorrelations.csv")

#This needs the files in the "additional" folder within data. For it for find them, you'd need to move them out of additional and into the main data folder. 

mainTrees = readRDS("Data/RemadeTreesAllZoonomiaSpecies.rds")
RERObject = CVHRERs = readRDS("Output/CVHRemake/CVHRemakeRERFile.rds")
phenotypeTree = readRDS("Output/CVHRemake/CVHRemakeBinaryForegroundTree.rds")
foregroundSpecies = readRDS("Output/CVHRemake/CVHRemakeBinaryTreeForegroundSpecies.rds")
geneOfInterest = "FNDC11"
foregroundName = "Carnivore"
BackgroundName = "Herbivore"
CVHCorrelations = readRDS("Output/CVHRemake/CVHRemakeCorrelationFile.rds")
CVHPermulationP = readRDS("Output/CVHRemake/CVHRemakeCombinedPrunedFastAllPermulationsPValue.rds")
CorrelationData = CVHCorrelations
CorrelationData$permPValue = CVHPermulationP
CVHPaths = readRDS("Output/CVHRemake/CVHRemakePathsFile.rds")

source("rerViolinPlot.R")
quickViolin = function(geneInterest){
  plot = rerViolinPlot(mainTrees, CVHRERs, phenotypeTree, foregroundSpecies, geneInterest, "Carnivore", "Herbivore", "blue", "black", CorrelationData)
  print(plot)
}

quickViolin("SLC8A3")
library(RERconverge)
source("Src/Reu/plotBinaryTreeViewable.R")
source("Src/Reu/ZoonomTreeNameToCommon.R")
source("Src/Reu/ZonomNameConvertVector.R")
source("Src/Reu/ZonomNameConvertMatrixCommon.R")

mainTrees  = readRDS("../RunRER/Data/RemadeTreesAllZoonomiaSpecies.rds")
speciesFilter = readRDS("Data/CVHRemakeSpeciesFilter.rds")
binaryTree = readRDS("Data/CVHRemakeBinaryForegroundTree.rds")
RERs = readRDS("Data/CVHRemakeRERFile.rds")
paths = readRDS("Data/CVHRemakePathsFile.rds")
foregroundSpecies = readRDS("Data/CVHRemakeBinaryTreeForegroundSpecies.rds")

plotTree(mainTrees$masterTree)
plotTree(binaryTree)
plotBinaryTreeViewable(binaryTree)

testTree= mainTrees$masterTree
tipsNotInFilter = which(testTree$tip.label %in% speciesFilter)

tipsToDrop = testTree$tip.label[which(!testTree$tip.label %in% speciesFilter)]

testTree = drop.tip(testTree, tipsToDrop)


plotTree(binaryTree)
plotBinaryTreeViewable(binaryTree)
plotBinaryTreeViewable(trimmedTree)
plotTree(trimmedTree)
edgelabels()

index = "BCAT2"

grep(index, names(mainTrees$trees))
mainTrees$trees[14769]

par(mfrow = c(1,2))

plotTreeLabelBranchlength = function(inTree, mainTrees, index, speciesFilter){
  plotBinaryTreeViewable = function(inTree){
    treeDisplayable = inTree
    treeDisplayable$edge.length = replace(treeDisplayable$edge.length, treeDisplayable$edge.length==0, 0.5)
    treeDisplayable$edge.length = replace(treeDisplayable$edge.length, treeDisplayable$edge.length==1, 4)
    
    plotTreeHighlightBranches(treeDisplayable, hlspecies=which(inTree$edge.length >= 1), hlcols="blue",)
    return()
  }
  
  indexNumber = grep(index, names(mainTrees$trees))
  RerTree = mainTrees$trees[[indexNumber]]
  
  tipsNotInFilter = which(RerTree$tip.label %in% speciesFilter)
  tipsToDrop = RerTree$tip.label[which(!RerTree$tip.label %in% speciesFilter)]
  
  trimmedTree = drop.tip(RerTree, tipsToDrop)
  
  
  
}

all.equal(testTree, RerTree)

inTree = binaryTree
plotBinaryTreeViewable = function(inTree){
  treeDisplayable = inTree
  treeDisplayable$edge.length = replace(treeDisplayable$edge.length, treeDisplayable$edge.length==0, 0.5)
  treeDisplayable$edge.length = replace(treeDisplayable$edge.length, treeDisplayable$edge.length==1, 4)
  
  plotTreeHighlightBranches(treeDisplayable, hlspecies=which(inTree$edge.length >= 1), hlcols="blue",)
  return()
}

plotTree(trimmedTree)
??Tree

treePlotRers(mainTrees, RERs, "BCAT2", phenv = paths)

returnRersAsTree(mainTrees, RERs, "BCAT2", phenv = paths)

plotTreeHighlightBranches(trimmedTree, hlspecies = foregroundSpecies, hlcols = "blue")

fgSpecies = ZonomNameConvertVectorCommon(foregroundSpecies)

plotTree(trimmedTree)
commonTree = ZoonomTreeNameToCommon(trimmedTree)


plotTreeHighlightBranches(commonTree, hlspecies = fgSpecies, hlcols = "blue")
title(main = "BCAT2")

plotTreeBranchlength = function(mainTrees, index, speciesFilter){

  indexNumber = grep(index, names(mainTrees$trees))
  RerTree = mainTrees$trees[[indexNumber[1]]]
  tipsNotInFilter = which(RerTree$tip.label %in% speciesFilter)
  tipsToDrop = RerTree$tip.label[which(!RerTree$tip.label %in% speciesFilter)]
  
  trimmedTree = drop.tip(RerTree, tipsToDrop)
  commonTree = ZoonomTreeNameToCommon(trimmedTree)
  plotTreeHighlightBranches(commonTree, hlspecies = fgSpecies, hlcols = "blue")
  title(main = index)
}

plotTreeBranchlength(mainTrees, "BCAT2", speciesFilter)
plotTreeBranchlength(mainTrees, "BCAT1", speciesFilter)
plotTreeBranchlength(mainTrees, "SDSL", speciesFilter)
plotTreeBranchlength(mainTrees, "SDS", speciesFilter)


?plotRers()

commonRers = ZonomNameConvertMatrixCommon(RERs)
commonPaths = ZonomNameConvertVectorCommon(paths)

plotRers(commonRers, "BCAT2", paths)
plotRers(commonRers, "BCAT2", paths, sortrers = T)
plotRers(commonRers, "BCAT1", paths)
plotRers(commonRers, "BCAT1", paths, sortrers = T)
plotRers(commonRers, "SDS", paths)
plotRers(commonRers, "SDS", paths, sortrers = T)
plotRers(commonRers, "SDSL", paths)
plotRers(commonRers, "SDSL", paths, sortrers = T)


dev.off(); dev.new()

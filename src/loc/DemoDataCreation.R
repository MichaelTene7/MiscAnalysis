library(RERconverge)


mainData = readRDS("Data/newHillerMainTrees.rds")

plotTree(mainData$masterTree)

rerpath = find.package('RERconverge')

demoData = mainData
demoData$trees = demoData$trees[1:20]

source("Src/Reu/ZoonomTreeNameToCommon.R")

commonMasterTree = demoData$masterTree

ZoonomTreeNameToCommon(commonMasterTree, manualAnnotLocation = "Data/HillerZoonomiaPhenotypeTable.csv")

demoData$masterTree = ZoonomTreeNameToCommon(demoData$masterTree, manualAnnotLocation = "Data/HillerZoonomiaPhenotypeTable.csv")
for(i in 1:length(demoData$trees)){
  demoData$trees[[i]] = ZoonomTreeNameToCommon(demoData$trees[[i]], manualAnnotLocation = "Data/HillerZoonomiaPhenotypeTable.csv")
}


plotTree(demoData$masterTree)

demoData$masterTree$tip.label[seq(1, length(demoData$masterTree$tip.label), 4)]

droplist = demoData$masterTree$tip.label[-seq(1, length(demoData$masterTree$tip.label), 2)]

dropTree = drop.tip(demoData$masterTree, droplist)
droplist2 = dropTree$tip.label[-seq(1, length(dropTree$tip.label), 2)]

droplist3 = append(droplist, droplist2)

testTree = drop.tip(demoData$masterTree, droplist3)
plotTree(testTree)

trimmedData = demoData

trimmedData$masterTree = drop.tip(trimmedData$masterTree, droplist3)
for(i in 1:length(trimmedData$trees)){
  demoData$trees[[i]] = drop.tip(demoData$trees[[i]], droplist3)
}


saveRDS(trimmedData, "Data/DemoData.rds")



?seq

toytreefile = "subsetMammalGeneTrees.txt"
toyTrees=readTrees(paste(rerpath,"/extdata/",toytreefile,sep=""), max.read = 200)

plotTree(toyTrees$masterTree)

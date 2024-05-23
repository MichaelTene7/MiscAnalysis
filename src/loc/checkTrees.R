clusterHillerNames = read.csv("Data/120files.csv")


clusterHillerNames = unlist(clusterHillerNames)

clusterHillerNames = sub("\\..*", "", clusterHillerNames)

oldHillerTrees = readRDS("Data/mam120aa_trees.rds")

oldHillerNames = names(oldHillerTrees$trees)

clusterHillerNames = toupper(clusterHillerNames)

onlyClusterGenes = clusterHillerNames[which(!clusterHillerNames %in% oldHillerNames)]
names(clusterHillerNames) = NULL


clusterHillerNamesDf = data.frame(onlyClusterGenes)
write.csv(clusterHillerNamesDf, "Output/clusterHillerGenesNotInMaintrees.csv")


missingGenes = oldHillerNames[which(!oldHillerNames %in% clusterHillerNames)]
missingGenesDf = data.frame(missingGenes)
write.csv(missingGenesDf, "Output/missingHillerGenesFromMaintrees.csv")


testTree = read.tree("OUtput/newHillerMasterTree.txt")
plotTree(testTree)
find_nonbinary_nodes(testTree)
tree = testTree

find_nonbinary_nodes <- function(tree) {
  startNodes = tree$edge[,1]
  startNodes = table(startNodes)
  nonBinNodes = names(startNodes)[!startNodes ==2]
  message(nonBinNodes)
  message(paste("Number non-binary nodes:", length(nonBinNodes)))
  return(nonBinNodes)
}

tree$edge.length[1]

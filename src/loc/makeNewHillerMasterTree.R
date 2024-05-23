library(RERconverge)
source("Src/Reu/ZoonomTreeNameToCommon.R")
source("Src/Reu/ZonomNameConvertVector.R")
library(TreeTools)

zoonomiaData = readRDS("Data/RemadeTreesAllZoonomiaSpecies.rds")
hillerConversionLocation = "Data/HillerZoonomiaPhenotypeTable.csv"
hillerConverstionTable = read.csv(hillerConversionLocation)
hillerData = readRDS("Data/mam120aa_trees.rds")

zoonomiaMaster = zoonomiaData$masterTree
zoonomiaCommon = ZoonomTreeNameToCommon(zoonomiaMaster)
hillerMaster = hillerData$masterTree
hillerCommon = ZoonomTreeNameToCommon(hillerMaster, manualAnnotLocation = hillerConversionLocation)

noZoTips = hillerConverstionTable$FaName[which(is.na(hillerConverstionTable$Zoonomia)) ]
extraMissingTipsZoonomia = hillerConverstionTable$Zoonomia[!hillerConverstionTable$Zoonomia %in% zoonomiaMaster$tip.label & !is.na(hillerConverstionTable$Zoonomia)]
extraMissingTips = hillerConverstionTable$FaName[match(extraMissingTipsZoonomia, hillerConverstionTable$Zoonomia)]


tipsToFix = append(noZoTips, extraMissingTips)
tipsToFixCommon = ZonomNameConvertVectorCommon(tipsToFix, manualAnnotLocation = hillerConversionLocation)
extraMissingTipsCommon = ZonomNameConvertVectorCommon(extraMissingTips, manualAnnotLocation = hillerConversionLocation)

plotTreeHighlightBranches(hillerMaster, hlspecies = tipsToFix)
plotTreeHighlightBranches(zoonomiaCommon, hlspecies = "Manatee")
plotTreeHighlightBranches(hillerCommon, hlspecies = tipsToFixCommon)

#Build the new tree

newTree = zoonomiaMaster
tipsToDrop = newTree$tip.label[!newTree$tip.label %in% hillerConverstionTable$Zoonomia]
newTree = drop.tip(newTree, tipsToDrop)

zoonomiaTipNames = newTree$tip.label
hillerTipNames = hillerConverstionTable$FaName[match(zoonomiaTipNames, hillerConverstionTable$Zoonomia)]

newTree$tip.label = hillerTipNames
plotTreeHighlightBranches(newTree, hlspecies = tipsToFix)
newTreeAdditions = newTree

updatePlots = function(){
plot(newTreeAdditions, show.node.label = T, show.tip.label = T, cex = 0.8)
nodelabels(col = "red", adj = c(0, -0), frame = "none")
tiplabels(col = "blue", frame = "none")
edgelabels(col = "darkgreen", frame = "none")
plotTreeHighlightBranches(hillerMaster, hlspecies = tipsToFix)
}



# --- add the internal banrches that were missing ---
newTreeAdditions = newTree

tipsToFix[6]
plotTreeHighlightBranches(newTreeAdditions, hlspecies = c("musFur1"))
newTreeAdditions = AddTip(newTreeAdditions, 99, tipsToFix[6], edgeLength = newTreeAdditions$edge.length[66])
#updatePlots()

tipsToFix[4]
plotTreeHighlightBranches(newTreeAdditions, hlspecies = c("balAcu1"))
newTreeAdditions = AddTip(newTreeAdditions, 67, tipsToFix[4], edgeLength = (newTreeAdditions$edge.length[59]/2))
plotTreeHighlightBranches(newTreeAdditions, hlspecies = tipsToFix)

tipsToFix[3]
plotTreeHighlightBranches(newTreeAdditions, hlspecies = c("lipVex1", "HLdelLeu1"))
newTreeAdditions = AddTip(newTreeAdditions, 138, tipsToFix[3], edgeLength = (newTreeAdditions$edge.length[62]))
plotTreeHighlightBranches(newTreeAdditions, hlspecies = tipsToFix)

tipsToFix[5]
plotTreeHighlightBranches(newTreeAdditions, hlspecies = c("HLodoVir1"))
newTreeAdditions = AddTip(newTreeAdditions, 72, tipsToFix[5], edgeLength = (newTreeAdditions$edge.length[91]/2))
plotTreeHighlightBranches(newTreeAdditions, hlspecies = tipsToFix)

tipsToFix[1]
plotTreeHighlightBranches(newTreeAdditions, hlspecies = c("nomLeu3", "aotNan1"))
newTreeAdditions = AddTip(newTreeAdditions, 109, tipsToFix[1], edgeLength = (newTreeAdditions$edge.length[39]+newTreeAdditions$edge.length[40]+newTreeAdditions$edge.length[41]))
plotTreeHighlightBranches(newTreeAdditions, hlspecies = tipsToFix)

tipsToFix[2]
plotTreeHighlightBranches(newTreeAdditions, hlspecies = c("otoGar3", "tarSyr2"))
newTreeAdditions = AddTip(newTreeAdditions, 109, tipsToFix[2], edgeLength = (newTreeAdditions$edge.length[3]+newTreeAdditions$edge.length[4]))
plotTreeHighlightBranches(newTreeAdditions, hlspecies = tipsToFix)

plotTreeHighlightBranches(newTreeAdditions, hlspecies = tipsToFix)


# --- Binarize by taking the middle clade and making it a sister (sister clades based on the hiller tree)

#At this point, I want to move/attach whole clades. However, that's much easier to do in eteTools, so I am just goin to add blank tips in the correct locatiosn to replace with the trees. 
newTreeAdditions = AddTip(newTreeAdditions, 108, "midCladeHere", edgeLength = (newTreeAdditions$edge.length[56]))

zonomiaMidClade = extract.clade(newTreeAdditions, 138, root.edge = 0)
zoonomiaMidTips = zonomiaSisterClade$tip.label

zoonomiaSisterPair = newTreeAdditions
zoonomiaSisterPair = drop.tip(zoonomiaSisterPair, zoonomiaMidTips)

attachPointTip = which(zoonomiaSisterPair$tip.label == "midCladeHere")

plotTreeHighlightBranches(zoonomiaSisterPair, hlspecies = tipsToFix)

zoonomiaBinary = bind.tree(zoonomiaSisterPair, zonomiaMidClade, where = attachPointTip)
plotTreeHighlightBranches(zoonomiaBinary, hlspecies = tipsToFix)


# ---- Make a tree with the non-zoonomia species -----

#Make a tree missing all but one of the zoonomia species
zoonomiaTips = which(hillerMaster$tip.label %in% newTreeAdditions$tip.label)
zoonomiaTipsBarOne = zoonomiaTips[-1]

missingOutgroup = drop.tip(hillerMaster, zoonomiaTipsBarOne)
plotTreeHighlightBranches(missingOutgroup, hlspecies = tipsToFix)

#fix the zoonomia tip to correct name and branch length 
zoonomiaTip = which(missingOutgroup$tip.label == hillerMaster$tip.label[1])
missingOutgroup$tip.label[zoonomiaTip] = "zoonomiaTreeHere"
zoonomiaBranch = which(missingOutgroup$edge[,2] == zoonomiaTip)

outgroupToMainBranchLength = hillerMaster$edge.length[2]

missingOutgroup$edge.length[zoonomiaBranch] = outgroupToMainBranchLength

plotTreeHighlightBranches(missingOutgroup, hlspecies = tipsToFix)

# -- binzarize the non-zoonomia by making platypus a double outgroup --

platypustip = which(missingOutgroup$tip.label == "ornAna2")
platypusBranch = which(missingOutgroup$edge[,2] == platypustip)
platypusBranchLength = missingOutgroup$edge.length[platypusBranch]
missingOutgroupBinary = missingOutgroup
missingOutgroupBinary = drop.tip(missingOutgroup, "ornAna2")
plotTreeHighlightBranches(missingOutgroupBinary, hlspecies = tipsToFix)


missingOutgroupBinary = AddTip(missingOutgroupBinary, RootNode(missingOutgroupBinary), "ornAna2", edgeLength = (platypusBranchLength-missingOutgroupBinary$edge.length[18]))
plotTreeHighlightBranches(missingOutgroupBinary, hlspecies = tipsToFix)


# -- Attach the zoonomia tree to the outgroup --- 

landingPointTip = which(missingOutgroupBinary$tip.label == "zoonomiaTreeHere")
finalTree = bind.tree(missingOutgroupBinary, zoonomiaBinary, where = landingPointTip)
plotTreeHighlightBranches(finalTree, hlspecies = tipsToFix)


finalTree

write.tree(finalTree, "OUtput/newHillerMasterTree.txt")



# - confirm that tree is binary - 
find_nonbinary_nodes <- function(tree) {
  startNodes = tree$edge[,1]
  startNodes = table(startNodes)
  nonBinNodes = names(startNodes)[!startNodes ==2]
  message(nonBinNodes)
  message(paste("Number non-binary nodes:", length(nonBinNodes)))
  return(nonBinNodes)
}

find_nonbinary_nodes(finalTree)


























# ---- Old Code ---- 

plotTreeHighlightBranches(newTreeAdditions, hlspecies = tipsToFix)


plotTreeHighlightBranches(midClade, hlspecies = tipsToFix)
midClade = extract.clade(newTreeAdditions, interactive = T)

testZonom = newTreeAdditions
testZonom = drop.tip(testZonom, 138)

#





















write.tree(newTreeAdditions, "Results/newTreeAdditions.txt")
# That tree is currently not binary. 
#At this point, the tree needs to be made binary by nesting the pika clade with the monkey clade (based on the 120 tree). 
ZoonomTreeNameToCommon(newTreeAdditions, manualAnnotLocation = hillerConversionLocation)



nodelabels(col = "red", adj = c(0, -0), frame = "none")
# Do this by first removing the non-sister clade from the tree
preOrderedAdditionsTree = Preorder(newTreeAdditions)
ZoonomTreeNameToCommon(preOrderedAdditionsTree, manualAnnotLocation = hillerConversionLocation)
nodelabels(col = "red", adj = c(0, -0), frame = "none")
nonPikaLemur = Subtree(preOrderedAdditionsTree, 158)
ZoonomTreeNameToCommon(nonPikaLemur, manualAnnotLocation = hillerConversionLocation)
plot(nonPikaLemur)
















# ---- Old code involving file edditing ----------------

#All of the remaining tips form an outgroup to the zoonomia master tree. Going to see if I can attach them all at once. 

missingOutgroup = drop.tip(hillerMaster, hillerMaster$tip.label[which(hillerMaster$tip.label %in% newTreeAdditions$tip.label)])
plotTreeHighlightBranches(missingOutgroup, hlspecies = tipsToFix)

# Need to binarize by making platypus an outgroup 
platypusBranchLength = missingOutgroup$edge.length[23]
missingOutgroupBinary = missingOutgroup
missingOutgroupBinary = drop.tip(missingOutgroup, "ornAna2")
plotTreeHighlightBranches(missingOutgroupBinary, hlspecies = tipsToFix)
newTreeAdditions = AddTip(newTreeAdditions, 109, tipsToFix[2], edgeLength = (newTreeAdditions$edge.length[3]+newTreeAdditions$edge.length[4]))





write.tree(newTreeAdditions, "Results/newAdditionsOut.txt") 
write.tree(missingOutgroup, "Results/missingOutgroup.txt") 


#At this point, the tree needs to be made binary by testing the pika clade with the monkey clade (based on the 120 tree). 

binaryAdditionsTree = read.tree("Results/binaryUnmergedHillerTree.txt")
plotTreeHighlightBranches(binaryAdditionsTree, hlspecies = tipsToFix)



#At this point I am going to just attach this as a newick tree directly because I am trying to add a whole 

mergedTree = read.tree("Results/mergedNewHiller.txt")



# that didn't work -- they are a shared outgroup, not nested outgroups. let's try that again, but staged. 


missingOutgroup1 = drop.tip(missingOutgroup, c("HLphaCin1", "sarHar1", "monDom5", "ornAna2"))
write.tree(missingOutgroup1, "Results/missingOutgroup1.txt")

mergedTree = read.tree("Results/mergedNewHillerStage1.txt")



missingOutgroup2 = drop.tip(missingOutgroup, missingOutgroup$tip.label[!missingOutgroup$tip.label %in% c("HLphaCin1", "sarHar1", "monDom5", "ornAna2")])
write.tree(missingOutgroup2, "Results/missingOutgroup2.txt")

mergedTree = read.tree("Results/mergedNewHillerStage2.txt")
plotTreeHighlightBranches(mergedTree, hlspecies = tipsToFix)


#Binaryize the tree by clustering the pika clade with the monkey clade, which is the hiller layout 
mergedTree = read.tree("Results/mergedNewHillerStage3.txt")


mergedTree = read.tree("Results/binaryMergedHillerTree.txt")
plotTreeHighlightBranches(mergedTree, hlspecies = tipsToFix)
is.binary(mergedTree)


newHilleMasterTree = mergedTree



write.tree(newHilleMasterTree, "Output/newHillerMasterTree.txt")



mergedTree$edge



startNodes = mergedTree$edge[,1]
startNodes = table(startNodes)
nonBinNodes = names(startNodes)[!startNodes ==2]


find_nonbinary_nodes <- function(tree) {
  nonbinary_nodes <- integer(0)
  for (i in 1:Nnode(tree)) {
    if (length(tree$edge[,1][tree$edge[,2] == i]) != 2) {
      
      nonbinary_nodes <- c(nonbinary_nodes, i)
    }
  }
  
  return(nonbinary_nodes)
}

find_nonbinary_nodes(mergedTree)




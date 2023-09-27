library(RERconverge) #load RERconverge package
library(RERconverge)
library(tools)
library(devtools)

# --- non permulated allINsectivory (AI) ----
nonPermInsectivoryPVal = readRDS("Data/allInsectivoryCorrelationFile.rds")
sortedNonPermInsectivoryPVal= nonPermInsectivoryPVal[order(nonPermInsectivoryPVal$p.adj),]
topNonPermAIgenes = rownames(sortedNonPermInsectivoryPVal)[1:30]

# --- Permualted old Insectivory (OAI) ---
PermulatedOldAllInsectivoryPVal = readRDS("Data/oldAllInsectivoryCombinedAppendedPermulationsPValue.rds")
sortedPermulatedOldAllInsectivoryPVal = PermulatedOldAllInsectivoryPVal[order(PermulatedOldAllInsectivoryPVal)]
topPermOAIgenes = names(sortedPermulatedOldAllInsectivoryPVal)[1:30]
topPermOAIgenes



# --- Permulated new Insectivory (NAI) ----
PermulatedNewAllInsectivoryPVals = readRDS("Data/allInsectivoryCombinedAppendedPermulationsPValue.rds")
sortedPermulatedNewAllInsectivoryPVals = PermulatedNewAllInsectivoryPVals[order(PermulatedNewAllInsectivoryPVals)]
topPermedNAIgenes = names(sortedPermulatedNewAllInsectivoryPVals)[1:30]
topPermedNAIgenes

#--- Insectivory Overlaps ---

#perm-no-perm OAI Overlap
overlapOAIPnp = topNonPermAIgenes[topNonPermAIgenes %in% topPermOAIgenes]
overlapOAIPnp

#Perm-no-perm NAI Overlap
overlapNAIPnp = topPermOAIgenes[topPermedNAIgenes %in% topNonPermAIgenes]
overlapNAIPnp

#Perm-NAI-Perm-OAI Overlap
overlapNvOPermAI = topPermOAIgenes[topPermOAIgenes %in% topPermedNAIgenes]
overlapNvOPermAI



# -------  Carn V Herbs --------

# ---- non permulated New CarnvHerbs (New CVH)
nonPermNewCVHpVal = readRDS("Data/carnvHerbsCorrelationFile.rds")
sortedNonPermNewCVHpVal = nonPermNewCVHpVal[order(nonPermNewCVHpVal$p.adj),]
topNonPermNewCVHgenes = rownames(sortedNonPermNewCVHpVal)[1:30]
topNonPermNewCVHgenes

# ---- permulated New CarnvHerbs (New CVH)
permNewCVHpVal = readRDS("Data/carnvHerbsCombinedPrunedFastAppendedPermulationsPValue.rds")
sortedPermNewCVHpVal = permNewCVHpVal[order(permNewCVHpVal)]
topPermNewCvHgenes = names(sortedPermNewCVHpVal)[6:36]                          #First six are either 0 in new or NA in old
topPermNewCvHgenes

# ---- non permulated Old CarnvHerbs (Old CVH) 
oldCarnvHerbspVal = read.csv("Data/CorrelationFishCarnHerbLaurasiatheria_wminsp_wnopermp_wcount_waddlperms0907.csv")
sortedNoPermOldCVHpVal = oldCarnvHerbspVal[order(oldCarnvHerbspVal$p.adj),1:10]
topNoPermOldCVHgenes = sortedNoPermOldCVHpVal$X[1:30]
topNoPermOldCVHgenes

# ---- permulated Old CarvHerbs (Old CVH)
oldCarnvHerbspVal = read.csv("Data/CorrelationFishCarnHerbLaurasiatheria_wminsp_wnopermp_wcount_waddlperms0907.csv")
sortedOldCVHpVal= oldCarnvHerbspVal[order(oldCarnvHerbspVal$permp.2sided),1:10]
topPermOldCVHgenes = sortedOldCVHpVal$X[1:30]
topPermOldCVHgenes

# --- CVH Overlaps ---

#Perm no perm new CVH
overlapNewCVHPnp = topNonPermNewCVHgenes[topNonPermNewCVHgenes %in% topPermNewCvHgenes]
overlapNewCVHPnp

#Perm no Perm old CVH
overlapOldCVHPnp = topNoPermOldCVHgenes[topNoPermOldCVHgenes %in% topPermOldCVHgenes]
overlapOldCVHPnp

# New Vs Old Permulated CVH
overlapOldvNewPermCVH = topPermNewCvHgenes[topPermNewCvHgenes %in% topPermOldCVHgenes]
overlapOldvNewPermCVH

# New Vs Old No-permulated CVH
overlapNewvOldNoPermCVH = topNoPermOldCVHgenes[topNoPermOldCVHgenes %in% topNonPermNewCVHgenes]
overlapNewvOldNoPermCVH

# --- Expand top to 100 
topNonPermNewCVHgenes = rownames(sortedNonPermNewCVHpVal)[1:100]
topPermNewCvHgenes = names(sortedPermNewCVHpVal)[6:106]
topNoPermOldCVHgenes = sortedNoPermOldCVHpVal$X[1:100]
topPermOldCVHgenes = sortedOldCVHpVal$X[1:100]

topNonPermAIgenes = rownames(sortedNonPermInsectivoryPVal)[1:100]
topPermOAIgenes = names(sortedPermulatedOldAllInsectivoryPVal)[1:100]
topPermedNAIgenes = names(sortedPermulatedNewAllInsectivoryPVals)[1:100]

# --- CVH Expanded Overlaps ---

#Perm no perm new CVH
overlapNewCVHPnp = topNonPermNewCVHgenes[topNonPermNewCVHgenes %in% topPermNewCvHgenes]
overlapNewCVHPnp

#Perm no Perm old CVH
overlapOldCVHPnp = topNoPermOldCVHgenes[topNoPermOldCVHgenes %in% topPermOldCVHgenes]
overlapOldCVHPnp

# New Vs Old Permulated CVH
overlapOldvNewPermCVH = topPermNewCvHgenes[topPermNewCvHgenes %in% topPermOldCVHgenes]
overlapOldvNewPermCVH

# New Vs Old No-permulated CVH
overlapNewvOldNoPermCVH = topNoPermOldCVHgenes[topNoPermOldCVHgenes %in% topNonPermNewCVHgenes]
overlapNewvOldNoPermCVH


# -- Expanded Insectivory overlaps --

#perm-no-perm OAI Overlap
overlapOAIPnp = topNonPermAIgenes[topNonPermAIgenes %in% topPermOAIgenes]
overlapOAIPnp

#Perm-no-perm NAI Overlap
overlapNAIPnp = topPermOAIgenes[topPermedNAIgenes %in% topNonPermAIgenes]
overlapNAIPnp

#Perm-NAI-Perm-OAI Overlap
overlapNvOPermAI = topPermOAIgenes[topPermOAIgenes %in% topPermedNAIgenes]
overlapNvOPermAI

# merge dataframes 
names(nonPermNewCVHpVal)
reformatNonPermNewCVHpVal = nonPermNewCVHpVal
names(reformatNonPermNewCVHpVal) = c("NewRho", "NewN", "NewP", "Newp.adj")
CVHCombined =  cbind(oldCarnvHerbspVal, reformatNonPermNewCVHpVal)
CVHCombined = cbind(CVHCombined, permNewCVHpVal)
names(CVHCombined)[19] = "NewPermP"

CVHCombinedSortNewPerm = CVHCombined[order(CVHCombined$NewPermP),]
NewPermComparePs = CVHCombinedSortNewPerm[,c(1,5,6,17,18,19)]
CVHCombinedSortOldPerm = CVHCombined[order(CVHCombined$permp.2sided),]
OldPermComparePs = CVHCombinedSortOldPerm[,c(1,5,6,17,18,19)]
CVHCombinedSortOldPerm1Side = CVHCombined[order(CVHCombined$permp.1sided),]
OldPerm1SideComparePs = CVHCombinedSortOldPerm[,c(1,5,6,7,17,18,19)]



# -- Analysis of foreground lists 

#From the makeTreesBinary 
binaryForegroundTree = readRDS("Data/carnvHerbsBinaryForegroundTree.rds")
speciesList = readRDS("Data/carnvHerbsSpeciesFilter.rds")

testTreeDisplayable = binaryForegroundTree
replace(testTreeDisplayable$edge.length, testTreeDisplayable$edge.length==0, 0.5)
replace(testTreeDisplayable$edge.length, testTreeDisplayable$edge.length==1, 4)

plotTreeHighlightBranches(testTreeDisplayable, hlspecies=which(binaryForegroundTree$edge.length==1), hlcols="blue",)


#From sistersListGeneration
ForegroundSpecies = readRDS("Data/carnvHerbsForegroundSpecies.rds")
phenotypeVector = readRDS("Data/carnvHerbsphenotypeVector.rds")

#Is the phenotype vector of the species filter the same?
length(speciesList)
length(phenotypeVector)
all.equal(names(phenotypeVector), speciesList)
  #Yes, the are 

#Are the phenotype vector = 1 the same as the foreground species?
all.equal(names(phenotypeVector[phenotypeVector ==1]), ForegroundSpecies)
  #Yes, they are. 

#Importing the carnvHerb tree from sol 

importedOldTree = read.tree("Data/Culled_0511_FishCarnvHerb.txt")
plotTree(importedOldTree)
all.equal(importedOldTree, binaryForegroundTree)
  #The binary foreground tree that I used, and the one from sol are identical -- like I thought they would be, as the new one's origin is a copy of the old one.  
importedTreeDisplayable = importedOldTree
replace(testTreeDisplayable$edge.length, testTreeDisplayable$edge.length==0, 0.5)
replace(testTreeDisplayable$edge.length, testTreeDisplayable$edge.length==1, 4)

plotTreeHighlightBranches(testTreeDisplayable, hlspecies=which(importedOldTree$edge.length==1), hlcols="blue", main = "Culled_0511_FishCarnvHerb.txt")

# -- Checking N for genes --- 
sortedNOldCarnvHerbspVal = oldCarnvHerbspVal[order(oldCarnvHerbspVal$N, decreasing = TRUE),1:10]
sortedNOldCarnvHerbspVal
?order()
newCVHRER = readRDS("Data/carnvHerbsRERFile12-15-22.rds")


# Compare maintree files 
load("Data/Phylotrees_FishCarn.Rdata")

mainTrees = readRDS("../RunRER/Data/RemadeTreesAllZoonomiaSpecies.rds")
all.equal(mainTrees, FishTree)
  #maintrees and fishtree and notably different  

#Compare species list
manualAnnot <- read.csv("Data/MA_Laura.csv") #read in spreadsheet
lauraManualAnnot <- manualAnnot[manualAnnot$Laurasiatheria==1,] #subset data frame to just Laurasiatheria species
laurasp <- lauraManualAnnot$FaName[lauraManualAnnot$FaName %in% importedOldTree$tip.label] #get overlap of Laurasiatheria and species in phenotype tree
laurasp
all.equal(speciesList, laurasp)
spListOverlap = speciesList[speciesList %in% laurasp]
length(spListOverlap)
newSpListEntries = speciesList[!speciesList %in% laurasp]
newSpListEntries
length(newSpListEntries)
    #See if the new entires are not in the old tree 
FishTree$masterTree$tip.label
mainTrees$masterTree$tip.label

newSPListNotInMTree = newSpListEntries[!newSpListEntries %in% FishTree$masterTree$tip.label]
  #No, the new species in the expanded list are in fact on the old master tree. 
newSPListNotInPTree = newSpListEntries[!newSpListEntries %in% importedOldTree$tip.label]
  #The are also all on the phenotype tree as well 
#Seeing where on the tree the left out ones are 
plotTreeHighlightBranches(testTreeDisplayable, hlspecies=newSpListEntries, hlcols="blue", main = "Differing species list")
#Seeing if the ones left out are not laura
notLauraManualAnnot = manualAnnot[manualAnnot$Laurasiatheria==0,]
newSpListEntriesNotLaura = newSpListEntries[!newSpListEntries %in% lauraManualAnnot$FaName]
newSpListEntriesNotNotLaura = newSpListEntries[!newSpListEntries %in% notLauraManualAnnot$FaName]
length(newSpListEntriesNotLaura)
  #The ones that were left out were not Laurasiatheria

#See if changing species list makes matching correlation files 

  pathsObject = tree2Paths(binaryForegroundTree, mainTrees, binarize=T, useSpecies = laurasp)
newSpListCorrelation = correlateWithBinaryPhenotype(newCVHRER, pathsObject, min.sp =35)
  #Compare the new one with nonPermNewCVHpVal and oldCarnvHerbspVal
speciesListCompare = cbind(newSpListCorrelation, nonPermNewCVHpVal, oldCarnvHerbspVal[,1:5])
names(speciesListCompare)[1:8] = c("newSLRho", "newSLN", "newSLP", "newSLp.adj", "CVHRho","CVHN","CVHP","CVHp.adj")
#Looks like the newSL is the same as the old results
oldCVHcorrelateWithBinaryPhenotypeCollumns = oldCarnvHerbspVal[,2:5]
rownames(oldCVHcorrelateWithBinaryPhenotypeCollumns) = oldCarnvHerbspVal[,1]

all.equal(newSpListCorrelation,oldCVHcorrelateWithBinaryPhenotypeCollumns)
#similar, but slightly different 
length(which(is.na(newSpListCorrelation)))
length(which(is.na(oldCVHcorrelateWithBinaryPhenotypeCollumns)))
length(which(which(is.na(oldCVHcorrelateWithBinaryPhenotypeCollumns)) %in% which(is.na(newSpListCorrelation))))

length(which(is.na(newSpListNewRERCorrel)))
length(which(which(is.na(oldCVHcorrelateWithBinaryPhenotypeCollumns)) %in% which(is.na(newSpListNewRERCorrel))))

#compare the RER values 
oldRERValues = readRDS("Data/mamRERCMU_FishCarn.rds")

redoRERValues = readRDS("Data/carnvHerbRERFileNewGeneration.rds")

all.equal(newCVHRER, oldRERValues)

all.equal(redoRERValues, oldRERValues)

all.equal(redoRERValues, newCVHRER)

saveRDS(laurasp, "Results/carnvHerbsSpeciesFilterCorrectCopyThisToOutputForUse.rds")


#compare colleration values with new sisterslists 

newSpListNewRERCorrel = readRDS("Data/carnvHerbsCorrelationFileNewSpNewRER.rds")
newSpListCorrelation
oldCVHcorrelateWithBinaryPhenotypeCollumns
newRunOldSpeciesListCorrelation = nonPermNewCVHpVal

all.equal(newSpListNewRERCorrel, newSpListCorrelation)
all.equal(newSpListNewRERCorrel, oldCVHcorrelateWithBinaryPhenotypeCollumns)
all.equal(newSpListCorrelation, oldCVHcorrelateWithBinaryPhenotypeCollumns)
plot(newSpListCorrelation$p.adj, oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)
NewCVHOldSpListRER = readRDS("Data/carnvHerbsRERFile.rds")

AttemptedReamkeOfOldPaths = tree2Paths(binaryForegroundTree, FishTree, binarize=T, useSpecies = laurasp)


AttemptedRemakeOfOldCorrelation = correlateWithBinaryPhenotype(oldRERValues, AttemptedReamkeOfOldPaths, min.sp =35)

all.equal(AttemptedRemakeOfOldCorrelation, oldCVHcorrelateWithBinaryPhenotypeCollumns)
#Have successfully remade the old correllation values using the FishTree main trees, the same binary foreground tree, and the RERs from the script. 
#Which means that the paths have also been 

OldRERnewPathCorrelation = correlateWithBinaryPhenotype(oldRERValues, pathsObject, min.sp =35)
all.equal(OldRERnewPathCorrelation, AttemptedRemakeOfOldCorrelation)
all.equal(OldRERnewPathCorrelation, AttemptedRemakeOfOldCorrelation)

all.equal(pathsObject, AttemptedReamkeOfOldPaths)

oldPathsNewRERsCorrelation = correlateWithBinaryPhenotype(redoRERValues, AttemptedReamkeOfOldPaths, min.sp =35)

length(which(is.na(NewCVHOldSpListRER)))/length(NewCVHOldSpListRER)
length(which(is.na(oldRERValues)))/length(oldRERValues)
length(which(is.na(redoRERValues)))/length(redoRERValues)

colnames(oldRERValues) %in% laurasp
colnames(oldRERValues) %in% laurasp

# Calculating mean relative difference without all.equal():
meanRelativeDifference = function (sample1, sample2){
  MRD = (mean(sample2, na.rm=T) - mean(sample1, na.rm=T))/mean(sample1, na.rm=T)
  MRD
}
meanRelativeDifference = function (sample1, sample2){
  MRD = (mean(sample2, na.rm=T) - mean(sample1, na.rm=T))/mean(sample1, na.rm=T)
  MRD
}

meanRelativeDifference(newSpListCorrelation$Rho, oldCVHcorrelateWithBinaryPhenotypeCollumns$Rho)
meanRelativeDifference(newSpListCorrelation$P, oldCVHcorrelateWithBinaryPhenotypeCollumns$P)
meanRelativeDifference(newSpListCorrelation$p.adj, oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)
meanRelativeDifference(newSpListCorrelation$N, oldCVHcorrelateWithBinaryPhenotypeCollumns$N)

all.equal(newSpListCorrelation, oldCVHcorrelateWithBinaryPhenotypeCollumns)
nostopAllEq(newSpListCorrelation$Rho, oldCVHcorrelateWithBinaryPhenotypeCollumns$Rho)
nostopAllEq(newSpListCorrelation$N, oldCVHcorrelateWithBinaryPhenotypeCollumns$N)
nostopAllEq(newSpListCorrelation$P, oldCVHcorrelateWithBinaryPhenotypeCollumns$P)
nostopAllEq(newSpListCorrelation$p.adj, oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)

all.equal(newSpListNewRERCorrel, newSpListCorrelation)
nostopAllEq(newSpListNewRERCorrel$Rho, newSpListCorrelation$Rho)
nostopAllEq(newSpListNewRERCorrel$N, newSpListCorrelation$N)
nostopAllEq(newSpListNewRERCorrel$P, newSpListCorrelation$P)
nostopAllEq(newSpListNewRERCorrel$p.adj, newSpListCorrelation$p.adj)

plot(newSpListNewRERCorrel$Rho, newSpListCorrelation$Rho)
abline(fit <- lm(newSpListNewRERCorrel$Rho ~ newSpListCorrelation$Rho), col='red')
legend("topright", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)))

plot(newSpListNewRERCorrel$Rho, oldCVHcorrelateWithBinaryPhenotypeCollumns$Rho)
abline(fit <- lm(newSpListNewRERCorrel$Rho ~ oldCVHcorrelateWithBinaryPhenotypeCollumns$Rho), col='red')
legend("topright", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)))

#Compare absolute p.adj values 
par(mfrow(1,2))
plot(newSpListNewRERCorrel$p.adj, oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)
abline(fit <- lm(newSpListNewRERCorrel$p.adj ~ oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj), col='red')
legend("topleft", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)))


plot(newSpListCorrelation$p.adj, oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)
abline(fit <- lm(newSpListCorrelation$p.adj ~ oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj), col='red')
legend("topleft", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)))

#
fit <- lm(newSpListCorrelation$Rho ~ oldCVHcorrelateWithBinaryPhenotypeCollumns$Rho)
summary(fit)$adj.r.squared

plot(newSpListCorrelation$Rho, oldCVHcorrelateWithBinaryPhenotypeCollumns$Rho)
abline(fit <- lm(newSpListCorrelation$Rho ~ oldCVHcorrelateWithBinaryPhenotypeCollumns$Rho), col='red')
legend("topright", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)))


all.equal(newSpListNewRERCorrel, oldCVHcorrelateWithBinaryPhenotypeCollumns)

orderednewSpListNewRERCorrel = newSpListNewRERCorrel[order(newSpListNewRERCorrel$Rho),]
rownames(orderednewSpListNewRERCorrel)
which(rownames(orderednewSpListNewRERCorrel) %in% "TMEM67")
genesNewRank = vector(length = 16209)
names(genesNewRank) = rownames(newSpListNewRERCorrel)
for(i in 1:16209){
  genesNewRank[i] = which(rownames(orderednewSpListNewRERCorrel) %in% names(genesNewRank)[i])
}
which(rownames(orderednewSpListNewRERCorrel) %in% names(genesNewRank)[1])

genesOldRank = vector(length = 16209)
names(genesOldRank) = rownames(oldCVHcorrelateWithBinaryPhenotypeCollumns)
for(i in 1:16209){
  genesOldRank[i] = which(rownames(oldCVHcorrelateWithBinaryPhenotypeCollumns) %in% names(genesOldRank)[i])
}
plot(genesNewRank, genesOldRank)



orderedPnewSpListNewRERCorrel = newSpListNewRERCorrel[order(newSpListNewRERCorrel$p.adj),]
rownames(orderednewSpListNewRERCorrel)
which(rownames(orderednewSpListNewRERCorrel) %in% "TMEM67")
genesNewRank = vector(length = 16209)
names(genesNewRank) = rownames(newSpListNewRERCorrel)
for(i in 1:16209){
  genesNewRank[i] = which(rownames(orderednewSpListNewRERCorrel) %in% names(genesNewRank)[i])
}
which(rownames(orderednewSpListNewRERCorrel) %in% names(genesNewRank)[1])

orderedoldCVHcorrelateWithBinaryPhenotypeCollumns = oldCVHcorrelateWithBinaryPhenotypeCollumns[order(oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj),]
genesOldRank = vector(length = 16209)
names(genesOldRank) = rownames(oldCVHcorrelateWithBinaryPhenotypeCollumns)
for(i in 1:16209){
  genesOldRank[i] = which(rownames(orderedoldCVHcorrelateWithBinaryPhenotypeCollumns) %in% names(genesOldRank)[i])
}
plot(genesNewRank, genesOldRank)

orderedoldCVHcorrelateWithBinaryPhenotypeCollumns[284,]
which(genesOldRank == 284)
genesOldRank[387]
genesNewRank[387]
length(which(rownames(orderedoldCVHcorrelateWithBinaryPhenotypeCollumns[1:3000,]) %in% rownames(orderednewSpListNewRERCorrel[1:3000,])))

rankCompare = data.frame(genesNewRank, genesOldRank)
rankCompare = rankCompare[order(rankCompare$genesNewRank, decreasing = F),]

genesOldRank[15000:16000]


#---

newSpNewRERCorrel =newSpListNewRERCorrel
OldCorrel = oldCVHcorrelateWithBinaryPhenotypeCollumns

orderednewSpNewRERCorrel = newSpNewRERCorrel[order(newSpNewRERCorrel$p.adj),]
orderedOldCorrel = OldCorrel[order(OldCorrel$p.adj),]

genesNewpRank = vector(length = 16209)
names(genesNewpRank) = rownames(newSpListNewRERCorrel)
for(i in 1:16209){
  genesNewpRank[i] = which(rownames(orderednewSpNewRERCorrel) %in% names(genesNewRank)[i])
}

genesOldpRank = vector(length = 16209)
names(genesOldpRank) = rownames(OldCorrel)
for(i in 1:16209){
  genesOldpRank[i] = which(rownames(orderedOldCorrel) %in% names(genesOldRank)[i])
}
plot(genesNewpRank, genesOldpRank)

orderedRhonewSpNewRERCorrel = newSpNewRERCorrel[order(newSpNewRERCorrel$Rho),]
genesNewRhoRank = vector(length = 16209)
names(genesNewRhoRank) = rownames(newSpListNewRERCorrel)
for(i in 1:16209){
  genesNewRhoRank[i] = which(rownames(orderedRhonewSpNewRERCorrel) %in% names(genesNewRank)[i])
}
plot(genesNewRhoRank, genesOldpRank)


#---

newSpMidRERCorrel =newSpListCorrelation
plot(newSpMidRERCorrel$p.adj, oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)
OldCorrel = oldCVHcorrelateWithBinaryPhenotypeCollumns

orderednewSpMidRERCorrel = newSpMidRERCorrel[order(newSpMidRERCorrel$p.adj),]
orderedOldCorrel = OldCorrel[order(OldCorrel$p.adj),]

genesMidRank = vector(length = 16209)
names(genesMidRank) = rownames(newSpMidRERCorrel)
for(i in 1:16209){
  genesMidRank[i] = which(rownames(orderednewSpMidRERCorrel) %in% names(genesMidRank)[i])
}

genesOldRank = vector(length = 16209)
names(genesOldRank) = rownames(OldCorrel)
for(i in 1:16209){
  genesOldRank[i] = which(rownames(orderedOldCorrel) %in% names(genesOldRank)[i])
}
plot(genesMidRank, genesOldRank)

#Compare the NewNew vs old 
NewNewVsOldCorrelCompare = cbind(newSpNewRERCorrel, OldCorrel)
names(NewNewVsOldCorrelCompare) = c("nnRho", "nnN", "nnP", "nnp.adj","oRho","oN", "oP", "op.adj")
plot(NewNewVsOldCorrelCompare$nnp.adj, NewNewVsOldCorrelCompare$op.adj)

orderedNewNewVsOldCorrelCompare = NewNewVsOldCorrelCompare[order(NewNewVsOldCorrelCompare$nnp.adj),]
plot(1:nrow(orderedNewNewVsOldCorrelCompare), orderedNewNewVsOldCorrelCompare$op.adj)
nnPosition = 1:16209
orderedNewNewVsOldCorrelCompare = cbind(orderedNewNewVsOldCorrelCompare, nnPosition)

orderedNewNewVsOldCorrelCompare = orderedNewNewVsOldCorrelCompare[order(orderedNewNewVsOldCorrelCompare$op.adj),]
oPosition = 1:16209
orderedNewNewVsOldCorrelCompare = cbind(orderedNewNewVsOldCorrelCompare, oPosition)

plot(orderedNewNewVsOldCorrelCompare$nnPosition, orderedNewNewVsOldCorrelCompare$oPosition)

{plot(orderedNewNewVsOldCorrelCompare$nnPosition, orderedNewNewVsOldCorrelCompare$nnp.adj)
plot(orderedNewNewVsOldCorrelCompare$oPosition, orderedNewNewVsOldCorrelCompare$op.adj)

plot(orderedNewNewVsOldCorrelCompare$oPosition, genesOldRank)
}
#Add in the middle to the comparission df
plot(newSpMidRERCorrel$mp.adj, orderedNewNewVsOldCorrelCompare$op.adj)
plot(newSpMidRERCorrel$mp.adj, orderedNewNewVsOldCorrelCompare$op.adj)
plot(orderedNewNewVsOldCorrelCompare$op.adj, oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)


newSpMidRERCorrel
names(newSpMidRERCorrel) = c("mRho", "mN", "mP", "mp.adj")
plot(newSpMidRERCorrel$mp.adj, oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)
orderedNewNewVsOldvsMidCorrelCompare = cbind(orderedNewNewVsOldCorrelCompare, newSpMidRERCorrel)
orderedNewNewVsOldvsMidCorrelCompare = orderedNewNewVsOldvsMidCorrelCompare[order(orderedNewNewVsOldvsMidCorrelCompare$mp.adj),]

#figure out what's cgoing on with the combined DF

nnvoCompare = cbind(newSpNewRERCorrel, OldCorrel)
all.equal(nnvoCompare[,8], oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)
nnvoCompareNamed = nnvoCompare
names(nnvoCompareNamed) = c("nnRho", "nnN", "nnP", "nnp.adj","oRho","oN", "oP", "op.adj")
normRank = 1:16209
nnvoCompareNamed = cbind(nnvoCompareNamed, normRank)


sortednnvoCompare = nnvoCompareNamed[order(nnvoCompareNamed$nnp.adj),]
nnRank = 1:16209
sortednnvoCompare = cbind(sortednnvoCompare, nnRank)
sortednnvoCompare = sortednnvoCompare[order(sortednnvoCompare$op.adj),]
oRank = 1:16209
sortednnvoCompare = cbind(sortednnvoCompare, oRank)
sortednnvoCompare = sortednnvoCompare[order(sortednnvoCompare$normRank),]

namedNewSpListCorrelation = newSpListCorrelation
names(namedNewSpListCorrelation) = c("mRho", "mN", "mP", "mp.adj")

all.equal(rownames(sortednnvoCompare), rownames(namedNewSpListCorrelation))

nnvovmCompare = cbind(sortednnvoCompare, namedNewSpListCorrelation)
sortednnvovmCompare = nnvovmCompare[order(nnvovmCompare$mp.adj),]
mRank = 1:16209
sortednnvovmCompare = cbind(sortednnvovmCompare, mRank)

#plot p.adj comparisions
styleplot = function(x, y, title = NULL){
  abline(fit <- lm(x ~ y), col='red')
  legend("topleft", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)), col = 'red')
  title(main = title)
}

topNNvOriginalPCompare = length(which(rownames(sortednnvovmCompare[order(sortednnvovmCompare$nnp.adj),])[1:1000] %in% rownames(sortednnvovmCompare[order(sortednnvovmCompare$op.adj),])[1:1000]))
topNNvOriginalPCompare
topNMvOriginalPCompare = length(which(rownames(sortednnvovmCompare[order(sortednnvovmCompare$mp.adj),])[1:1000] %in% rownames(sortednnvovmCompare[order(sortednnvovmCompare$op.adj),])[1:1000]))
topNMvOriginalPCompare

plot(sortednnvovmCompare$nnp.adj, sortednnvovmCompare$op.adj)
styleplot(sortednnvovmCompare$nnp.adj, sortednnvovmCompare$op.adj, "NewNew vs Original p.adj")
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", topNNvOriginalPCompare))

plot(sortednnvovmCompare$mp.adj, sortednnvovmCompare$op.adj)
styleplot(sortednnvovmCompare$mp.adj, sortednnvovmCompare$op.adj, "NewOld vs Original p.adj")
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", topNMvOriginalPCompare))



#plot rank Comparisons: 

topNNvOriginalRankCompare = length(which(rownames(sortednnvovmCompare[order(sortednnvovmCompare$nnRank),])[1:1000] %in% rownames(sortednnvovmCompare[order(sortednnvovmCompare$oRank),])[1:1000]))
topNNvOriginalRankCompare
topNMvOriginalRankCompare = length(which(rownames(sortednnvovmCompare[order(sortednnvovmCompare$mRank),])[1:1000] %in% rownames(sortednnvovmCompare[order(sortednnvovmCompare$oRank),])[1:1000]))
topNMvOriginalRankCompare

plot(sortednnvovmCompare$nnRank, sortednnvovmCompare$oRank)
styleplot(sortednnvovmCompare$nnRank, sortednnvovmCompare$oRank, "NewNew vs Original Rank")
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", topNNvOriginalRankCompare))

plot(sortednnvovmCompare$mRank, sortednnvovmCompare$oRank)
styleplot(sortednnvovmCompare$mRank, sortednnvovmCompare$oRank, "NewOld vs Original Rank")
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", topNMvOriginalRankCompare))

#compare N values: 

hist(sortednnvovmCompare$nnN)
hist(sortednnvovmCompare$oN)
hist(sortednnvovmCompare$mN)


#Compare ns specifically on rows in new but not others
which(is.na(sortednnvovmCompare$oP))
which(!is.na(sortednnvovmCompare$nnP))

length(which(is.na(sortednnvovmCompare$oP)))
length(which(is.na(sortednnvovmCompare$nnP)))

sortednnvovmCompare = sortednnvovmCompare[order(sortednnvovmCompare$normRank),]

oPNAs = (which(is.na(sortednnvovmCompare$oP)))
nnPNAs = (which(is.na(sortednnvovmCompare$nnP)))
length(which(!oPNAs %in% nnPNAs))
length(nnPNAs)

uniqueNAIndexes = which(!oPNAs %in% nnPNAs)
uniqueNAs = oPNAs[uniqueNAIndexes]
length(uniqueNAs)

#Now, generate correlations with min.sp =1 to find n in the other setups. 
NewMidMinsp1Correlation = correlateWithBinaryPhenotype(newCVHRER, pathsObject, min.sp =1)

NewMidMinsp1Correlation[uniqueNAs,]
names(NewMidMinsp1Correlation) = c("", "mMinSp1N")

sortednnvovmCompare = cbind(sortednnvovmCompare, NewMidMinsp1Correlation[,2])
names(sortednnvovmCompare)[17] = "mSp1N"

OriginalMinsp1Correlation = correlateWithBinaryPhenotype(oldRERValues, AttemptedReamkeOfOldPaths, min.sp =1)
sortednnvovmCompare = cbind(sortednnvovmCompare, OriginalMinsp1Correlation[,2])
names(sortednnvovmCompare)[18] = "oSp1N"


t(sortednnvovmCompare[uniqueNAs,c(2,17,18)])
hist(sortednnvovmCompare$nnN[uniqueNAs])
hist(sortednnvovmCompare$oSp1N[uniqueNAs])
hist(sortednnvovmCompare$mSp1N[uniqueNAs])

sortednnvovmCompare[uniqueNAs,2] -sortednnvovmCompare[uniqueNAs,17]


saveRDS(sortednnvovmCompare, "Results/ComparisonOfCarnvHerbsCorrelationFiles.rds")
write.csv(sortednnvovmCompare, "Results/ComparisonOfCarnvHerbsCorrelationFiles.csv" )

rownames(which(is.na(sortednnvovmCompare$oP)) %in% which(!is.na(sortednnvovmCompare$nnP)))


styleplot = function(x, y, title = NULL){
  abline(fit <- lm(x ~ y), col='red')
  legend("topleft", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)))
  title(main = title)
}

{all.equal(nnvoCompareNamed[,8], oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)
  all.equal(nnvoCompareNamed[,8], nnvoCompareNamed$op.adj)
  plot(nnvoCompareNamed[,8], nnvoCompareNamed$op.adj)
  plot(nnvoCompareNamed$op.adj, oldCVHcorrelateWithBinaryPhenotypeCollumns$p.adj)
  plot(nnvoCompareNamed$op.adj, NewNewVsOldCorrelCompare$op.adj)}
library(RERconverge)

nexusTree= read.nexus("Data/UphamDNAOnlyNodeBasedTree.tre")

write.tree(nexusTree, "Data/UphamDNAOnlyNodeBasedTree.nwk")

testTree =read.tree("Data/UphamDNAOnlyNodeBasedTree.nwk")

unroot(testTree)
plotTree(testTree)
testTree$tip.label[1]

dataSet$DebugNewMatchingName[i] = sub('^([^_]+_[^_]+).*', '\\1', dataSet$DebugNewMatchingName[i]) #remove anything  after a second underscore


uphamTips = data.frame(testTree$tip.label)
uphamTips$scientific = sub('^([^_]+_[^_]+).*', '\\1', uphamTips$testTree.tip.label)
names(uphamTips)[1] = "uphamName"
write.csv(uphamTips, "Results/UphmanNameConversion.csv")


nexusTree= read.nexus("Data/FoleyTree.nex")

obj<-scan(file="Data/FoleyTree.nex",n=1,skip=2,what="character")
tree<-read.tree(text=obj)

plotTree(tree)

foleyTips = tree$tip.label
write.csv(foleyTips, "Results/foleyTipList.csv")

source("src/reu/ZoonomTreeNameToCommon.R")

ZoonomTreeNameToCommon(tree, manualAnnotLocation = "Results/MergedData.csv", tipCol = "foleyTip")

convertTree = ZoonomTreeNameToCommon(tree, manualAnnotLocation = "Results/MergedData.csv", tipCol = "foleyTip", commonCol = "Zoonomia")
commonTree = ZoonomTreeNameToCommon(tree, manualAnnotLocation = "Results/MergedData.csv", tipCol = "foleyTip")



convertTree$tip.label


mergedData = read.csv("Results/MergedData.csv")



zonomTipsMissing = mergedData$Zoonomia[which(is.na(mergedData$foleyTip))]

zonomTipsMissing = zonomTipsMissing[which(!is.na(zonomTipsMissing))]

missingCOmmons = mergedData$CommonName[which(mergedData$Zoonomia %in% zonomTipsMissing)]

plotTreeHighlightBranches2(commonTree, hlspecies = missingCOmmons, hlcols = "blue")

missingSpecies = mergedData$ScientificName[which(is.na(mergedData$foleyTip))]

unique(mergedData$Scientific_Binomial)
length(mergedData$Scientific_Binomial)
mergedData$Scientific_Binomial[which(duplicated(mergedData$Scientific_Binomial))]

togaTree = read.tree("Data/ZoonomiaTOGATree.txt")
plotTreeHighlightBranches(togaTree, hlspecies = "REFRENCE")

togaTips = togaTree$tip.label

which(mergedData$TogaTreeName %in% togaTips)

ZoonomiaNames = mergedData$Zoonomia
ZoonomiaTrim = apply(matrix(ZoonomiaNames, nrow=1), 2, function(x) substr(x,4,nchar(x)))

ZoonomiaTrim[which(!ZoonomiaTrim %in% togaTips)]

mergedData = add_column(mergedData, TogaTreeName = ZoonomiaTrim, .before = "Divider")

mergedData$TogaTreeName[which(!mergedData$TogaTreeName %in% togaTips)]

togaTips

newTogaTree = togaTree
newTogaTree$tip.label = paste0("vs_", togaTree$tip.label)

which(newTogaTree$tip.label == "vs_hg38")
newTogaTree$tip.label[129] = "REFRENCE"

write.tree(newTogaTree, "Output/TogaTree.nwk")

test = read.tree("Output/TogaTree.nwk")

is.binary(test)

?fastAnc

?char2TreeCategorical

which(!is.na(mergedData$Meyer.Lab.Classification.Clean))
phenotypedMergedData = mergedData[which(!is.na(mergedData$Meyer.Lab.Classification.Clean)),]

unique(phenotypedMergedData$Meyer.Lab.Classification.Clean)
args = c('r=TrueCategoricalRefrenceTree', 'm=data/zoonomiaAllMammalsTrees.txt', 'd=Data/mergedData.csv', 'a=Meyer.Lab.Classification.Clean', 
         'c=c("Carnivore", "Omnivore", "Herbivore", "Insectivore", "Piscivore", "Generalist", "Planktivore")', 
         'u=list(c("Generalist", "Omnivore"), c("Omnivore-IH", "Omnivore"), c("Omnivore", "_Omnivore"))', 
         'o=list(c("Piscivore", "Carnivore"), c("Planktivore", "Carnivore"), c("Insectivore", "Carnivore"), c("Piscivore", "Insectivore"))',
         'v=T', 't=ER', 'n=Zoonomia')

substitutions = list(c("Generalist", "Omnivore"), c("Omnivore-IH", "Omnivore"), c("Omnivore", "_Omnivore"))
mergeOnlys = list(c("Piscivore", "Carnivore"), c("Planktivore", "Carnivore"), c("Insectivore", "Carnivore"), c("Piscivore", "Insectivore"))


mergedData = read.csv("Results/MergedData.csv")
manualAnnots = mergedData
annotColumn = "Meyer.Lab.Classification.Compressed"


if(!is.null(substitutions)){                                                    #Consider species with multiple combined categories as the merged category
  for( i in 1:length(substitutions)){                                           #Eg if [X] is replaced with [Y], [X/Y] becomes [Y]
    substitutePhenotypes = substitutions[[i]]
    message(paste("Combining", substitutePhenotypes[1], "/", substitutePhenotypes[2]))
    entriesWithPhen1 = grep(substitutePhenotypes[1], manualAnnots[[annotColumn]])
    entriesWithPhen2 = grep(substitutePhenotypes[2], manualAnnots[[annotColumn]])
    combineEntries = which(entriesWithPhen1 %in% entriesWithPhen2)
    combineIndexes = entriesWithPhen1[combineEntries]
    manualAnnots[[annotColumn]][combineIndexes] = substitutePhenotypes[2]
  }
}

if(!is.null(mergeOnlys)){                                                    #Consider species with multiple combined categories as the merged category
  for( i in 1:length(mergeOnlys)){                                           #Eg if [X] is replaced with [Y], [X/Y] becomes [Y]
    substitutePhenotypes = mergeOnlys[[i]]
    message(paste("Merging Hybrids of", substitutePhenotypes[1], "/", substitutePhenotypes[2], "to", substitutePhenotypes[2]))
    entriesWithPhen1 = grep(substitutePhenotypes[1], manualAnnots[[annotColumn]])
    entriesWithPhen2 = grep(substitutePhenotypes[2], manualAnnots[[annotColumn]])
    combineEntries = which(entriesWithPhen1 %in% entriesWithPhen2)
    combineIndexes = entriesWithPhen1[combineEntries]
    manualAnnots[[annotColumn]][combineIndexes] = substitutePhenotypes[2]
  }
}

if(!is.null(substitutions)){
  for( i in 1:length(substitutions)){
    substitutePhenotypes = substitutions[[i]]
    message(paste("replacing", substitutePhenotypes[1], "with", substitutePhenotypes[2]))
    phenotypeVector = gsub(substitutePhenotypes[1], substitutePhenotypes[2], phenotypeVector)
  }
}


manualAnnots$Meyer.Lab.Classification.Compressed

mergedData = manualAnnots

mergedData$Meyer.Lab.Classification.Compressed

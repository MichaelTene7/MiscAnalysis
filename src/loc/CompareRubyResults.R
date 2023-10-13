library(RERconverge)
source("Src/Reu/ZoonomTreeNameToCommon.R")
source("Src/Reu/ZonomNameConvertVector.R")
ZoonomTreeNameToCommon = function(tree, plot = T, isForegroundTree = T, manualAnnotLocation = "Data/manualAnnotationsSheet.csv", hlcol = "blue", bgcol = "black", fontSize = 0.8){
  
  manualAnnot = read.csv(manualAnnotLocation) 
  inputTree = tree
  tipNames = inputTree$tip.label
  for(i in 1:length(tipNames)){                                                    #for each name: 
    currentName = tipNames[i]                                                      #use the 'i'th name in the list
    currentRow = manualAnnot[manualAnnot$Assembly_Name %in% currentName, ]             #find a row with the zonom name that matches the current name 
    currentSize = dim(currentRow)                                               #Part of "does row exist check": get the dimensions of the currentRow dataframe
    obsNumber = currentSize[1]                                                  #set "size" equal to the number of observations in 'currentRow'; which is the number of matches to the current name. If none exist it will be 0, if more than one it will be greater than 1. 
    if(obsNumber == 1){                                                         #if only one match exists:
      currentName = currentRow$Common.Name.or.Group                             #get the name from that row
    }else{
      currentName = tipNames[i]                                                    #Otherwise, keep the name the same
    }
    tipNames[i] = currentName                                                      #update the main name list with the name chose
  }
  inputTree$tip.label = tipNames
  
  if(plot){
    if(isForegroundTree){
      readableTree = inputTree
      readableTree$edge.length[readableTree$edge.length == 0] = 1
      plotTreeHighlightBranches2(readableTree, hlspecies = which(inputTree$edge.length == 1), hlcols = hlcol, bgcol = bgcol, fontSize = fontSize)
    }else{
      plotTree(inputTree)
    }
  }
  return(inputTree)
}
ZonomNameConvertVectorCommon = function(namesVector){
  names = namesVector                                                    #make a vector of the names
  manualAnnot = read.csv("Data/manualAnnotationsSheet.csv")                     #improt manual annots file
  for(i in 1:length(names)){                                                    #for each name: 
    currentName = names[i]                                                      #use the 'i'th name in the list
    currentRow = manualAnnot[manualAnnot$Assembly_Name %in% currentName, ]             #find a row with the zonom name that matches the current name 
    currentSize = dim(currentRow)                                               #Part of "does row exist check": get the dimensions of the currentRow dataframe
    obsNumber = currentSize[1]                                                  #set "size" equal to the number of observations in 'currentRow'; which is the number of matches to the current name. If none exist it will be 0, if more than one it will be greater than 1. 
    if(obsNumber == 1){                                                         #if only one match exists:
      currentName = currentRow$Common.Name.or.Group                             #get the name from that row
    }else{
      currentName = names[i]                                                    #Otherwise, keep the name the same
    }
    names[i] = currentName                                                      #update the main name list with the name chose
  }
  #colnames(nMatrix) = names                                                     #update the matrix with the new names. 
  return(names)
}

# ---------------- #
rubyMainTree = readRDS("Data/RubyData/mam120aa_trees.rds")
rubyPhens = readRDS("Data/RubyData/binaryPhenotypes.rds")
rubyPhensFinal = readRDS("Data/RubyData/dietPhensFinal.rds")

length(rubyPhensFinal)
myCategoricalPhenotypes = readRDS("Data/CategoricalPermulationsTimingHillerPhenotypes.rds")
hillerConversionTable = readRDS("Data/HillerZoonomPhenotypeTable.rds")
zoonmiaToHillerCodes = readRDS("Data/zoonomiaToHillerCodesTable.rds")

mineNotRuby = myCategoricalPhenotypes[which(!names(myCategoricalPhenotypes) %in% names(rubyPhensFinal))]


RubyNotMine = rubyPhensFinal[which(!names(rubyPhensFinal) %in% names(myCategoricalPhenotypes))]
length(RubyNotMine)

hillerNotRuby = zoonmiaToHillerCodes[which(!zoonmiaToHillerCodes$TipLabel %in% names(rubyPhensFinal)),]
grep("HLmus", names(rubyPhensFinal))


hillerConversionTable$common[which(hillerConversionTable$Hiller %in% names(mineNotRuby))]
hillerConversionTable$Zoonomia[which(hillerConversionTable$Hiller %in% names(mineNotRuby))]
hillerConversionTable$Hiller[which(hillerConversionTable$Hiller %in% names(mineNotRuby))]

zoonmiaToHillerCodes$ScientificName[which(zoonmiaToHillerCodes$TipLabel %in% names(RubyNotMine))]
hillerConversionTable$common[which(hillerConversionTable$Hiller %in% names(RubyNotMine))]
hillerConversionTable$Zoonomia[which(hillerConversionTable$Hiller %in% names(RubyNotMine))]
hillerConversionTable$Hiller[which(hillerConversionTable$Hiller %in% names(RubyNotMine))]

names(myCategoricalPhenotypes)[58]

names(rubyPhensFinal)[59]
















char2TreeCategorical(rubyPhensFinal, rubyMainTree, model = "ARD", plot = T)

rubyPhensFinal = gsub("omnivore", "_Omnivore", rubyPhensFinal)
rubyPhensFinal = gsub("carnivore", "Carnivore", rubyPhensFinal)
rubyPhensFinal = gsub("herbivore", "Herbivore", rubyPhensFinal)
saveRDS(rubyPhensFinal, "Data/RubyData/RubyRegenPhenotypeVector.rds")

commonMainTrees = rubyMainTree
commonMainTrees$masterTree = ZoonomTreeNameToCommon(commonMainTrees$masterTree)
commonPhenotypeVector = rubyPhensFinal
names(commonPhenotypeVector) = ZonomNameConvertVectorCommon(names(commonPhenotypeVector))

char2TreeCategorical(commonPhenotypeVector, commonMainTrees, model = "ARD", plot = T)

pdf(height = length(commonPhenotypeVector)/18)                     #make a pdf to store the plot, sized based on tree size
char2TreeCategorical(commonPhenotypeVector, commonMainTrees, model = "ARD", plot = T)
char2TreeCategorical(commonPhenotypeVector, commonMainTrees, model = "ER", plot = T)
dev.off()   

rubyAnnots = readRDS("Data/RubyData/annots.rds")
rubyCorrelations = readRDS("Data/RubyData/CategoricalDietCorsFinal.rds")

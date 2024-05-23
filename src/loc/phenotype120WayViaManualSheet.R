library(RERconverge)
source("Src/Reu/ZoonomTreeNameToCommon.R")
source("Src/Reu/ZonomNameConvertVector.R")
ZoonomTreeNameToCommon = function(tree, plot = T, isForegroundTree = T, manualAnnotLocation = "Data/manualAnnotationsSheet.csv", hlcol = "blue", bgcol = "black", fontSize = 0.8){
  
  manualAnnot = read.csv(manualAnnotLocation) 
  inputTree = tree
  tipNames = inputTree$tip.label
  for(i in 1:length(tipNames)){                                                    #for each name: 
    currentName = tipNames[i]                                                      #use the 'i'th name in the list
    currentRow = manualAnnot[manualAnnot$Genome %in% currentName, ]             #find a row with the zonom name that matches the current name 
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
      if(obsNumber == 0){
        currentNameTrim = substring(currentName, 1, last=nchar(currentName)-1)
        currentRow = manualAnnot[substring(manualAnnot$Assembly_Name, 1, last=nchar(manualAnnot$Assembly_Name)-1) %in% currentNameTrim, ]             #find a row with the zonom name that matches the current name 
        currentSize = dim(currentRow)                                               #Part of "does row exist check": get the dimensions of the currentRow dataframe
        obsNumber = currentSize[1]                                                  #set "size" equal to the number of observations in 'currentRow'; which is the number of matches to the current name. If none exist it will be 0, if more than one it will be greater than 1. 
        if(obsNumber == 1){                                                         #if only one match exists:
          currentName = currentRow$Common.Name.or.Group                             #get the name from that row
        }else{
          currentName = names[i] 
        }
      }else{
        currentName = names[i]                                                    #Otherwise, keep the name the same  
      }  
    }
    names[i] = currentName                                                      #update the main name list with the name chose
  }
  #colnames(nMatrix) = names                                                     #update the matrix with the new names. 
  return(names)
}

# ---------------- #
rubyMainTree = readRDS("Data/RubyData/mam120aa_trees.rds")

commonMainTrees = rubyMainTree
commonMainTrees$masterTree = ZoonomTreeNameToCommon(commonMainTrees$masterTree)
commonMainTrees$masterTree$tip.label

ZonomNameConvertVectorCommon(commonMainTrees$masterTree$tip.label)
rubyMainTree$masterTree$tip.label


codeTable = readRDS("Data/zoonomiaToHillerCodesTable.rds")
i=1
inputTree = rubyMainTree$masterTree
HillerTreeNameToCommon = function(tree, plot = T, isForegroundTree = T, manualAnnotLocation = "Data/manualAnnotationsSheet.csv", hlcol = "blue", bgcol = "black", fontSize = 0.8){
  manualAnnot = read.csv(manualAnnotLocation) 
  inputTree = tree
  tipNames = inputTree$tip.label
  for(i in 1:length(tipNames)){                                                    #for each name: 
    hillerName = tipNames[i]                                                      #use the 'i'th name in the list
    zoonomName = codeTable$zoonomiaCode[which(codeTable$TipLabel %in% hillerName)]
    currentName = zoonomName
    currentRow = manualAnnot[manualAnnot$FaName %in% currentName, ]             #find a row with the zonom name that matches the current name 
    currentSize = dim(currentRow)                                               #Part of "does row exist check": get the dimensions of the currentRow dataframe
    obsNumber = currentSize[1]                                                  #set "size" equal to the number of observations in 'currentRow'; which is the number of matches to the current name. If none exist it will be 0, if more than one it will be greater than 1. 
    
    if(obsNumber == 1){                                                         #if only one match exists:
      currentName = currentRow$Common.Name.or.Group                             #get the name from that row
    }else{
      currentRow = manualAnnot[manualAnnot$FaName %in% currentName, ]             #find a row with the zonom name that matches the current name 
      currentSize = dim(currentRow)                                               #Part of "does row exist check": get the dimensions of the currentRow dataframe
      obsNumber = currentSize[1]                                                  #set "size" equal to the number of observations in 'currentRow'; which is the number of matches to the current name. If none exist it will be 0, if more than one it will be greater than 1. 
      if(obsNumber == 1){
        currentName = currentRow$Common.Name.or.Group 
      }else{
        
      }
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

HillerTreeNameToZoonom = function(tree, plot = T, isForegroundTree = T, manualAnnotLocation = "Data/manualAnnotationsSheet.csv", hlcol = "blue", bgcol = "black", fontSize = 0.8){
  manualAnnot = read.csv(manualAnnotLocation) 
  inputTree = tree
  tipNames = inputTree$tip.label
  for(i in 1:length(tipNames)){                                                    #for each name: 
    hillerName = tipNames[i]                                                      #use the 'i'th name in the list
    zoonomName = codeTable$zoonomiaCode[which(codeTable$TipLabel %in% hillerName)]
    currentName = zoonomName
    currentRow = manualAnnot[manualAnnot$FaName %in% currentName, ]             #find a row with the zonom name that matches the current name 
    currentSize = dim(currentRow)                                               #Part of "does row exist check": get the dimensions of the currentRow dataframe
    obsNumber = currentSize[1]                                                  #set "size" equal to the number of observations in 'currentRow'; which is the number of matches to the current name. If none exist it will be 0, if more than one it will be greater than 1. 
    
    if(obsNumber == 1){                                                         #if only one match exists:
      currentName = currentRow$Common.Name.or.Group                             #get the name from that row
    }else{
      currentRow = manualAnnot[manualAnnot$FaName %in% currentName, ]             #find a row with the zonom name that matches the current name 
      currentSize = dim(currentRow)                                               #Part of "does row exist check": get the dimensions of the currentRow dataframe
      obsNumber = currentSize[1]                                                  #set "size" equal to the number of observations in 'currentRow'; which is the number of matches to the current name. If none exist it will be 0, if more than one it will be greater than 1. 
      if(obsNumber == 1){
        currentName = currentRow$Common.Name.or.Group 
      }else{
        
      }
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

commonMainTrees = rubyMainTree
commonMainTrees$masterTree = HillerTreeNameToCommon(commonMainTrees$masterTree)
commonMainTrees$masterTree = ZoonomTreeNameToCommon(commonMainTrees$masterTree)
commonMainTrees$masterTree$tip.label






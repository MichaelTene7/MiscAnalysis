library(tidyr)
library(dplyr)
library(tibble)

# ---- Code to add name values to the combinedData ---- 


# -- sorting data function --

#This function takes the input  data, and then reorders it so that the rows match the species in the main data (including missing species).
#It matches based on a cleaned version of the specified scientific name column. 
#If addNewSpecies is TRUE, it will add species in the new data not in the merged data to the end of the data. Otherwise, they will not be used.
orderNewData = function(dataSet, scientificNameColumn, mergedData = combinedData, addNewSpecies = F, commonNameColumn = NA){
  #Make a column with the scientific name for matching
  inScientificNameColumn = which(names(dataSet) == scientificNameColumn)
  dataSet$DebugNewMatchingName = dataSet[,inScientificNameColumn]
  
  #Clean up that name into the same formatting 
  for(i in 1:length(dataSet$DebugNewMatchingName)){
  dataSet$DebugNewMatchingName[i] = gsub(" ", "_", dataSet$DebugNewMatchingName[i]) #replace spaces with underscores 
  dataSet$DebugNewMatchingName[i] = sub('^([^_]+_[^_]+).*', '\\1', dataSet$DebugNewMatchingName[i]) #remove anything  after a second underscore
  dataSet$DebugNewMatchingName[i] = tolower(dataSet$DebugNewMatchingName[i])
  }
  dataSet$DebugMatchColumn = match(dataSet$DebugNewMatchingName, mergedData$Scientific_Binomial)
  dataSet = dataSet %>% select(DebugNewMatchingName, everything())
  
  newSpecies = dataSet$DebugNewMatchingName[is.na(dataSet$DebugMatchColumn)]
  message("Species in new Data not in merged data:")
  for(i in 1:length(newSpecies)){
    message(newSpecies[i])
  }
  newData = as.data.frame(matrix(NA, ncol = ncol(dataSet), nrow = nrow(mergedData)))
  names(newData) = names(dataSet)
  newData$DebugNewMatchingName = mergedData$Scientific_Binomial
  
  for(i in 1:nrow(newData)){
    if(i %in% dataSet$DebugMatchColumn){
      matchedRow = which(dataSet$DebugMatchColumn == i)
      newData[i,] = dataSet[matchedRow,]
    }
  }
  
  if(addNewSpecies){
    newSpeciesRows = which(is.na(dataSet$DebugMatchColumn))
    for(i in newSpeciesRows){
      newData = rbind(newData, dataSet[i,])
    }
    
    #Make a global envinroment dataset of the new species with scientific and optionally common names 
    
    newSpeciesData = dataSet[newSpeciesRows,]
    newScientificNameColumn = which(names(newSpeciesData) == scientificNameColumn)
    if(!is.na(commonNameColumn)){
      newCommonNameColumn = which(names(newSpeciesData) == commonNameColumn)
      if(length(newCommonNameColumn) == 0){stop("Specified Common Name Column not found. Make sure it is spelled correctly, and run the code again.")}
    }else{
      newCommonNameColumn = which(names(newSpeciesData) == "DebugMatchColumn") #This is a column that garuntees that all values will be NA, because if they had a match they wouldn't be new.
    }
    names(newSpeciesData)[1] = "Scientific_Binomial"
    names(newSpeciesData)[newCommonNameColumn] = "CommonName"
    names(newSpeciesData)[newScientificNameColumn] = "ScientificNameFull"
    newSpeciesDataset <<- newSpeciesData[,c(1,newCommonNameColumn,newScientificNameColumn)]
  }
  
  newData
}


# -- Function to add new species to main data --

addMainSpecies = function(speciesToAdd = newSpeciesDataset, mergedData = combinedData){
  newSpeciesDataLong = as.data.frame(matrix(NA, ncol = ncol(mergedData), nrow = nrow(speciesToAdd)))
  newSpeciesDataLong[,1] = speciesToAdd[,1]
  newSpeciesDataLong[,2] = speciesToAdd[,2]
  newSpeciesDataLong[,3] = speciesToAdd[,3]
  names(newSpeciesDataLong) = names(mergedData)
  
  mergedData = rbind(mergedData, newSpeciesDataLong)
  
  mergedData
}

# -- Adding column function --
#This function adds a column with the correct order into the merged data.
#It supports renaming the column, and supports adding "name" columns before the divider. 
addColumn = function(dataSet, column, columnRename = NA, mergedData = combinedData, nameColumn = F){
  inColumnIndex = which(names(dataSet) == column)
  columnToAdd = dataSet[[inColumnIndex]]
  inColumnName = names(dataSet)[inColumnIndex]
  if(!is.na(columnRename)){
    outColumnName = columnRename
  }else{
    outColumnName = inColumnName
  }
  
  if(nameColumn){
    mergedData = add_column(mergedData, debugInsertedColumn = columnToAdd, .before = "Divider")
  }else{
    mergedData = add_column(mergedData, debugInsertedColumn = columnToAdd)
  }
  
  outColumnIndex = which(names(mergedData) == "debugInsertedColumn")
  names(mergedData)[outColumnIndex] = outColumnName
  
  mergedData
}


# -- Code to make the starter sheet -- 
#combinedData = manualAnnots[,c(4,3,4)]
#names(combinedData) = c("Scientific_Binomial", "CommonName", "ScientificNameFull")
#combinedData$Scientific_Binomial = gsub(" ", "_", combinedData$Scientific_Binomial)
#combinedData$Divider = rep("-")

#myVec = "This_is_a_test_vector"
##This line of code replaces any characters after "<not '_'> + _ + <not '_'>" with blank space
#sub('^([^_]+_[^_]+).*', '\\1', myVec)

#combinedData$Scientific_Binomial = sub('^([^_]+_[^_]+).*', '\\1', combinedData$Scientific_Binomial)
#combinedData$Scientific_Binomial = tolower(combinedData$Scientific_Binomial)

#which(combinedData$Scientific_Binomial== "")
#combinedData = combinedData[-which(combinedData$Scientific_Binomial== ""), ]

##write.csv(combinedData, "Results/MergedDataBase.csv", row.names = F)

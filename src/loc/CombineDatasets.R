#This is a script designed to combine multiple datasets. 



# ----- Debug code ----
eltonTraits = read.table("Data/MamFuncDat.txt", sep = "\t", header = T)
newDataset = eltonTraits

dataSet = eltonTraits
scientificNameColumn = newDataScientificNameColumn
# ----- 


# --- Setup ---

#Defaults 
source("Src/Loc/CombineDatasetsFunctions.R")
combinedDataInLocation = "Results/MergedData.csv"; combinedDataOutLocation = "Results/MergedData.csv";newDataLocation = "Data/HillerZoonomiaPhenotypeTable.csv";newDataScientificNameColumn = "scientific";newDataCommonNameColumn = "Common.Name.or.Group";addNewSpeciesValue = T ;attachAllColumns = T ;nameColumns = NA;manualAddColumns = NA;manualColumnRenames = NA;manualNameColumnRenames = NA; manualColumnsToIgnore = NA


####
# - EDIT ME: Data Merging Arguments 
#Eltontraits
combinedDataInLocation = "Results/MergedData.csv"
combinedDataOutLocation = "Results/MergedData.csv"
newDataLocation = "Data/do this manually it's not a csv"
newDataScientificNameColumn = "Scientific"
newDataCommonNameColumn = NA
addNewSpeciesValue = F 
attachAllColumns = T 
nameColumns = c("MSW3_ID", "MSWFamilyLatin")
manualAddColumns = NA
manualColumnRenames = NA
manualNameColumnRenames = NA
manualColumnsToIgnore = NA

#Manual annotations file
newDataLocation = "Data/manualAnnotationsSheet.csv"
newDataScientificNameColumn = "Species.Name"
newDataCommonNameColumn = "Common.Name.or.Group"
addNewSpeciesValue = F 
attachAllColumns = T 
nameColumns = c("Genome", "Assembly_Name", "FaName")
manualAddColumns = NA
manualColumnRenames = NA
manualNameColumnRenames = NA
manualColumnsToIgnore =c("Tip_Label..Red.is.included.in.CMU.enhancer.dataset..but.missing.alignment.", "X52") 

#hillerConversionTable
newDataLocation = "Data/HillerZoonomiaPhenotypeTable.csv"
newDataScientificNameColumn = "scientific"
newDataCommonNameColumn = "Common.Name.or.Group"
addNewSpeciesValue = T 
attachAllColumns = T 
nameColumns = c("Zoonomia", "FaName")
manualAddColumns = NA
manualColumnRenames = NA
manualNameColumnRenames = NA

#uphamName
com
newDataLocation = "Results/UphamNameConversion.csv"
newDataScientificNameColumn = "scientific"
newDataCommonNameColumn = NA
addNewSpeciesValue = F
attachAllColumns = F 
nameColumns = c("uphamName")
manualAddColumns = NA
manualColumnRenames = NA
manualNameColumnRenames = NA

#foleyName
newDataLocation = "Results/foleyTipList.csv"
newDataScientificNameColumn = "Scientific"
newDataCommonNameColumn = NA
addNewSpeciesValue = F
attachAllColumns = F 
nameColumns = c("foleyTip")
manualAddColumns = NA
manualColumnRenames = NA
manualNameColumnRenames = NA





# ---- Main code ---- 

{
# -- Set up the two dataframes -- 
combinedData = read.csv(combinedDataInLocation)

newDataset = read.csv(newDataLocation)

orderedNewData = orderNewData(newDataset, newDataScientificNameColumn, mergedData = combinedData, addNewSpecies = addNewSpeciesValue, commonNameColumn = newDataCommonNameColumn)

if(addNewSpeciesValue){
  combinedData = addMainSpecies(speciesToAdd = newSpeciesDataset)
}

# --- attach the columns --- 

if(!attachAllColumns){
  #Data columns
  if(!all(is.na(manualColumnRenames))){
    lengthMatchCheck = length(manualAddColumns) == length(manualColumnRenames)
    if(!lengthMatchCheck){
      stop("The list of main columns to add and renames are not the same length. Makes sure they are, and re-run the script. If you do not want to rename a column, use the columns original name in that postion.")
    }
    usingMainRenames = T
  }else{usingMainRenames = F}
  columnsToAdd = manualAddColumns
  
  if(usingMainRenames){
    for(i in 1:length(columnsToAdd)){
      combinedData = addColumn(orderedNewData, columnsToAdd[i], columnRename = manualColumnRenames[i])
    }
  }else{
    if(!is.na(columnsToAdd)){
      for(i in 1:length(columnsToAdd)){
        combinedData = addColumn(orderedNewData, columnsToAdd[i])
      }
    }
  }
  #Name columns 
  if(!all(is.na(nameColumns))){
    if(!all(is.na(manualNameColumnRenames))){
      lengthMatchCheck = length(nameColumns) == length(manualNameColumnRenames)
      if(!lengthMatchCheck){
        stop("The list of name columns to add and renames are not the same length. Makes sure they are, and re-run the script. If you do not want to rename a column, use the columns original name in that postion.")
      }
      usingNameRenames = T
    }
    
    if(usingNameRenames){
      for(i in 1:length(nameColumns)){
        combinedData = addColumn(orderedNewData, nameColumns[i], columnRename = manualNameColumnRenames[i], nameColumn = T)
      }
    }else{
      for(i in 1:length(nameColumns)){
        combinedData = addColumn(orderedNewData, nameColumns[i], nameColumn = T)
      }
    }
  }
  
}else{
  columnsToAdd = names(orderedNewData)
  
  #Making a list of collumn which have already been proccessed by or created by the script 
  collumsAlwaysIgnored = c("DebugNewMatchingName", "X", "DebugMatchColumn")
  collumsAlwaysIgnored = append(collumsAlwaysIgnored, newDataScientificNameColumn)
  collumsAlwaysIgnored = append(collumsAlwaysIgnored, newDataCommonNameColumn)
  collumsAlwaysIgnored = append(collumsAlwaysIgnored, manualColumnsToIgnore)
  
  #Remove those columns from the set to be added 
  columnsToAdd = columnsToAdd[-which(columnsToAdd %in% collumsAlwaysIgnored)]
  
  #Remove any columns specified as name columns, as those will be handled later 
  columnsToAdd = columnsToAdd[-which(columnsToAdd %in% nameColumns)]
  
  #Add non-name columns
  for(i in columnsToAdd){
    combinedData = addColumn(orderedNewData, i)
  }
  
  #Add name columns
  if(!all(is.na(manualNameColumnRenames))){
    lengthMatchCheck = length(nameColumns) == length(manualNameColumnRenames)
    if(!lengthMatchCheck){
      stop("The list of name columns to add and renames are not the same length. Makes sure they are, and re-run the script. If you do not want to rename a column, use the columns original name in that postion.")
    }
    usingNameRenames = T
  }else{usingNameRenames = F}
  
  if(usingNameRenames){
    for(i in 1:length(nameColumns)){
      combinedData = addColumn(orderedNewData, nameColumns[i], columnRename = manualNameColumnRenames[i], nameColumn = T)
    }
  }else{
    for(i in 1:length(nameColumns)){
      combinedData = addColumn(orderedNewData, nameColumns[i], nameColumn = T)
    }
  }
}


}
write.csv(combinedData, combinedDataOutLocation, row.names = F)




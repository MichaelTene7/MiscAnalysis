library(tibble)

# -----

styledplot = function(x, y, title = NULL){
  plot(x,y)
  abline(fit <- lm(x ~ y), col='red')
  legend("topleft", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)), text.col = 'blue')
  title(main = title)
  
}
styleplot = function(x, y, title = NULL){
  abline(fit <- lm(x ~ y), col='red')
  legend("topleft", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)), text.col = 'blue')
  title(main = title)
  
}
#-----

# --- Insectivory ----

#-- get data --
#Load in Insectivory missing Species
insectivoryMissingSpeciesPValues = readRDS("Data/pValues/insectivoryTypoedListPermulationPValues.rds")
#insectivoryMissingSpeciesPValuesSorted = insectivoryMissingSpeciesPValues[order(insectivoryMissingSpeciesPValues)]
insectivoryMissingSpeciesPValues = insectivoryMissingSpeciesPValues[1:16208] #Remove duplicate BCAR1

#Load in Insectivory full species 
insectivoryAllSpeciesPValues = readRDS("Data/pValues/insectivoryFullListPermulationPValues.rds")
#insectivoryAllSpeciesPValuesSorted = insectivoryAllSpeciesPValues[order(insectivoryAllSpeciesPValues)]
insectivoryAllSpeciesPValues = insectivoryAllSpeciesPValues[1:16208] #Remove duplicate BCAR1

#Organize into Dataframe
insectivoryDF = data.frame(insectivoryMissingSpeciesPValues)
colnames(insectivoryDF)[1] = "missingSpeciesPValue"
rownames(insectivoryDF) = names(insectivoryMissingSpeciesPValues)
insectivoryDF$defaultPosition = 1:nrow(insectivoryDF)
insectivoryDF = insectivoryDF[,c(2,1)]
insectivoryDF$allSpeciesPValue = insectivoryAllSpeciesPValues

insectivoryDF = insectivoryDF[order(insectivoryDF$missingSpeciesPValue),]
insectivoryDF$missingSpeciesRank = 1:nrow(insectivoryDF)

insectivoryDF = insectivoryDF[order(insectivoryDF$allSpeciesPValue),]
insectivoryDF$allSpeciesRank = 1:nrow(insectivoryDF)

insectivoryDF = insectivoryDF[order(insectivoryDF$defaultPosition),]


# -- Compare data -- 
#compare top genes
insectivoryTop1000GenesInCommon = length(which(rownames(insectivoryDF[order(insectivoryDF$missingSpeciesRank),])[1:1000] %in% rownames(insectivoryDF[order(insectivoryDF$allSpeciesRank),])[1:1000]))
insectivoryTop1000GenesInCommon
#Plot position in first dataset against position in other dataset 
plot(insectivoryDF$missingSpeciesRank, insectivoryDF$allSpeciesRank)
styleplot(insectivoryDF$missingSpeciesRank, insectivoryDF$allSpeciesRank, "Insectivory Full vs Missing Species")
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", insectivoryTop1000GenesInCommon), text.col = 'blue4')


styledplot(insectivoryDF$missingSpeciesRank, insectivoryDF$defaultPosition, "Insectivory Full vs Missing Species")



# -------- CarnvHerbs -------

#load in the pValues
carnvHerbsLauraOnlyTreePValue = readRDS("Data/pValues/carnvHerbsLauraOnlyTreePermulationPValues.rds")
carnvHerbsNonLauraTreePValue = readRDS("Data/pValues/carnvHerbsNonLauraTreePermulationPValues.rds")
carnvHerbsSisterListPValue = readRDS("Data/pValues/carnvHerbsSisterlistPermulationPValues.rds")
carnvHerbOldCorrelationNewPermulationPValue = readRDS("Data/pValues/carnvHerbsOldStatsNewAnalysisPermulationPValues.rds")

carnvHerbsPreviousAnalysisData = read.csv("Data/pValues/CorrelationFishCarnHerbLaurasiatheria_wminsp_wnopermp_wcount_waddlperms0907.csv")
carnvHerbsPreviousAnalysisPValue = carnvHerbsPreviousAnalysisData$permp.2sided
carnvHerbsPreviousAnalysisPValueOneSide = carnvHerbsPreviousAnalysisData$permp.1sided
names(carnvHerbsPreviousAnalysisPValue) = carnvHerbsPreviousAnalysisData$X
names(carnvHerbsPreviousAnalysisPValueOneSide) = carnvHerbsPreviousAnalysisData$X

#remove duplicate BCAR1
carnvHerbsLauraOnlyTreePValue = carnvHerbsLauraOnlyTreePValue[1:16208] #Remove duplicate BCAR1
carnvHerbsNonLauraTreePValue = carnvHerbsNonLauraTreePValue[1:16208] #Remove duplicate BCAR1
carnvHerbsSisterListPValue = carnvHerbsSisterListPValue[1:16208] #Remove duplicate BCAR1
carnvHerbsPreviousAnalysisPValue = carnvHerbsPreviousAnalysisPValue[1:16208] #Remove duplicate BCAR1
carnvHerbsPreviousAnalysisPValueOneSide = carnvHerbsPreviousAnalysisPValueOneSide[1:16208]
carnvHerbOldCorrelationNewPermulationPValue = carnvHerbOldCorrelationNewPermulationPValue[1:16208]

#format as dataframe
carnvHerbsDF = data.frame(carnvHerbsLauraOnlyTreePValue)
colnames(carnvHerbsDF)[1] = "lauraOnlyPValue"
rownames(carnvHerbsDF) = names(carnvHerbsLauraOnlyTreePValue)
carnvHerbsDF$defaultPosition = 1:nrow(carnvHerbsDF)
carnvHerbsDF = carnvHerbsDF[,c(2,1)]

carnvHerbsDF$nonLauraPValue = carnvHerbsNonLauraTreePValue
carnvHerbsDF$sisterListPValue = carnvHerbsSisterListPValue
carnvHerbsDF$previousAnalysisPValue = carnvHerbsPreviousAnalysisPValue
carnvHerbsDF$previousAnalysisPValueOneSide = carnvHerbsPreviousAnalysisPValueOneSide
carnvHerbsDF$oldCorrelationNewPermulationPValue = carnvHerbOldCorrelationNewPermulationPValue

carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$lauraOnlyPValue),]
carnvHerbsDF$lauraOnlyRank = 1:nrow(carnvHerbsDF)
carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$nonLauraPValue),]
carnvHerbsDF$nonLauraRank = 1:nrow(carnvHerbsDF)
carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$sisterListPValue),]
carnvHerbsDF$sisterListRank = 1:nrow(carnvHerbsDF)
carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$previousAnalysisPValue),]
carnvHerbsDF$previousAnalysisRank = 1:nrow(carnvHerbsDF)
carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$previousAnalysisPValueOneSide),]
carnvHerbsDF$previousAnalysisOneSideRank = 1:nrow(carnvHerbsDF)
carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$oldCorrelationNewPermulationPValue),]
carnvHerbsDF$OldCorellationRank = 1:nrow(carnvHerbsDF)

carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$defaultPosition),]

# -- Compare data by rank -- 
{
plot(carnvHerbsDF$lauraOnlyRank, carnvHerbsDF$previousAnalysisRank)
styleplot(carnvHerbsDF$lauraOnlyRank, carnvHerbsDF$previousAnalysisRank, "CarnvHerb New vs Old Analysis Rank")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$lauraOnlyRank),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$previousAnalysisRank),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$lauraOnlyRank, carnvHerbsDF$nonLauraRank)
styleplot(carnvHerbsDF$lauraOnlyRank, carnvHerbsDF$nonLauraRank, "CarnvHerb Laura vs Non-Laura Rank")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$lauraOnlyRank),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$nonLauraRank),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$nonLauraRank, carnvHerbsDF$previousAnalysisRank)
styleplot(carnvHerbsDF$nonLauraRank, carnvHerbsDF$previousAnalysisRank, "CarnvHerb Non-Laura vs Old Analysis Rank")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$nonLauraRank),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$previousAnalysisRank),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$lauraOnlyRank, carnvHerbsDF$sisterListRank)
styleplot(carnvHerbsDF$lauraOnlyRank, carnvHerbsDF$sisterListRank, "CarnvHerb Laura vs Sisterlist Rank")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$lauraOnlyRank),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$sisterListRank),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$nonLauraRank, carnvHerbsDF$sisterListRank)
styleplot(carnvHerbsDF$nonLauraRank, carnvHerbsDF$sisterListRank, "CarnvHerb Non-Laura vs Sisterlist Rank")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$nonLauraRank),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$sisterListRank),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$nonLauraRank, carnvHerbsDF$previousAnalysisOneSideRank)
styleplot(carnvHerbsDF$nonLauraRank, carnvHerbsDF$previousAnalysisOneSideRank, "CarnvHerb Non-Laura vs Previous Analysis One Sided Rank")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$nonLauraRank),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$previousAnalysisOneSideRank),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

# --- Compare data by p Value --- 
plot(carnvHerbsDF$lauraOnlyPValue, carnvHerbsDF$previousAnalysisPValue)
styleplot(carnvHerbsDF$lauraOnlyPValue, carnvHerbsDF$previousAnalysisPValue, "CarnvHerb New vs Old Analysis pValue")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$lauraOnlyPValue),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$previousAnalysisPValue),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$lauraOnlyPValue, carnvHerbsDF$nonLauraPValue)
styleplot(carnvHerbsDF$lauraOnlyRank, carnvHerbsDF$nonLauraPValue, "CarnvHerb Laura vs Non-Laura pValue")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$lauraOnlyPValue),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$nonLauraPValue),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$nonLauraPValue, carnvHerbsDF$previousAnalysisPValue)
styleplot(carnvHerbsDF$nonLauraPValue, carnvHerbsDF$previousAnalysisPValue, "CarnvHerb Non-Laura vs Old Analysis pValue")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$nonLauraPValue),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$previousAnalysisPValue),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$lauraOnlyPValue, carnvHerbsDF$sisterListPValue)
styleplot(carnvHerbsDF$lauraOnlyPValue, carnvHerbsDF$sisterListPValue, "CarnvHerb Laura vs Sisterlist pValue")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$lauraOnlyPValue),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$sisterListPValue),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$nonLauraPValue, carnvHerbsDF$sisterListPValue)
styleplot(carnvHerbsDF$nonLauraPValue, carnvHerbsDF$sisterListPValue, "CarnvHerb Non-Laura vs Sisterlist pValue")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$nonLauraPValue),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$sisterListPValue),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')
}

# --- Compare non-permulated p Values ---

newCorrelationFile = readRDS("Data/carnvHerbsCorrelationFile.rds")
newNonPermulatedPValues = newCorrelationFile$p.adj
oldNonPermulatedPValues = carnvHerbsPreviousAnalysisData$p.adj

plot(newCorrelationFile$p.adj, carnvHerbsPreviousAnalysisData$p.adj)
styleplot(newCorrelationFile$p.adj, carnvHerbsPreviousAnalysisData$p.adj, "CarnvHerb new vs old non-permulated p-values")
Top1000GenesInCommon = length(which(rownames(newCorrelationFile[order(newCorrelationFile$p.adj),])[1:1000] %in% carnvHerbsPreviousAnalysisData[order(carnvHerbsPreviousAnalysisData$p.adj),]$X[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')


# -- Compare clades-correlation vs non-clades correlation values --- 

cladeCorrelationFile = readRDS("Data/carnvHerbsCladesCOrrelationFile.rds")

#p.adj
plot(newCorrelationFile$p.adj, cladeCorrelationFile$p.adj)
styleplot(newCorrelationFile$p.adj, cladeCorrelationFile$p.adj, "CarnvHerb Clades vs non-clades non-permulated p-values")
Top1000GenesInCommon = length(which(rownames(newCorrelationFile[order(newCorrelationFile$p.adj),])[1:1000] %in% rownames(newCorrelationFile[order(newCorrelationFile$p.adj),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(cladeCorrelationFile$p.adj, carnvHerbsPreviousAnalysisData$p.adj)
styleplot(cladeCorrelationFile$p.adj, carnvHerbsPreviousAnalysisData$p.adj, "CarnvHerb clade-based vs old non-permulated p-values")
Top1000GenesInCommon = length(which(rownames(newCorrelationFile[order(cladeCorrelationFile$p.adj),])[1:1000] %in% carnvHerbsPreviousAnalysisData[order(carnvHerbsPreviousAnalysisData$p.adj),]$X[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(newCorrelationFile$p.adj, carnvHerbsPreviousAnalysisData$p.adj)
styleplot(newCorrelationFile$p.adj, carnvHerbsPreviousAnalysisData$p.adj, "New Non-clade-based vs old non-permulated p-values")
Top1000GenesInCommon = length(which(rownames(newCorrelationFile[order(newCorrelationFile$p.adj),])[1:1000] %in% carnvHerbsPreviousAnalysisData[order(carnvHerbsPreviousAnalysisData$p.adj),]$X[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

#P
plot(newCorrelationFile$P, cladeCorrelationFile$P)
styleplot(newCorrelationFile$P, cladeCorrelationFile$P, "CarnvHerb Clades vs non-clades non-permulated p-values")
Top1000GenesInCommon = length(which(rownames(newCorrelationFile[order(newCorrelationFile$P),])[1:1000] %in% rownames(newCorrelationFile[order(newCorrelationFile$P),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(cladeCorrelationFile$P, carnvHerbsPreviousAnalysisData$P)
styleplot(cladeCorrelationFile$P, carnvHerbsPreviousAnalysisData$P, "CarnvHerb clade-based vs old non-permulated p-values")
Top1000GenesInCommon = length(which(rownames(newCorrelationFile[order(cladeCorrelationFile$P),])[1:1000] %in% carnvHerbsPreviousAnalysisData[order(carnvHerbsPreviousAnalysisData$P),]$X[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(newCorrelationFile$P, carnvHerbsPreviousAnalysisData$P)
styleplot(newCorrelationFile$P, carnvHerbsPreviousAnalysisData$p.adj, "New Non-clade-based vs old non-permulated p-values")
Top1000GenesInCommon = length(which(rownames(newCorrelationFile[order(newCorrelationFile$P),])[1:1000] %in% carnvHerbsPreviousAnalysisData[order(carnvHerbsPreviousAnalysisData$P),]$X[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')





# --- Output a file of the old test statistics 
oldTestStats = carnvHerbsPreviousAnalysisData[,c(2,3,4,5)]
rownames(oldTestStats) = carnvHerbsPreviousAnalysisData$X
saveRDS(oldTestStats, "Output/carnvHerbsOldCorrelationValuesFile.rds")

carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$defaultPosition),]
carnvHerbsDF$oldStatep = carnvHerbsNonLauraTreePValue

carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$lauraOnlyPValue),]
carnvHerbsDF$lauraOnlyRank = 1:nrow(carnvHerbsDF)




# -- Load in Non-clades permulation P Values

carnvHerbsNonCladePValues = readRDS("Data/pValues/carnvHerbsNonCladesPermulationsPValue.rds")
carnvHerbsNonCladePValues = carnvHerbsNonCladePValues[1:16208] #Remove duplicate BCAR1

carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$defaultPosition),]
carnvHerbsDF$NoncladesPermulationsPValue = carnvHerbsNonCladePValues

carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$NoncladesPermulationsPValue),]
carnvHerbsDF$nonCladesRank = 1:nrow(carnvHerbsDF)

carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$defaultPosition),]

# -- Compare Non-clades with other data by rank 
#Laura-only clades
plot(carnvHerbsDF$lauraOnlyRank, carnvHerbsDF$nonCladesRank)
styleplot(carnvHerbsDF$lauraOnlyRank, carnvHerbsDF$nonCladesRank, "CarnvHerb New vs Non-Clades Rank")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$lauraOnlyRank),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$nonCladesRank),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')
#Previous analysis
plot(carnvHerbsDF$previousAnalysisRank, carnvHerbsDF$nonCladesRank)
styleplot(carnvHerbsDF$previousAnalysisRank, carnvHerbsDF$nonCladesRank, "CarnvHerb Old analysis vs Non-Clades Rank")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$previousAnalysisRank),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$nonCladesRank),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

# -- Compare Non-clades with other data by p Value

plot(carnvHerbsDF$lauraOnlyPValue, carnvHerbsDF$NoncladesPermulationsPValue)
styleplot(carnvHerbsDF$lauraOnlyPValue, carnvHerbsDF$NoncladesPermulationsPValue, "CarnvHerb New vs Non-Clades pValue")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$lauraOnlyPValue),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$NoncladesPermulationsPValue),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

plot(carnvHerbsDF$previousAnalysisPValue, carnvHerbsDF$NoncladesPermulationsPValue)
styleplot(carnvHerbsDF$previousAnalysisPValue, carnvHerbsDF$NoncladesPermulationsPValue, "CarnvHerb NOld Analysis vs Non-Clades pValue")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$previousAnalysisPValue),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$NoncladesPermulationsPValue),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')



#Compare old correlation statistic new analysis to other data
#by rank
plot(carnvHerbsDF$previousAnalysisRank, carnvHerbsDF$OldCorellationRank)
styleplot(carnvHerbsDF$previousAnalysisRank, carnvHerbsDF$OldCorellationRank, "New vs Old Analysis on old correlation statistics by Rank")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$previousAnalysisRank),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$OldCorellationRank),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')

#by p Value
plot(carnvHerbsDF$previousAnalysisPValue, carnvHerbsDF$oldCorrelationNewPermulationPValue)
styleplot(carnvHerbsDF$previousAnalysisPValue, carnvHerbsDF$oldCorrelationNewPermulationPValue, "New vs Old Analysis on old correlation statistics by pValue")
Top1000GenesInCommon = length(which(rownames(carnvHerbsDF[order(carnvHerbsDF$previousAnalysisPValue),])[1:1000] %in% rownames(carnvHerbsDF[order(carnvHerbsDF$oldCorrelationNewPermulationPValue),])[1:1000]))
legend("bottomright", bty="n", legend=paste("Top 1000 genes in common:", Top1000GenesInCommon), text.col = 'blue4')







# --- Get overlapping top genes ------

carnvHerbsDFByClades = carnvHerbsDF[order(carnvHerbsDF$lauraOnlyRank),]
carnvHerbsDFByNonClades = carnvHerbsDF[order(carnvHerbsDF$nonCladesRank),]
carnvHerbsDFByOld= carnvHerbsDF[order(carnvHerbsDF$previousAnalysisRank),]
carnvHerbsDFByOldOneSided= carnvHerbsDF[order(carnvHerbsDF$previousAnalysisOneSideRank),]


numberOfTop100GeneOverlap = length(which(rownames(carnvHerbsDFByClades)[1:100] %in% rownames(carnvHerbsDFByNonClades)[1:100]))
top100GeneOverlap = rownames(carnvHerbsDFByClades)[which(rownames(carnvHerbsDFByClades)[1:100] %in% rownames(carnvHerbsDFByNonClades)[1:100])]

carnvHerbsDFByClades[which(rownames(carnvHerbsDFByClades)[1:100] %in% rownames(carnvHerbsDFByNonClades)[1:100]),]

numberOfNCTop100GeneOverlap = length(which(rownames(carnvHerbsDFByOld)[1:100] %in% rownames(carnvHerbsDFByNonClades)[1:100]))
top100NCGeneOverlap = rownames(carnvHerbsDFByOld)[which(rownames(carnvHerbsDFByOld)[1:100] %in% rownames(carnvHerbsDFByNonClades)[1:100])]

numberOfNCTop1000GeneOverlap = length(which(rownames(carnvHerbsDFByOld)[1:1000] %in% rownames(carnvHerbsDFByNonClades)[1:1000]))
top1000NCGeneOverlap = rownames(carnvHerbsDFByOld)[which(rownames(carnvHerbsDFByOld)[1:1000] %in% rownames(carnvHerbsDFByNonClades)[1:1000])]
top1000NCGeneOverlap
nonTop1000Genes = which(!rownames(carnvHerbsDFByOld) %in% top1000NCGeneOverlap)
write(paste(top1000NCGeneOverlap, sep = "\n"), "Output/carnvHerbsGenenames.txt")
write(paste(nonTop1000Genes, sep = "\n"), "Output/carnvHerbsBackgroundGenes.txt")

#direct ranked list 
rownames(carnvHerbsDFByNonClades)
write(paste(rownames(carnvHerbsDFByNonClades), sep = "\n"), "Output/carnvHerbsRankedNewGenes.txt")
write(paste(rownames(carnvHerbsDFByOld), sep = "\n"), "Output/carnvHerbsRankedOld2SidedGenes.txt")
write(paste(rownames(carnvHerbsDFByOldOneSided), sep = "\n"), "Output/carnvHerbsRankedOld1SidedGenes.txt")

#shorter foreground-background list 
numberOfNCTop200GeneOverlap = length(which(rownames(carnvHerbsDFByOld)[1:200] %in% rownames(carnvHerbsDFByNonClades)[1:200]))
top200NCGeneOverlap = rownames(carnvHerbsDFByOld)[which(rownames(carnvHerbsDFByOld)[1:200] %in% rownames(carnvHerbsDFByNonClades)[1:200])]
length(top200NCGeneOverlap)
nonTop200Genes = rownames(carnvHerbsDFByOld)[which(!rownames(carnvHerbsDFByOld) %in% top200NCGeneOverlap)]

write(paste(top200NCGeneOverlap, sep = "\n"), "Output/carnvHerbsTop200FGGene.txt")
write(paste(nonTop200Genes, sep = "\n"), "Output/carnvHerbsTop200BGGenes.txt")


# ---- Generate sign-based ranking of genes 

# Sort by the non-permulated p value
sortableNonCladeTestStats$permPVal = readRDS("Data/pValues/carnvHerbsNonCladesPermulationsPValue.rds")
sortableNonCladeTestStats$DefaultOrder = 1:nrow(sortableNonCladeTestStats)

#Compare the permulated andn non-permulated results: 
plot(NoPermTestStatsBySign$p.adj, NoPermTestStatsBySign$permPVal)
styleplot(NoPermTestStatsBySign$p.adj, NoPermTestStatsBySign$permPVal, "perm vs non-perm new data")

sortableNonCladeTestStats = sortableNonCladeTestStats[order(sortableNonCladeTestStats$p.adj),]
sortableNonCladeTestStats$nonPermRank = 1:nrow(sortableNonCladeTestStats)
sortableNonCladeTestStats = sortableNonCladeTestStats[order(sortableNonCladeTestStats$permPVal),]
sortableNonCladeTestStats$PermRank = 1:nrow(sortableNonCladeTestStats)
sortableNonCladeTestStats = sortableNonCladeTestStats[order(sortableNonCladeTestStats$DefaultOrder),]

plot(sortableNonCladeTestStats$p.adj, sortableNonCladeTestStats$permPVal)
styleplot(sortableNonCladeTestStats$p.adj, sortableNonCladeTestStats$permPVal, "perm vs non-perm new data")

plot(sortableNonCladeTestStats$nonPermRank, sortableNonCladeTestStats$PermRank)
styleplot(sortableNonCladeTestStats$nonPermRank, sortableNonCladeTestStats$PermRank, "perm vs non-perm new data Rank ")


negativePNoPermTestStats = sortableNonCladeTestStats[order(sortableNonCladeTestStats$p.adj),]
negativePNoPermTestStats = negativePNoPermTestStats[which(negativePNoPermTestStats$Rho < 0), ]

positivePNoPermTestStats = sortableNonCladeTestStats[order(sortableNonCladeTestStats$p.adj, decreasing = T),]
positivePNoPermTestStats = positivePNoPermTestStats[which(positivePNoPermTestStats$Rho > 0), ]

NoPermTestStatsBySign = rbind(negativePNoPermTestStats, positivePNoPermTestStats)

plot(NoPermTestStatsBySign$Rho)

write(paste(rownames(NoPermTestStatsBySign), sep = "\n"), "Output/carnvHerbsNoPermGenesRankedBySign.txt")

NoPermTestStatsBySignPositiveTop = NoPermTestStatsBySign[nrow(NoPermTestStatsBySign):1,]
NonCladeTestStatsBySignPositiveTop[14967,]


write(paste(rownames(NoPermTestStatsBySignPositiveTop), sep = "\n"), "Output/carnvHerbsNoPermGenesRankedBySignPositiveTop.txt")




# generate one direction only result tables

sortableNonCladeTestStats = newCorrelationFile

sortableNonCladeTestStats$permPVal = readRDS("Data/pValues/carnvHerbsNonCladesPermulationsPValue.rds")
sortableNonCladeTestStats$DefaultOrder = 1:nrow(sortableNonCladeTestStats)

negativePNonCladeTestStats = sortableNonCladeTestStats[order(sortableNonCladeTestStats$permPVal),]
negativePNonCladeTestStats = negativePNonCladeTestStats[which(negativePNonCladeTestStats$Rho < 0), ]

positivePNonCladeTestStats = sortableNonCladeTestStats[order(sortableNonCladeTestStats$permPVal, decreasing = T),]
positivePNonCladeTestStats = positivePNonCladeTestStats[which(positivePNonCladeTestStats$Rho > 0), ]

NonCladeTestStatsBySign = rbind(negativePNonCladeTestStats, positivePNonCladeTestStats)

plot(NonCladeTestStatsBySign$Rho)

write(paste(rownames(NonCladeTestStatsBySign), sep = "\n"), "Output/carnvHerbsNonCladesGenesRankedBySign.txt")

NonCladeTestStatsBySignPositiveTop = NonCladeTestStatsBySign[nrow(NonCladeTestStatsBySign):1,]
NonCladeTestStatsBySignPositiveTop[14967,]


write(paste(rownames(NonCladeTestStatsBySignPositiveTop), sep = "\n"), "Output/carnvHerbsNonCladesGenesRankedBySignPositiveTop.txt")


#compare perm effects
all.equal(negativePNoPermTestStats, negativePNonCladeTestStats)
all.equal(positivePNoPermTestStats, positivePNonCladeTestStats)
all.equal(NoPermTestStatsBySignPositiveTop, NonCladeTestStatsBySignPositiveTop)
all.equal(NoPermTestStatsBySign, NonCladeTestStatsBySign )

positivePPermEffectTest = positivePNoPermTestStats
positivePPermEffectTest = positivePPermEffectTest[order(positivePPermEffectTest$p.adj),]
positivePPermEffectTest$nonPermRankPostive = 1:nrow(positivePPermEffectTest)
positivePPermEffectTest = positivePPermEffectTest[order(positivePPermEffectTest$permPVal),]
positivePPermEffectTest$PermRankPositve = 1:nrow(positivePPermEffectTest)
positivePPermEffectTest = positivePPermEffectTest[order(positivePPermEffectTest$DefaultOrder),]

plot(positivePPermEffectTest$nonPermRankPostive, positivePPermEffectTest$PermRankPositve)


#-- try to figure out why dynein signal still present --

#working with the old data

carnvHerbsPreviousAnalysisData = read.csv("Data/pValues/CorrelationFishCarnHerbLaurasiatheria_wminsp_wnopermp_wcount_waddlperms0907.csv")
carnvHerbsPreviousAnalysisDataNegative = carnvHerbsPreviousAnalysisData[which(carnvHerbsPreviousAnalysisData$RhoSign == "Rho_Negative"), ]
carnvHerbsPreviousAnalysisDataPositive = carnvHerbsPreviousAnalysisData[which(carnvHerbsPreviousAnalysisData$RhoSign == "Rho_Positive"), ]

previousAnalysisGenesRankedBySignPostiveTop = rbind(carnvHerbsPreviousAnalysisDataPositive[order(carnvHerbsPreviousAnalysisDataPositive$permp.2sided),], carnvHerbsPreviousAnalysisDataNegative[order(carnvHerbsPreviousAnalysisDataNegative$permp.2sided, decreasing =  T),])

write(paste(previousAnalysisGenesRankedBySignPostiveTop$X, sep = "\n"), "Output/oldAnalysisGenesRankedBySignPositiveTop.txt")


previousAnalysisGenesRankedBySignPostiveTop$X
rownames(NonCladeTestStatsBySignPositiveTop)
rownames(NoPermTestStatsBySignPositiveTop)

#----- redo the new GO data to be clea 


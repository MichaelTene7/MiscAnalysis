newCorrelationFile = readRDS("Data/carnvHerbsCorrelationFile.rds")
newAnalysisData = newCorrelationFile
newAnalysisData$permPValue = readRDS("Data/pValues/carnvHerbsNonCladesPermulationsPValue.rds")
newAnalysisData$sameSignPermPValue = readRDS("Data/pValues/carnvHerbsSameSignPermulationsPValue.rds")
carnvHerbsPreviousAnalysisData = read.csv("Data/pValues/CorrelationFishCarnHerbLaurasiatheria_wminsp_wnopermp_wcount_waddlperms0907.csv")


#Plot normal vs same sign p value
par(mfrow = c(1,1))
plot(newAnalysisData$permPValue, newAnalysisData$sameSignPermPValue)


#Show differences in permualt p value distributions
par(mfrow = c(1,3))
hist(carnvHerbsPreviousAnalysisData$permp.2sided, breaks = 40, xaxp = c(0,1,20))
hist(newAnalysisData$permPValue, breaks = 40, xaxp = c(0,1,20))
hist(newAnalysisData$sameSignPermPValue, breaks = 40, xaxp = c(0,1,20))


par(mfrow = c(1,2))
hist(newAnalysisData$p.adj, breaks = 40)
hist(carnvHerbsPreviousAnalysisData$p.adj)

#Check for number of significant rho positives
newAnalysisDataNegative = newAnalysisData[which(newAnalysisData$Rho <= 0),]
newAnalysisDataPositive = newAnalysisData[which(newAnalysisData$Rho > 0),]

length(which(newAnalysisDataPositive$permPValue < 0.05))

#compare dip between new and old analysis 
significantOldGene = carnvHerbsPreviousAnalysisData$X[which(carnvHerbsPreviousAnalysisData$permp.2sided < 0.05)]
significantNewGene = rownames(newAnalysisData[which(newAnalysisData$permPValue < 0.05),])
significantSSGene = rownames(newAnalysisData[which(newAnalysisData$sameSignPermPValue < 0.05),])

length(significantOldGene)
length(significantNewGene)
length(significantSSGene)

length(which(significantOldGene %in% significantNewGene))

uniqueNewGenes = significantNewGene[!(significantNewGene %in% significantOldGene)]

length(uniqueNewGenes)


# ------- Figuring out what's up with the same sign p values and them not looking like the others ------

plot(carnvHerbsPreviousAnalysisData$permp.2sided, newAnalysisData$permPValue)

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

sameSignPValue = readRDS("Data/pValues/carnvHerbsSameSignPermulationsPValue.rds")
sameSignPValue = sameSignPValue[1:16208]
carnvHerbsDF$sameSignPValue = sameSignPValue
carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$sameSignPValue),]
carnvHerbsDF$sameSignRank = 1:nrow(carnvHerbsDF)

carnvHerbsDF = carnvHerbsDF[order(carnvHerbsDF$defaultPosition),]

plot(carnvHerbsDF$lauraOnlyPValue, carnvHerbsDF$sameSignPValue)

# THESE ARE THE SAME, DETERMINED THAT THIS "SAME SIGN" VALUE WAS ACTUALLY THE CLADE BASED VALUES
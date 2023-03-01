library(qvalue)
library(RERconverge)
library(ggplot2)

# --- import analysis data 
newCorrelationFile = readRDS("Data/carnvHerbsCorrelationFile.rds")
newAnalysisData = newCorrelationFile
newAnalysisData$permPValue = readRDS("Data/pValues/carnvHerbsNonCladesPermulationsPValue.rds")
newAnalysisData$sameSignPermPValue = readRDS("Data/pValues/carnvHerbsSameSignPermulationsPValue.rds")
simplePermQ = qvalue(p = newAnalysisData$permPValue)
newAnalysisData$simplePermQVal = simplePermQ$qvalues
samesignPermQ = qvalue(p = newAnalysisData$sameSignPermPValue)
newAnalysisData$sameSignQVal = samesignPermQ$qvalues

carnvHerbsPreviousAnalysisData = read.csv("Data/pValues/CorrelationFishCarnHerbLaurasiatheria_wminsp_wnopermp_wcount_waddlperms0907.csv")

newAnalysisDataPositive = newAnalysisData[which(newAnalysisData$Rho > 0),]
newAnalysisDataNegative = newAnalysisData[which(newAnalysisData$Rho < 0),]
# --- 

# --- Import RERs
oldRERs = readRDS("Data/RERFiles/mamRERCMU_FishCarn.rds")
newRERs = readRDS("Data/RERFiles/carnvHerbsRERFile2-2-23.rds")


# -- Determine which genes differ in Q values ---- 
significantOldGene = carnvHerbsPreviousAnalysisData$X[which(carnvHerbsPreviousAnalysisData$qvalue.2sided < 0.05)]
significantNewGene = rownames(newAnalysisData[which(newAnalysisData$simplePermQ < 0.05),])

differingSignificantGenes = significantOldGene[! significantOldGene %in% significantNewGene]


plotRers(oldRERs, "TBCB")
plotRers(newRERs, "TBCB")



?plotRers

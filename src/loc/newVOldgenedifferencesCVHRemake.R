library(qvalue)
library(RERconverge)
library(ggplot2)

# --- import analysis data 
newCorrelationFile = readRDS("Data/CVHRemakeCorrelationFile.rds")
newAnalysisData = newCorrelationFile
newAnalysisData$permPValue = readRDS("Data/pValues/CVHRemakeCombinedPrunedFastAppendedPermulationsPValue.rds")
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
newRERs = readRDS("Data/CVHRemakeRERFile.rds")


# -- Determine which genes differ in Q values ---- 
significantOldGene = carnvHerbsPreviousAnalysisData$X[which(carnvHerbsPreviousAnalysisData$qvalue.2sided < 0.05)]
significantNewGene = rownames(newAnalysisData[which(newAnalysisData$simplePermQ < 0.05),])

RhoChange = abs(newAnalysisData$Rho) - abs(carnvHerbsPreviousAnalysisData$Rho)
RhoChangePercent = RhoChange/newAnalysisData$Rho

newAnalysisData$RhoChange = RhoChange
newAnalysisData$RhoChangePercent = RhoChangePercent



pChange = newAnalysisData$permPValue - carnvHerbsPreviousAnalysisData$permp.2sided
pChangePercent = pChange/newAnalysisData$permPValue
newAnalysisData$pChange = pChange
newAnalysisData$pChangePercent = pChangePercent

newAnalysisData[order(newAnalysisData$RhoChange),]

oldData = carnvHerbsPreviousAnalysisData

compareData = data.frame(newAnalysisData$Rho, oldData$Rho, newAnalysisData$p.adj, oldData$p.adj, newAnalysisData$permPValue, oldData$permp.2sided, RhoChange, RhoChangePercent, pChange, pChangePercent)
row.names(compareData) = rownames(newAnalysisData)
compareData
compareData[order(compareData$oldData.permp.2sided),]

differingSignificantGenes = significantOldGene[! significantOldGene %in% significantNewGene]
write.csv(compareData, "Output/NewOldAnalysisCompare.csv")



plotRers(oldRERs, "TBCB")
plotRers(newRERs, "TBCB")



compareCorrelations = data.frame(newAnalysisData$p.adj, carnvHerbsPreviousAnalysisData$p.adj)



?plotRers

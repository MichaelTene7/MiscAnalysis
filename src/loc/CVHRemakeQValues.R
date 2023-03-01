library(qvalue)

CVHCorrelationFile = readRDS("Data/CVHRemakeCorrelationFile.rds")
CVHData = newCorrelationFile
CVHData$permPValue = readRDS("Data/CVHRemakeCombinedPrunedFastAllPermulationsPValue.rds")

str(CVHData$permPValue)

CVHData[order(CVHData$permPValue),]
CVHData[order(CVHData$p.adj),]

range(CVHData$permPValue, na.rm = T)

CVHData$permPValue[CVHData$permPValue >1]
plot(CVHData$permPValue)


# ---- 
simplePermQ = qvalue(p = CVHData$permPValue)
CVHData$simplePermQVal = simplePermQ$qvalues

CVHDataPositive = CVHData[which(newAnalysisData$Rho > 0),]
CVHDataNegative = CVHData[which(newAnalysisData$Rho < 0),]

plot(CVHDataPositive$permPValue)
plot(CVHDataNegative$permPValue)

# -- order by q values 
CVHData[order(CVHData$simplePermQVal),]


# -- plot q values -- 
hist(CVHData$simplePermQVal, breaks = 40, xaxp = c(0,1,20))

hist(CVHData$simplePermQVal, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))


par(mfrow = c(1,1))
hist(newAnalysisData$cladeBasedQVal, breaks = 40, xaxp = c(0,1,20))
newAnalysisDataLowQ = newAnalysisData[which(newAnalysisData$cladeBasedQVal <0.9),]
hist(newAnalysisDataLowQ$cladeBasedQVal, breaks = 40, xaxp = c(0,1,20))

# -- number of q values <0.05
newAnalysisDataVeryLowQ = newAnalysisData[which(newAnalysisData$sameSignQVal < 0.05),]
length(which(newAnalysisData$cladeBasedQVal < 0.05))
length(which(newAnalysisDataNegative$cladeBasedQVal < 0.05))
length(which(newAnalysisDataPositive$cladeBasedQVal < 0.05))


# -- plot the p and q values -- 
par(mfrow = c(2,3))
hist(carnvHerbsPreviousAnalysisData$permp.2sided, breaks = 40, xaxp = c(0,1,20))
hist(newAnalysisData$permPValue, breaks = 40, xaxp = c(0,1,20))
hist(newAnalysisData$sameSignPermPValue, breaks = 40, xaxp = c(0,1,20))

hist(carnvHerbsPreviousAnalysisData$qvalue.2sided, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))
hist(newAnalysisData$simplePermQVal, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))
hist(newAnalysisData$sameSignQVal, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))

length(newAnalysisData[which(newAnalysisData$sameSignQVal <0.05),])

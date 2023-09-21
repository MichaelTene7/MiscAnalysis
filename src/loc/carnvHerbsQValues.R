library(qvalue)

newCorrelationFile = readRDS("Data/carnvHerbsCorrelationFile.rds")
newAnalysisData = newCorrelationFile
newAnalysisData$permPValue = readRDS("Data/pValues/carnvHerbsNonCladesPermulationsPValue.rds")
newAnalysisData$cladeBasedPermPVal = readRDS("Data/pValues/carnvHerbsSameSignPermulationsPValue.rds")
carnvHerbsPreviousAnalysisData = read.csv("Data/pValues/CorrelationFishCarnHerbLaurasiatheria_wminsp_wnopermp_wcount_waddlperms0907.csv")

simplePermQ = qvalue(p = newAnalysisData$permPValue)
newAnalysisData$simplePermQVal = simplePermQ$qvalues

cladeBasedPermQ = qvalue(p = newAnalysisData$cladeBasedPermPVal)
newAnalysisData$cladeBasedQVal = cladeBasedPermQ$qvalues

newAnalysisDataPositive = newAnalysisData[which(newAnalysisData$Rho > 0),]
newAnalysisDataNegative = newAnalysisData[which(newAnalysisData$Rho < 0),]

# -- order by q values 
newAnalysisData[order(newAnalysisData$simplePermQVal),]
newAnalysisData[order(newAnalysisData$cladeBasedQVal),]

carnvHerbsPreviousAnalysisData[order(carnvHerbsPreviousAnalysisData$qvalue.2sided), c(1,2,4,5,6,9,14)]


# -- plot q values -- 
hist(carnvHerbsPreviousAnalysisData$qvalue.2sided, breaks = 40, xaxp = c(0,1,20))
hist(newAnalysisData$simplePermQVal, breaks = 40, xaxp = c(0,1,20))
hist(newAnalysisData$cladeBasedQVal, breaks = 40, xaxp = c(0,1,20))

hist(carnvHerbsPreviousAnalysisData$qvalue.2sided, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))
hist(newAnalysisData$simplePermQVal, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))
hist(newAnalysisData$cladeBasedQVal, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))

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

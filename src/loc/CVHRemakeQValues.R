library(qvalue)
library(RERconverge)

CVHCorrelationFile = readRDS("Data/CVHRemakeCorrelationFile.rds")
CVHData = CVHCorrelationFile
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

CVHDataPositive = CVHData[which(CVHData$Rho > 0),]
CVHDataNegative = CVHData[which(CVHData$Rho < 0),]

plot(CVHDataPositive$permPValue)
plot(CVHDataNegative$permPValue)
hist(CVHDataPositive$permPValue, breaks = 40, xaxp = c(0,1,20))
hist(CVHDataNegative$permPValue, breaks = 40, xaxp = c(0,1,20))

hist(CVHDataPositive$p.adj, breaks = 40, xaxp = c(0,1,20))
hist(CVHDataNegative$p.adj, breaks = 40, xaxp = c(0,1,20))

# -- order by q values 
CVHData[order(CVHData$simplePermQVal),]
range(CVHData$simplePermQVal, na.rm = T)

# -- plot q values -- 
hist(CVHData$simplePermQVal, breaks = 40, xaxp = c(0,1,20))

hist(CVHData$simplePermQVal, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))

genesRankedPermP = rownames(CVHData[order(CVHData$permPValue),])
write(genesRankedPermP, "Output/NewCVHRankedPermGenes.txt")

# -- number of q values <0.05
CVHDataVeryLowQ = CVHData[which(CVHData$simplePermQVal < 0.05),]
length(which(CVHData$simplePermQVal < 0.05))


#plot RERs
RERFile = readRDS("Data/CVHRemakeRERFile.rds")
PathsFile = readRDS("Data/CVHRemakePathsFile.rds")
plotRers(RERFile, "AC010255", PathsFile)
for(i in 1:100){
  plotRers(RERFile, genesRankedPermP[i], PathsFile)
  message(i)
  Sys.sleep(4)
}
#




# -- plot the p and q values -- 
par(mfrow = c(2,3))
hist(carnvHerbsPreviousAnalysisData$permp.2sided, breaks = 40, xaxp = c(0,1,20))
hist(newAnalysisData$permPValue, breaks = 40, xaxp = c(0,1,20))
hist(newAnalysisData$sameSignPermPValue, breaks = 40, xaxp = c(0,1,20))

hist(carnvHerbsPreviousAnalysisData$qvalue.2sided, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))
hist(newAnalysisData$simplePermQVal, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))
hist(newAnalysisData$sameSignQVal, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))

length(newAnalysisData[which(newAnalysisData$sameSignQVal <0.05),])

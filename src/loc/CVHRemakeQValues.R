library(qvalue)
library(RERconverge)
library(dprint)

CVHCorrelationFile = readRDS("Data/CVHRemakeCorrelationFile.rds")
CVHData = CVHCorrelationFile
CVHData$permPValue = readRDS("Data/CVHRemakeCombinedPrunedFastAllPermulationsPValue.rds")
CVHData$permPValueSameSign = readRDS("Data/CVHRemakeCombinedPrunedFastAllPermulationsPValue-SameSign.rds")
CVHData$permPValueAllSign = readRDS("Data/CVHRemakeCombinedPrunedFastAllPermulationsPValue-AllSign.rds")


str(CVHData$permPValue)

CVHData[order(CVHData$permPValueSameSign),]
CVHData[order(CVHData$p.adj),]

range(CVHData$permPValueSameSign, na.rm = T)

plot(CVHData$permPValueSameSign)


# ---- Same sign version ---
# --------------------------
sameSignPermQ = qvalue(p = CVHData$permPValueSameSign)
CVHData$sameSignPermQVal = sameSignPermQ$qvalues

CVHDataPositive = CVHData[which(CVHData$Rho > 0),]
CVHDataNegative = CVHData[which(CVHData$Rho < 0),]

plot(CVHDataPositive$permPValueSameSign)
plot(CVHDataNegative$permPValueSameSign)
hist(CVHDataPositive$permPValueSameSign, breaks = 40, xaxp = c(0,1,20))
hist(CVHDataNegative$permPValueSameSign, breaks = 40, xaxp = c(0,1,20))

hist(CVHDataPositive$p.adj, breaks = 40, xaxp = c(0,1,20))
hist(CVHDataNegative$p.adj, breaks = 40, xaxp = c(0,1,20))

# -- order by q values 
CVHData[order(CVHData$sameSignPermQ),]
range(CVHData$simplePermQVal, na.rm = T)

# -- plot q values -- 
hist(CVHData$sameSignPermQ, breaks = 40, xaxp = c(0,1,20))

hist(CVHData$sameSignPermQ, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))

genesRankedPermP = rownames(CVHData[order(CVHData$permPValueSameSign),])
write(genesRankedPermP, "Output/NewCVHRankedPermGenesSameSign.txt")

# -- number of q values <0.05
CVHDataVeryLowQ = CVHData[which(CVHData$sameSignPermQ < 0.05),]
length(which(CVHData$sameSignPermQ < 0.05))


# ---- All sign version ----
# -------------------------
allSignPermQ = qvalue(p = CVHData$permPValueAllSign)
CVHData$allSignPermQVal = allSignPermQ$qvalues

CVHDataPositive = CVHData[which(CVHData$Rho > 0),]
CVHDataNegative = CVHData[which(CVHData$Rho < 0),]

plot(CVHDataPositive$permPValueAllSign)
plot(CVHDataNegative$permPValueAllSign)
hist(CVHDataPositive$permPValueAllSign, breaks = 40, xaxp = c(0,1,20))
hist(CVHDataNegative$permPValueAllSign, breaks = 40, xaxp = c(0,1,20))

hist(CVHDataPositive$p.adj, breaks = 40, xaxp = c(0,1,20))
hist(CVHDataNegative$p.adj, breaks = 40, xaxp = c(0,1,20))

# -- order by q values 
CVHData[order(CVHData$allSignPermQ),]
range(CVHData$simplePermQVal, na.rm = T)

# -- plot q values -- 
hist(CVHData$allSignPermQ, breaks = 40, xaxp = c(0,1,20))

hist(CVHData$allSignPermQ, breaks = 40, xaxp = c(0,1,20), ylim = c(0,100))

genesRankedPermP = rownames(CVHData[order(CVHData$permPValueAllSign),])
write(genesRankedPermP, "Output/NewCVHRankedPermGenesAllSign.txt")

# -- number of q values <0.05
CVHDataVeryLowQ = CVHData[which(CVHData$allSignPermQ < 0.05),]
length(which(CVHData$allSignPermQ < 0.05))


# ---- Compare Same and All sign ---
# --------------------------------
par(mfrow = c(1,2))

hist(CVHDataPositive$permPValueSameSign, breaks = 40, xaxp = c(0,1,20))
hist(CVHDataPositive$permPValueAllSign, breaks = 40, xaxp = c(0,1,20))

hist(CVHDataNegative$permPValueSameSign, breaks = 40, xaxp = c(0,1,20))
hist(CVHDataNegative$permPValueAllSign, breaks = 40, xaxp = c(0,1,20))

par(mfrow = c(1,1))
plot(CVHData$permPValueSameSign, CVHData$permPValueAllSign)
abline(fit <- lm(CVHData$permPValueSameSign ~ CVHData$permPValueAllSign), col='red')
legend("topleft", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=12)), text.col = 'blue')


printTest = head(CVHData[order(CVHData$allSignPermQ),], n=40)
sinkplot()
printTest
sinkplot("plot")
sinkplot("cancel")

?dprint()

# -----
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






# --- Not remaned Version -----
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

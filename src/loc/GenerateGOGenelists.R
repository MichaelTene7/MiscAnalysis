# --- old analysis ---
carnvHerbsPreviousAnalysisData = read.csv("Data/CorrelationFishCarnHerbLaurasiatheria_wminsp_wnopermp_wcount_waddlperms0907.csv")
carnvHerbsPreviousAnalysisDataNegative = carnvHerbsPreviousAnalysisData[which(carnvHerbsPreviousAnalysisData$RhoSign == "Rho_Negative"), ]
carnvHerbsPreviousAnalysisDataPositive = carnvHerbsPreviousAnalysisData[which(carnvHerbsPreviousAnalysisData$RhoSign == "Rho_Positive"), ]

previousAnalysisPermulationsGenesRankedBySignPostiveTop = rbind(carnvHerbsPreviousAnalysisDataPositive[order(carnvHerbsPreviousAnalysisDataPositive$permp.2sided),], carnvHerbsPreviousAnalysisDataNegative[order(carnvHerbsPreviousAnalysisDataNegative$permp.2sided, decreasing =  T),])
previousAnalysisPermulationsGenesRankedBySignNegativeTop = previousAnalysisPermulationsGenesRankedBySignPostiveTop[nrow(previousAnalysisPermulationsGenesRankedBySignPostiveTop):1, ]


write(paste(previousAnalysisPermulationsGenesRankedBySignPostiveTop$X, sep = "\n"), "Output/oldAnalysisPermulationsGenesRankedBySignPositiveTop.txt")
write(paste(previousAnalysisPermulationsGenesRankedBySignNegativeTop$X, sep = "\n"), "Output/oldAnalysisPermulationsGenesRankedBySignNegativeTop.txt")

previousAnalysisNonPermGenesRankedBySignPostiveTop = rbind(carnvHerbsPreviousAnalysisDataPositive[order(carnvHerbsPreviousAnalysisDataPositive$p.adj),], carnvHerbsPreviousAnalysisDataNegative[order(carnvHerbsPreviousAnalysisDataNegative$p.adj, decreasing =  T),])
previousAnalysisNonpermGenesRankedBySignNegativeTop = previousAnalysisNonPermGenesRankedBySignPostiveTop[nrow(previousAnalysisNonPermGenesRankedBySignPostiveTop):1, ]


write(paste(previousAnalysisNonPermGenesRankedBySignPostiveTop$X, sep = "\n"), "Output/oldAnalysisNonPermsGenesRankedBySignPositiveTop.txt")
write(paste(previousAnalysisNonpermGenesRankedBySignNegativeTop$X, sep = "\n"), "Output/oldAnalysisNonPermsGenesRankedBySignNegativeTop.txt")


# -- New Analysis ---

newCorrelationFile = readRDS("Data/carnvHerbsCorrelationFile.rds")
newAnalysisData = newCorrelationFile
newAnalysisData$permPValue = readRDS("Data/pValues/carnvHerbsNonCladesPermulationsPValue.rds")

newAnalysisDataNegative = newAnalysisData[which(newAnalysisData$Rho <= 0),]
newAnalysisDataPositive = newAnalysisData[which(newAnalysisData$Rho > 0),]

newAnalysisPermulationsGenesRankedBySignPositiveTop = rbind(newAnalysisDataPositive[order(newAnalysisDataPositive$permPValue),], newAnalysisDataNegative[order(newAnalysisDataNegative$permPValue, decreasing =  T),])
newAnalysisPermulationsGenesRankedBySignNegativeTop = newAnalysisPermulationsGenesRankedBySignPositiveTop[nrow(newAnalysisPermulationsGenesRankedBySignPositiveTop):1,]

write(paste(rownames(newAnalysisPermulationsGenesRankedBySignPositiveTop), sep = "\n"), "Output/newAnalysisPermulationsGenesRankedBySignPositiveTop.txt")
write(paste(rownames(newAnalysisPermulationsGenesRankedBySignNegativeTop), sep = "\n"), "Output/newAnalysisPermulationsGenesRankedBySignNegativeTop.txt")

newAnalysisNonPermGenesRankedBySignPositiveTop = rbind(newAnalysisDataPositive[order(newAnalysisDataPositive$p.adj),], newAnalysisDataNegative[order(newAnalysisDataNegative$p.adj, decreasing =  T),])
newAnalysisNonPermsGenesRankedBySignNegativeTop = newAnalysisNonPermGenesRankedBySignPositiveTop[nrow(newAnalysisNonPermGenesRankedBySignPositiveTop):1,]

write(paste(rownames(newAnalysisNonPermGenesRankedBySignPositiveTop), sep = "\n"), "Output/newAnalysisNonPermGenesRankedBySignPositiveTop.txt")
write(paste(rownames(newAnalysisNonPermsGenesRankedBySignNegativeTop), sep = "\n"), "Output/newAnalysisNonPermGenesRankedBySignNegativeTop.txt")


# something weird is happening with the non-permulated new negative top results 
newAnalysisDataNegative[order(newAnalysisDataNegative$p.adj),]

all.equal(rownames(newAnalysisData), names(readRDS("Data/pValues/carnvHerbsNonCladesPermulationsPValue.rds")))

newAnalysisDataNegative[order(newAnalysisDataNegative$permPValue),]

newAnalysisData[order(newAnalysisData$permPValue),]
newAnalysisData[order(newAnalysisData$p.adj),]

plot(carnvHerbsPreviousAnalysisData$p.adj, newAnalysisData$p.adj)
plot(carnvHerbsPreviousAnalysisData$permp.2sided, newAnalysisData$permPValue)
plot(newAnalysisData$p.adj, newAnalysisData$permPValue)

plot(carnvHerbsPreviousAnalysisDataNegative$p.adj, newAnalysisDataNegative$p.adj)

all.equal(rownames(newAnalysisData), carnvHerbsPreviousAnalysisData$X)
combinedAnalaysisData = cbind(carnvHerbsPreviousAnalysisData, newAnalysisData)

combinedAnalaysisDataNegative = combinedAnalaysisData[which(combinedAnalaysisData$RhoSign == "Rho_Negative"),]

plot(combinedAnalaysisDataNegative[,5], combinedAnalaysisDataNegative[,18])

newAnalysisDataNegativeMatching = newAnalysisData[which(rownames(newAnalysisData) %in% combinedAnalaysisDataNegativeMatching$X),]

combinedAnalaysisDataNegativeMatching = combinedAnalaysisDataNegative[which(combinedAnalaysisDataNegative$X %in% rownames(newAnalysisDataNegative)),]
all.equal(rownames(newAnalysisDataNegativeMatching), combinedAnalaysisDataNegativeMatching$X)

plot(combinedAnalaysisDataNegativeMatching$p.adj, newAnalysisDataNegativeMatching$p.adj)

combinedAnalaysisDataNegative[order(combinedAnalaysisDataNegative[,18]),]$X
combinedAnalaysisDataNegative[order(combinedAnalaysisDataNegative[,5]),]$X

significantNewAnalysisDataNegative = newAnalysisDataNegative[newAnalysisDataNegative$p.adj <0.06,]
permsignificantNewAnalysisDataNegative = newAnalysisDataNegative[newAnalysisDataNegative$permPValue <0.06,]

newAnalysisDataPositive

significantNewAnalysisDataPositive = newAnalysisDataPositive[newAnalysisDataPositive$p.adj <0.06,]
permsignificantNewAnalysisDataPositive = newAnalysisDataPositive[newAnalysisDataPositive$permPValue <0.06,]

dyneinGenes = c("DNAH17", "TEKT2","CCDC114","CCDC151" ,"TTC12","DRC1" ,"TTC25" ,"DAW1" ,"DNAI2" ,"CCDC65" ,"DNAH1" ,"ARMC4" ,"DNAH5" ,"DNAH7" ,"DNAI1" ,"SPAG1")

dyneinGenes %in% rownames(significantNewAnalysisDataPositive)
dyneinGenes %in% rownames(permsignificantNewAnalysisDataPositive)

significantNewAnalysisDataPositive$p.adj[rownames(significantNewAnalysisDataPositive) %in% dyneinGenes]
significantNewAnalysisDataPositive$permPValue[rownames(significantNewAnalysisDataPositive) %in% dyneinGenes]
significantNewAnalysisDataPositive$sameSignPermPValue[rownames(significantNewAnalysisDataPositive) %in% dyneinGenes]

significantOldAnalysisDataNegative = carnvHerbsPreviousAnalysisDataNegative[carnvHerbsPreviousAnalysisDataNegative$p.adj <0.06,]
permsignificantOldAnalysisDataNegative = carnvHerbsPreviousAnalysisDataNegative[carnvHerbsPreviousAnalysisDataNegative$permp.2sided <0.06,]

dyneinGenes %in% significantOldAnalysisDataNegative$X
dyneinGenes %in% permsignificantOldAnalysisDataNegative$X

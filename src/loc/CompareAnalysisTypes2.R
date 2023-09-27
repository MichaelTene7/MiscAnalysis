library(RERconverge)
library(data.table)
library(ggplot2)
source("Src/Reu/ZonomNameConvertMatrixCommon.R")
source("Src/Reu/ZonomNameConvertVector.R")
source("Src/Reu/ZoonomTreeNameToCommon.R")
CVHCat = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenCarnivore-HerbivorePermulationsCorrelationFile.rds")
CVOCat = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3Phen_Omnivore-CarnivorePermulationsCorrelationFile.rds")
OVHCat = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3Phen_Omnivore-HerbivorePermulationsCorrelationFile.rds")
CVHBin = readRDS("Data/CompareAnalysisTypes/CVHRemakeCorrelationsFilePermulated.rds"); CVHBin = CVHBin[,c(1,3:5)]

CVhBinAtAReadIn = readRDS("Data/CompareAnalysisTypes/BinaryCVHApplesToApplesCorrelationFile.rds")
CVhBinAtAPerms = readRDS("Data/BinaryCVHApplesToApplesCombinedPrunedFastAppendedPermulationsPValue.rds")
CVhBinAtA = cbind(CVhBinAtAReadIn[c(1,3,4)], CVhBinAtAPerms); names(CVhBinAtA)[4] = "permPValue"

CVHTree = readRDS("Data/CompareAnalysisTypes/CVHRemakeBinaryForegroundTree.rds")
Cat3Tree = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenCategoricalTree.rds")

Cat3RER = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenRERFile.rds")
CvhRER = readRDS("Data/CompareAnalysisTypes/CVHRemakeRERFile.rds")

Cat3Path = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenCategoricalPathsFile.rds")
CvhBinPath = readRDS("Data/CompareAnalysisTypes/CVHRemakePathsFile.rds")

BinarySpeciesFilter = readRDS("Data/CompareAnalysisTypes/CVHRemakeSpeciesFilter.rds")
CategoricalSpeciesFilter = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenSpeciesFilter.rds")

Cat3RERCommon = ZonomNameConvertMatrixCommon(Cat3RER)
Cat3PathCommon = ZonomNameConvertVectorCommon(Cat3Path)

CvhBinRERCommon = ZonomNameConvertMatrixCommon(CvhRER)
CvhBinPathCommon = ZonomNameConvertVectorCommon(CvhBinPath)

#----
#binM6PR = CvhRER[2,]
#catM6PR = Cat3RER[2,]


# ---------
CVHBin$DefaultOrder = seq.int(nrow(CVHBin))
CVHBin = CVHBin[order(CVHBin$permPValue),]
CVHBin$permRank = seq.int(nrow(CVHBin))
CVHBin = CVHBin[order(CVHBin$DefaultOrder),]
CVHBin


CVHCat$DefaultOrder = seq.int(nrow(CVHCat))
CVHCat = CVHCat[order(CVHCat$permP),]
CVHCat$permRank = seq.int(nrow(CVHCat))
CVHCat = CVHCat[order(CVHCat$DefaultOrder),]
CVHCat

CVHContIn = read.csv("Data/CompareAnalysisTypes/CVHContinousCorrelations.csv")
CVHCont = CVHContIn
CVHCont$DefaultOrder = match(CVHContIn$Gene, rownames(CVHBin))
CVHContMatch = CVHCont
CVHContMatch[1:16209,1:7] = NA

for(i in 1:length(CVHCont$DefaultOrder)){
  orderIndex = CVHCont$DefaultOrder[i]
  CVHContMatch[orderIndex,] = CVHCont[i,]
}
CVHContMatch$DefaultOrder = seq.int(nrow(CVHContMatch))
CVHContMatch = CVHContMatch[order(CVHContMatch$Permulation.P.value),]
CVHContMatch$ContinousPermRank = seq.int(nrow(CVHContMatch))
CVHContMatch = CVHContMatch[order(CVHContMatch$DefaultOrder),]
CVHContMatch$ContinousPermRank[which(is.na(CVHContMatch$Permulation.P.value))] = NA
# --- Generate Comparison table ----
{
  CompareVals = CVHBin[c(5,4)]
  CompareVals = cbind(CompareVals, CVHCat[4])
  CompareVals = cbind(CompareVals, CVHContMatch[5])
  CompareVals = cbind(CompareVals, CVHBin[6])
  CompareVals = cbind(CompareVals, CVHCat[6])
  CompareVals = cbind(CompareVals, CVHContMatch[c(8)])
  colnames(CompareVals) = c("DefaultOrder", "BinaryPerm", "CatPerm", "ContPerm","BinaryRank", "CatRank","ContRank")
  
  
  binCalculatePercentile = ecdf(CompareVals$BinaryPerm)
  binPercents = sapply(CompareVals$BinaryPerm, binCalculatePercentile)
  
  catCalculatePercentile = ecdf(CompareVals$CatPerm)
  catPercents = sapply(CompareVals$CatPerm, catCalculatePercentile)
  
  contCalculatePercentile = ecdf(CompareVals$ContPerm)
  contPercents = sapply(CompareVals$ContPerm, contCalculatePercentile)
  
  
  binCalculateRankPercentile = ecdf(CompareVals$BinaryRank)
  binRankPercents = sapply(CompareVals$BinaryRank, binCalculateRankPercentile)
  
  catCalculateRankPercentile = ecdf(CompareVals$CatRank)
  catRankPercents = sapply(CompareVals$CatRank, catCalculateRankPercentile)
  
  contCalculateRankPercentile = ecdf(CompareVals$ContRank)
  contRankPercents = sapply(CompareVals$ContRank, contCalculateRankPercentile)
  
  CompareVals = cbind(CompareVals, binPercents)
  CompareVals = cbind(CompareVals, catPercents)
  CompareVals = cbind(CompareVals, contPercents)
  
  CompareVals = cbind(CompareVals, binRankPercents)
  CompareVals = cbind(CompareVals, catRankPercents)
  CompareVals = cbind(CompareVals, contRankPercents)
  colnames(CompareVals) = c("DefaultOrder", "BinaryPerm", "CatPerm", "ContPerm","BinaryRank", "CatRank","ContRank", "BinaryPercent", "CatPercent", "ContPercent", "BinaryRankPercent", "CatRankPercent", "ContRankPercent")
}
CompareVals
colnames(CompareVals) = c("Order", "BinPerm", "CatPerm", "ContPerm","BinRank", "CatRank","ContRank", "BinPercent", "CatPercent", "ContPercent", "BinaryRankPercent", "CatRankPercent", "ContRankPercent")
CompareVals

#make readable version
CompareValsReadable = CompareVals
colnames(CompareValsReadable) = c("Order", "BinPerm", "CatPerm", "ContPerm","BinRank", "CatRank","ContRank", "Bin%", "Cat%", "Cont%", "BinRank%", "CatRank%", "ContRank%")

for(i in c(5,6,7)){
  CompareValsReadable[,i] = sapply(X = CompareValsReadable[,i], FUN = format, digits = 4, scientific = F)
}
for(i in c(8,9,10,11,12,13)){
  CompareValsReadable[,i] = sapply(X = CompareValsReadable[,i], FUN = format, digits = 3, scientific = F)
}
for(i in c(2,3,4)){
  CompareValsReadable[,i] = sapply(X = CompareValsReadable[,i], FUN = format, digits = 3, scientific = T)
}
CompareValsReadable

#write.csv(CompareVals, "Output/threeWayCVHMethodComparision.csv")
#write.csv(CompareValsReadable, "Output/threeWayCVHMethodComparisionCleanNames.csv")

# -----

plot(CompareVals$BinPercent, CompareVals$CatPercent)
plot(CompareVals$BinPercent, CompareVals$ContPercent)
plot(CompareVals$CatPercent, CompareVals$ContPercent)

plot(CompareVals$BinRankPercent, CompareVals$CatRankPercent)
plot(CompareVals$BinRankPercent, CompareVals$ContRanPercent)
plot(CompareVals$CatRankPercent, CompareVals$ContRankPercent)

library(tidyverse)

d2hist = function(x,y){
  ggplot(CompareVals, aes(x=, y=))+
  geom_bin_2d()+
  theme_bw()
}

d2hist = function(x,y){
  ggplot(CompareVals, aes(x=eval(str2lang(x)), y=eval(str2lang(y))))+
    geom_bin_2d()+
    theme_bw()+
    labs(x=x, y=y)
}

d2hist("BinPerm", "CatPerm")
d2hist("BinPerm", "ContPerm")
d2hist("ContPerm", "CatPerm")

d2hist("BinRank", "CatRank")
d2hist("BinRank", "ContRank")
d2hist("ContRank", "CatRank")

d2hist("BinPercent", "CatPercent")
d2hist("BinPercent", "ContPercent")
d2hist("ContPercent", "CatPercent")

d2hist("BinaryRank%", "CatRank%")
d2hist("BinaryRank%", "ContRank%")
d2hist("ContRank%", "CatRank%")


#------------



mainTrees = readRDS("../RunRER/Data/RemadeTreesAllZoonomiaSpecies.rds")


#compare branch RER plots
treePlotRers(mainTrees, rermat = CvhRER, index = "M6PR")
treePlotRers(mainTrees, rermat = Cat3RER, index = "M6PR")

treePlotRers()
returnRersAsTree()

source("Src/Reu/RERConvergeFunctions.R")
returnRERAsTree2 = function (treesObj, rermat, index, phenv = NULL, rer.cex = 0.7, 
          tip.cex = 0.7, nalab = "", plot = T) 
{
  trgene <- treesObj$trees[[index]]
  trgene$edge.length <- rep(2, nrow(trgene$edge))
  ee = edgeIndexRelativeMaster(trgene, treesObj$masterTree)
  ii = treesObj$matIndex[ee[, c(2, 1)]]
  rertree = rermat[index, ii]
  rertree[is.nan(rertree)] = NA
  if (plot) {
    par(mar = c(1, 1, 1, 0))
    edgcols <- rep("black", nrow(trgene$edge))
    edgwds <- rep(1, nrow(trgene$edge))
    if (!is.null(phenv)) {
      edgcols <- rep("black", nrow(trgene$edge))
      edgwds <- rep(1, nrow(trgene$edge))
      edgcols[phenv[ii] == 0] <- "darkgreen"
      edgwds[phenv[ii] == 0] <- 2
      edgcols[phenv[ii] == 1] <- "red"
      edgwds[phenv[ii] == 1] <- 2
      edgcols[phenv[ii] == 4] <- "darkblue"
      edgwds[phenv[ii] == 4] <- 1
    }
    plot.phylo(trgene, font = 2, edge.color = edgcols, edge.width = edgwds, 
               cex = tip.cex)
    rerlab <- round(rertree, 3)
    rerlab[is.na(rerlab)] <- nalab
    edgelabels(rerlab, bg = NULL, adj = c(0.5, 0.9), col = edgcols, 
               frame = "none", cex = rer.cex, font = 2)
  }
  trgene$edge.length <- rertree
  return(trgene)
}
returnRERAsTree2(mainTrees, rermat = CvhRER, index = "M6PR", phenv = CvhBinPath)
returnRERAsTree2(mainTrees, rermat = Cat3RER, index = "M6PR", phenv = Cat3Path)
plotTree(CVHTree)

Cat3PathRecolor = Cat3Path
Cat3PathRecolor[which(Cat3PathRecolor == 1)] =4 
Cat3PathRecolor[which(Cat3PathRecolor == 2)] = 1 
Cat3PathRecolor[which(Cat3PathRecolor == 3)] = 0

returnRERAsTree2(mainTrees, rermat = CvhRER, index = "M6PR", phenv = CvhBinPath)
returnRERAsTree2(mainTrees, rermat = Cat3RER, index = "M6PR", phenv = Cat3PathRecolor)

par(mfrow = c(1,2))
returnRERAsTree2(mainTrees, rermat = CvhRER, index = "M6PR", phenv = CvhBinPath)
returnRERAsTree2(mainTrees, rermat = Cat3RER, index = "M6PR", phenv = Cat3PathRecolor)


ATACVHRER = readRDS("Data/BinaryCVHApplesToApplesRERFile.rds")
ATACVHPaths = readRDS("Data/BinaryCVHApplesToApplesBinaryPathsFile.rds")
par(mfrow = c(1,3))
returnRERAsTree2(mainTrees, rermat = CvhRER, index = "M6PR", phenv = CvhBinPath)
returnRERAsTree2(mainTrees, rermat = ATACVHRER, index = "M6PR", phenv = ATACVHPaths)
returnRERAsTree2(mainTrees, rermat = Cat3RER, index = "M6PR", phenv = Cat3PathRecolor)


Cat3PathCompare = Cat3PathRecolor
Cat3PathCompare[which(Cat3PathRecolor == 4)] = NA 
Cat3PathCompare

Cat3PathCompare[which(!Cat3PathCompare == ATACVHPaths)]
ATACVHPaths

par(mfrow = c(1,2))
returnRERAsTree2(mainTrees, rermat = Cat3RER, index = "M6PR", phenv = ATACVHPaths)
returnRERAsTree2(mainTrees, rermat = Cat3RER, index = "M6PR", phenv = Cat3PathRecolor)

#
BinarySpeciesFilter = readRDS("Data/CompareAnalysisTypes/CVHRemakeSpeciesFilter.rds")
CategoricalSpeciesFilter = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenSpeciesFilter.rds")
ATASpeciesFilter = readRDS("Data/BinaryCVHApplesToApplesSpeciesFilter.rds")

all.equal(BinarySpeciesFilter, CategoricalSpeciesFilter)
all.equal(ATASpeciesFilter, CategoricalSpeciesFilter)


# --- 

topVals = function(col, number){
  CompareVals[[col]] %in% head(CompareVals[[col]][order(CompareVals[[col]])],number)
}

cutOff = function(col, number){
  CompareVals[[col]] < number
}
topVals("BinPerm", 250)
cutOff("BinPerm", 0.05)

CompareValsReadable[which(topVals("BinPerm", 500) & topVals("CatPerm", 500) & topVals("ContPerm", 500)),]
rownames(CompareValsReadable[which(topVals("BinPerm", 500) & topVals("CatPerm", 500) & topVals("ContPerm", 500)),])
CompareValsReadable[which(cutOff("BinPerm", 0.05) & cutOff("CatPerm", 0.05) & cutOff("ContPerm", 0.05)),]
dput(rownames(CompareValsReadable[which(cutOff("BinPerm", 0.05) & cutOff("CatPerm", 0.05) & cutOff("ContPerm", 0.05)),]))

# compare narrow binary, wide binary, and categroical 

CompareWidth = data.frame(CVHBin$p.adj, CVhBinAtA$p.adj, CVHCat$p.adj)
colnames(CompareWidth) = c("NarrowBinary", "WideBinary", "Categorical")

x = "NarrowBinary"
y = "WideBinary"

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

r2plot = function(x,y, dataframe = CompareWidth){
  plot(dataframe[[str2lang(x)]], dataframe[[str2lang(y)]], xlab = str2lang(x), ylab = str2lang(y))
  model = lm(eval(str2lang(x)) ~ eval(str2lang(y)), dataframe)
  abline(model) 
  legend("topleft",legend=paste("R2 is", format(summary(model)$r.squared,digits=3)))
}

d2hist2 = function(x,y){
  ggplot(CompareWidth, aes(x=eval(str2lang(x)), y=eval(str2lang(y))))+
    geom_bin_2d()+
    theme_bw()+
    labs(x=x, y=y)
}


CompareWidth$Categorical[which(CompareWidth$Categorical == 1)] = NA

plot(CompareWidth$NarrowBinary, CompareWidth$WideBinary )
plot(CompareWidth$Categorical, CompareWidth$WideBinary )
plot(CompareWidth$Categorical, CompareWidth$NarrowBinary )

r2plot("NarrowBinary", "WideBinary")
r2plot("NarrowBinary", "Categorical")
r2plot("WideBinary", "Categorical")



d2hist2("NarrowBinary", "WideBinary")
d2hist2("NarrowBinary", "Categorical")
d2hist2("WideBinary", "Categorical")


# -- Compare width permulated --

CompareWidthPerm = data.frame(CVHBin$permPValue, CVhBinAtA$permPValue, CVHCat$permP)
colnames(CompareWidthPerm) = c("NarrowBinary", "WideBinary", "Categorical")

x = "NarrowBinary"
y = "WideBinary"

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

r2plotPerm = function(x,y, dataframe = CompareWidthPerm){
  plot(dataframe[[str2lang(x)]], dataframe[[str2lang(y)]], xlab = str2lang(x), ylab = str2lang(y))
  model = lm(eval(str2lang(x)) ~ eval(str2lang(y)), dataframe)
  abline(model) 
  legend("topleft",legend=paste("R2 is", format(summary(model)$r.squared,digits=3)))
}

d2hist2Perm = function(x,y){
  ggplot(CompareWidthPerm, aes(x=eval(str2lang(x)), y=eval(str2lang(y))))+
    geom_bin_2d()+
    theme_bw()+
    labs(x=x, y=y)
}


#CompareWidth$Categorical[which(CompareWidth$Categorical == 1)] = NA

all.equal(rownames(CVHBin), rownames(CVHCat))
all.equal(rownames(CVHBin), rownames(CVhBinAtA))
plot(CompareWidth$WideBinary, CompareWidthPerm$WideBinary )


plot(CompareWidthPerm$NarrowBinary, CompareWidthPerm$WideBinary )
plot(CompareWidthPerm$Categorical, CompareWidthPerm$WideBinary )
plot(CompareWidthPerm$Categorical, CompareWidthPerm$NarrowBinary )

r2plotPerm("NarrowBinary", "WideBinary")
r2plotPerm("NarrowBinary", "Categorical")
r2plotPerm("WideBinary", "Categorical")



d2hist2Perm("NarrowBinary", "WideBinary")
d2hist2Perm("NarrowBinary", "Categorical")
d2hist2Perm("WideBinary", "Categorical")







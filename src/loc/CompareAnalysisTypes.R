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

CVHTree = readRDS("Data/CompareAnalysisTypes/CVHRemakeBinaryForegroundTree.rds")
Cat3Tree = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenCategoricalTree.rds")

CVHTree$tip.label

CVHTree$tip.label[which(!CVHTree$tip.label %in% Cat3Tree$tip.label)]



head(CVHBin)
head(CVHCat)
head(OVHCat)
head(CVOCat)

all.equal(CVHBin, CVHCat)
?all.equal()

Cat3RER = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenRERFile.rds")
Cat3Path = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenCategoricalPathsFile.rds")
CvhRER = readRDS("Data/CompareAnalysisTypes/CVHRemakeRERFile.rds")
CvhBinPath = readRDS("Data/CompareAnalysisTypes/CVHRemakePathsFile.rds")

plotRers(CvhRER, "M6PR", CvhBinPath)
plotRers(Cat3RER, "M6PR", Cat3Path)
?plotRers()

Cat3RERCommon = ZonomNameConvertMatrixCommon(Cat3RER)
Cat3PathCommon = ZonomNameConvertVectorCommon(Cat3Path)

CvhBinRERCommon = ZonomNameConvertMatrixCommon(CvhRER)
CvhBinPathCommon = ZonomNameConvertVectorCommon(CvhBinPath)


plotRers(Cat3RERCommon, "M6PR", Cat3Path)
plotRers(CvhBinRERCommon, "M6PR", CvhBinPathCommon)

which(colnames(Cat3RERCommon) %in% "Bottle-nose Dolphin")
bndVal= Cat3RERCommon[,1283:1296]
all(is.na(bndVal))

which(colnames(CvhBinRERCommon) %in% "Bottle-nose Dolphin")
bndBinVal= CvhBinRERCommon[,1283:1296]
all(is.na(bndBinVal))

binM6PR = CvhBinRERCommon[2,]
catM6PR = Cat3RERCommon[2,]
all.equal(rownames(CVHBin), rownames(CVHCat))
plot(binM6PR, catM6PR, xlim = c(-3,4), ylim = c(-3,4))
abline(0,1)
abline(1,1)
abline(0.5,1)
trnedline = lm(binM6PR ~ catM6PR)
all.equal(names(binM6PR), names(catM6PR))

which(rownames(CvhBinRERCommon) == "M6PR")
which(!is.na(binM6PR))
which(!is.na(catM6PR))

all.equal(rownames(Cat3RER), rownames(CvhRER))

BvC = data.frame(binM6PR, catM6PR)
rownames(BvC) = names(binM6PR)

ggplot(BvC, aes(binM6PR, catM6PR)) + geom_point()



# ----------
Cat3RER = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenRERFile.rds")
CvhRER = readRDS("Data/CompareAnalysisTypes/CVHRemakeRERFile.rds")

all.equal(colnames(Cat3RER), colnames(CvhRER))
# -- This returns true. 
  # -- This may be because the mulitphylo is the same, even if the species included in the species filter in the RER calculations differ?
which(rownames(Cat3RER) == "M6PR")
# -- This returns 2
binM6PR = CvhRER[2,]
catM6PR = Cat3RER[2,]

plot(binM6PR, catM6PR, xlim = c(-3,4), ylim = c(-3,4))

binaryFilledValues = which(!is.na(binM6PR)); names(binaryFilledValues) = NULL
binaryFilledValues
categoricalFilledValues = which(!is.na(catM6PR)); names(categoricalFilledValues) = NULL
categoricalFilledValues
length(binaryFilledValues)
length(categoricalFilledValues)
binaryFilledValues %in% categoricalFilledValues
categoricalFilledValues %in% binaryFilledValues

binaryFilledValues = which(!is.na(binM6PR));
categoricalFilledValues = which(!is.na(catM6PR));

binaryFilledValues[which(!binaryFilledValues %in% categoricalFilledValues)]
grep("orca", names(binaryFilledValues))

# -- 
BinarySpeciesFilter = readRDS("Data/CompareAnalysisTypes/CVHRemakeSpeciesFilter.rds")
CategoricalSpeciesFilter = readRDS("Data/CompareAnalysisTypes/CategoricalDiet3PhenSpeciesFilter.rds")

all.equal(BinarySpeciesFilter, CategoricalSpeciesFilter)

# ---- 

binFKBP4= CvhRER[3,]
catFKBP4 = Cat3RER[3,]

plot(binFKBP4, catFKBP4, xlim = c(-3,4), ylim = c(-3,4))
abline(0,1)


grep("gor", BinarySpeciesFilter)
grep("micOch", BinarySpeciesFilter)
grep("Ruf1", BinarySpeciesFilter)


CVHBinByPerm = CVHBin[order(CVHBin$permPValue),]
CVHCatByPerm = CVHCat[order(CVHCat$permP),]

head(CVHBinByPerm)
head(CVHCatByPerm)

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


CompareVals = CVHBin[c(6,4)]
CompareVals = cbind(CompareVals, CVHCat[4])
CompareVals = cbind(CompareVals, CVHBin[5])
CompareVals = cbind(CompareVals, CVHCat[6])
colnames(CompareVals) = c("DefaultOrder", "BinaryPerm", "CatPerm", "BinaryRank", "CatRank")
CompareVals

plot(CompareVals$BinaryPerm, CompareVals$CatPerm)
PermLine = lm(CompareVals$BinaryPerm ~ CompareVals$CatPerm)
summary(PermLine)


plot(CompareVals$BinaryRank, CompareVals$CatRank)
rankLine = lm(CompareVals$BinaryRank ~ CompareVals$CatRank)
summary(rankLine)


CVHContIn = read.csv("Data/CompareAnalysisTypes/CVHContinousCorrelations.csv")
match(CVHContIn$Gene, rownames(CompareVals))
CVHCont = CVHContIn
CVHCont$DefaultOrder = match(CVHContIn$Gene, rownames(CompareVals))
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

CompareVals = cbind(CompareVals, CVHContMatch[c(5,8)])
colnames(CompareVals) = c("DefaultOrder", "BinaryPerm", "CatPerm", "BinaryRank", "CatRank", "ContPerm", "ContRank")

plot(CompareVals$BinaryPerm, CompareVals$ContPerm)
plot(CompareVals$BinaryRank, CompareVals$ContRank)

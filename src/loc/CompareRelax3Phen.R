relaxData = readRDS("Data/CategoricalDiet3PhenPermulationsPValueCorrelations.rds")
noRelaxData = readRDS("Data/CategoricalDiet3PhenPermulationsPValueCorrelationsNoRelax.rds")

centralRelaxData = relaxData[1]
centralRelaxData = centralRelaxData[[1]]
centralNoRelaxData = noRelaxData[[1]]

setRelaxData = relaxData[[2]]
setNoRelaxData = noRelaxData[[2]]

OCRelaxData = setRelaxData[1]
OCRelaxData = OCRelaxData[[1]]
OCNoRelaxData = setNoRelaxData[1]
OCNoRelaxData = OCNoRelaxData[[1]]

OHRelaxData = setRelaxData[2]
OHRelaxData = OHRelaxData[[1]]
OHNoRelaxData = setNoRelaxData[2]
OHNoRelaxData = OHNoRelaxData[[1]]

CHRelaxData = setRelaxData[3]
CHRelaxData = CHRelaxData[[1]]
CHNoRelaxData = setNoRelaxData[3]
CHNoRelaxData = CHNoRelaxData[[1]]

head(centralRelaxData)
head(centralNoRelaxData)

all.equal(centralRelaxData$permP, centralNoRelaxData$permP)
all.equal(OCRelaxData$permP, OCNoRelaxData$permP)
all.equal(OHRelaxData$permP, OHNoRelaxData$permP)
all.equal(CHRelaxData$permP, CHNoRelaxData$permP)



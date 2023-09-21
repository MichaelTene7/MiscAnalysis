newRER = readRDS("Data/carnvHerbsRERFile.rds")
oldRER = readRDS("Data/mamRERCMU_FishCarn.rds")
midRER = readRDS("Data/carnvHerbsRERFile12-15-22.rds")

all.equal(newRER, oldRER)

length(which(!is.na(newRER)))/length(newRER)
length(which(!is.na(midRER)))/length(midRER)
length(which(!is.na(oldRER)))/length(oldRER)

all.equal(midRER, oldRER)

someRER = readRDS("Data/carnvHerbRERFileNewGeneration.rds")
minsp35RER = readRDS("Data/carnvHerbRERFileMinSp35.rds")

all.equal(someRER, minsp35RER)


carnvHerbsPreviousAnalysisData


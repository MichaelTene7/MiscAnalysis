catGoData = readRDS("Data/CategoricalDiet3PhenCarnivore-HerbivoreEnrichment-GO_Biological_Process_2023.rds")
binGoData = readRDS("Data/CVHRemakeEnrichment-GO_Biological_Process_2023.rds")

binGoData = binGoData[[1]]
catGoData = catGoData[[1]]

topBinSets = head(binGoData[order(binGoData$p.adj),], 50)
topCatSets = head(catGoData[order(catGoData$p.adj),], 50)

rownames(topBinSets)
rownames(topCatSets)

rownames(topCatSets)[rownames(topCatSets) %in% rownames(topBinSets)]

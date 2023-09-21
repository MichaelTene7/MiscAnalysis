library(RERconverge)

#These files were generated using a different script, based on the permulation p values. The p.adj displayed is the MHT correction of the p value of the geneset detection. 
tissuesResults = readRDS("Data/CVHRemakeEnrichment-tissue_specific.rds")
pathwaysResults = readRDS("Data/CVHRemakeEnrichment-GSEA-c5-HsSymbols.rds")

tissue_specific = tissuesResults[[1]]

pathways = pathwaysResults[[1]]


tissue_specific = tissue_specific[order(tissue_specific$pval), ]
#Remove EBV.Lymphoma (cell line), and Testis (p val inflated due to geneset size)
tissue_specific = tissue_specific[-2,]
tissue_specific = tissue_specific[-1,]


removePosition = function(vector){
  sub(":.*", "", vector)
}
convertGeneValsToNames = function(string){
  genestring = string
  genestring = strsplit(genestring, ",")
  genestringNames = lapply(genestring, removePosition)
  genestringNames = genestringNames[[1]]
}
calculateTissueiness = function(list){
  geneNameList = list
  tissueGenesInSet = grep(paste(tissueGenes, collapse ="|"), geneNameList)
  tissueiness = length(tissueGenesInSet) / length(geneNameList)
  tissueiness
}

#get liver genenames 
liverGenes = tissue_specific$gene.vals[1]
liverGenesNames = convertGeneValsToNames(liverGenes)
liverGenesNames= liverGenesNames[1:100]

#get thyroid genes
thyroidGenes = tissue_specific$gene.vals[2]
thyroidGenesNames = convertGeneValsToNames(thyroidGenes)
thyroidGenesNames= thyroidGenesNames[1:60]

#get cerebral genes 
cerebralGenes = tissue_specific$gene.vals[3]
cerebralGenesNames = convertGeneValsToNames(cerebralGenes)
cerebralGenesNames= cerebralGenesNames[1:100]

#get muscle genes 
muscleGenes = tissue_specific$gene.vals[4]
muscleGenesNames = convertGeneValsToNames(muscleGenes)
muscleGenesNames= muscleGenesNames[1:92]

#get heart names
heartGenes = tissue_specific$gene.vals[5]
heartGenesNames = convertGeneValsToNames(muscleGenes)
heartGenesNames= heartGenesNames[1:30]


#setup pathways 
pathwaysGeneNames = pathways
pathwaysGeneNames$gene.vals = lapply(pathwaysGeneNames$gene.vals, convertGeneValsToNames)

tissueGenes = liverGenesNames
liveriness = lapply(pathwaysGeneNames$gene.vals, calculateTissueiness)
liveriness = unlist(liveriness)
pathwaysGeneNames$liveriness = liveriness

tissueGenes = thyroidGenesNames
thyroidiness = lapply(pathwaysGeneNames$gene.vals, calculateTissueiness)
thyroidiness = unlist(thyroidiness)
pathwaysGeneNames$thyroidiness = thyroidiness

tissueGenes = cerebralGenesNames
cerebraliness = lapply(pathwaysGeneNames$gene.vals, calculateTissueiness)
cerebraliness = unlist(cerebraliness)
pathwaysGeneNames$cerebraliness = cerebraliness

tissueGenes = muscleGenesNames
muscleiness = lapply(pathwaysGeneNames$gene.vals, calculateTissueiness)
muscleiness = unlist(muscleiness)
pathwaysGeneNames$muscleiness = muscleiness

tissueGenes = heartGenesNames
heartiness = lapply(pathwaysGeneNames$gene.vals, calculateTissueiness)
heartiness = unlist(heartiness)
pathwaysGeneNames$heartiness = heartiness



#

pathwaysGeneNames = pathwaysGeneNames[order(abs(pathwaysGeneNames$stat), decreasing = T),]
AAMetabolismPathways = pathwaysGeneNames
AAMetabolismPathways = AAMetabolismPathways[c(4, 5, 7, 11),]
pathwaysGeneNames = pathwaysGeneNames[order(abs(pathwaysGeneNames$p.adj)),]
AAMetabolismPathways = rbind(AAMetabolismPathways, pathwaysGeneNames[c(9, 14, 17, 18),])
#pathwaysGeneNames = pathwaysGeneNames[order(abs(pathwaysGeneNames$liveriness), decreasing = T),]

#


thyroidiPathways = pathwaysGeneNames[which(!pathwaysGeneNames$thyroidiness == 0), ]
thyroidiPathways = thyroidiPathways[order(thyroidiPathways$thyroidiness, decreasing = T), ]
thyroidiPathways = thyroidiPathways[order(abs(thyroidiPathways$stat), decreasing = T), ]

liveriPathways = pathwaysGeneNames[which(!pathwaysGeneNames$liveriness == 0), ]
liveriPathways = liveriPathways[order(liveriPathways$liveriness, decreasing = T), ]

cerebraliPathways = pathwaysGeneNames[which(!pathwaysGeneNames$cerebraliness == 0), ]
cerebraliPathways = cerebraliPathways[order(cerebraliPathways$cerebraliness, decreasing = T), ]
cerebraliPathways = cerebraliPathways[order(cerebraliPathways$p.adj), ]
cerebraliPathways = cerebraliPathways[order(abs(cerebraliPathways$stat), decreasing = T), ]

muscleiPathways = pathwaysGeneNames[which(!pathwaysGeneNames$muscleiness == 0), ]
muscleiPathways = muscleiPathways[order(muscleiPathways$muscleiness, decreasing = T), ]
muscleiPathways = muscleiPathways[order(muscleiPathways$p.adj), ]
muscleiPathways = muscleiPathways[order(abs(muscleiPathways$stat), decreasing = T), ]

heartiPathways = pathwaysGeneNames[which(!pathwaysGeneNames$heartiness == 0), ]
heartiPathways = heartiPathways[order(heartiPathways$heartiness, decreasing = T), ]

#


which(rownames(pathways) %in% "HP_FALCIFORM_RETINAL_FOLD")
pathways[which(rownames(pathways) %in% "HP_FALCIFORM_RETINAL_FOLD"), ]

grep("RETIN", rownames(pathways))
retinalPathways = pathways[grep("RETIN", rownames(pathways)),]
retinalPathways = retinalPathways[order(retinalPathways$stat), ]



tissue_significant = tissue_specific[which(tissue_specific$p.adj < 0.05),]

liverGenes = tissue_specific$gene.vals[1]
liverGenes = strsplit(liverGenes, ",")
liverGenesNames = lapply(liverGenes, removePosition)
liverGenesNames = liverGenesNames[[1]]
liverGenesNames= liverGenesNames[1:100]

removePosition = function(vector){
  sub(":.*", "", vector)
}
convertGeneValsToNames = function(string){
  genestring = string
  genestring = strsplit(genestring, ",")
  genestringNames = lapply(genestring, removePosition)
  genestringNames = genestringNames[[1]]
}
calculateTissueiness = function(list){
  geneNameList = list
  tissueGenesInSet = grep(paste(tissueGenes, collapse ="|"), geneNameList)
  tissueiness = length(tissueGenesInSet) / length(geneNameList)
  tissueiness
}

GenesetsWLiverGenes = pathways[grep(paste(liverGenesNames, collapse ="|"), pathways$gene.vals),]
GenesetsWLiverGenes = GenesetsWLiverGenes[order(GenesetsWLiverGenes$p.adj),]

GenesetsWLiverGenes$gene.vals = lapply(GenesetsWLiverGenes$gene.vals, convertGeneValsToNames)




GenesetsWLiverGenes$gene.vals[1]
GenesetsWLiverGenes$gene.vals[[1]] = append(GenesetsWLiverGenes$gene.vals[[1]], " TAT")


tissueGenes = liverGenesNames
tissuiness = lapply(GenesetsWLiverGenes$gene.vals, calculateTissueiness)
?lapply
tissuiness = unlist(tissuiness)

GenesetsWLiverGenes$liveriness = tissuiness

GenesetsWLiverGenes = GenesetsWLiverGenes[order(GenesetsWLiverGenes$liveriness, decreasing = T),]
GenesetsWLiverGenes = GenesetsWLiverGenes[order(abs(GenesetsWLiverGenes$stat), decreasing = T),]


GenesetsWLiverGenesSignificant = GenesetsWLiverGenes[which(GenesetsWLiverGenes$p.adj < 0.05),]

#######
muscleiPathwaysStrict = muscleiPathways[which(muscleiPathways$muscleiness >0.05),]
muscleiPathwaysStrict = muscleiPathwaysStrict[order(muscleiPathwaysStrict$muscleiness, decreasing = T), ]
muscleiPathwaysStrict = muscleiPathwaysStrict[order(muscleiPathwaysStrict$p.adj), ]
MuscleDevelopmentPathways = muscleiPathwaysStrict[c(2,4,5,6,8),]
muscleiPathwaysStrict = muscleiPathwaysStrict[order(abs(muscleiPathwaysStrict$stat), decreasing = T), ]
MuscleDevelopmentPathways = rbind(MuscleDevelopmentPathways, muscleiPathwaysStrict[c(5,8,10,15,16,18,21),])



######

AAMetabolismPathwaysPretty = AAMetabolismPathways

AAMetabolismPathwaysPretty = AAMetabolismPathwaysPretty[,1:5]
rownames(AAMetabolismPathwaysPretty)[1] = "Branched Chain Amino Acid Biosynthesis"
rownames(AAMetabolismPathwaysPretty)[2] = "Regulation of Tyrosine Phosphatases"
#rownames(AAMetabolismPathwaysPretty)[3] = "Cystine Metabolism"
#rownames(AAMetabolismPathwaysPretty)[4] = "Leukotrine Catabolism"

AAMetabolismPathwaysPretty[2,] = AAMetabolismPathwaysPretty[3,]
rownames(AAMetabolismPathwaysPretty)[2] = "Cystine Metabolism"
AAMetabolismPathwaysPretty[3,] = AAMetabolismPathwaysPretty[4,]
rownames(AAMetabolismPathwaysPretty)[3] = "Leukotrine Catabolism"
AAMetabolismPathwaysPretty[4,] = ""
rownames(AAMetabolismPathwaysPretty)[4] = "   "


rownames(AAMetabolismPathwaysPretty)[5] = "OrganoNitrogen Compound Catabolism"
rownames(AAMetabolismPathwaysPretty)[6] = "Lipid Metabolism"
rownames(AAMetabolismPathwaysPretty)[7] = "Alpha Amino Acid Metabolism"
rownames(AAMetabolismPathwaysPretty)[8] = "Organic Acid Catabolism"


AAMetabolismPathwaysPretty[,1] = signif(as.numeric(AAMetabolismPathwaysPretty[,1]), 5)
AAMetabolismPathwaysPretty[,2] = signif(as.numeric(AAMetabolismPathwaysPretty[,2]), 5)
AAMetabolismPathwaysPretty[,3] = signif(as.numeric(AAMetabolismPathwaysPretty[,3]), 5)
AAMetabolismPathwaysPretty[,4] = signif(as.numeric(AAMetabolismPathwaysPretty[,4]), 5)
AAMetabolismPathwaysPretty[4,] = ""

names(AAMetabolismPathwaysPretty) = c("Stat", "P-value", "P-value (MHT Corrected)", "Gene Number")
AAMetabolismPathwaysPrettyFlipped = AAMetabolismPathwaysPretty[c(5,6,7,8,4,1,2,3),]

#
MuscleFunctionPathwaysPretty = MuscleDevelopmentPathways
MuscleFunctionPathwaysPretty = MuscleFunctionPathwaysPretty[,1:5]

divider = c("","","","","")
MuscleFunctionPathwaysPretty= rbind(MuscleFunctionPathwaysPretty[1:5,], divider, MuscleFunctionPathwaysPretty[c(6,8,9),], divider, MuscleFunctionPathwaysPretty[c(7, 11, 12),])
rownames(MuscleFunctionPathwaysPretty) = c(
  "Muscle Contraction",
  "Cytoskeletal Motor Activity",
  "Straited Muscle Contraction",
  "Contractile Fiber", 
  "Muscle System Process", 
  "   ",
  "Skeletal Muscle Thin Filament Assembly",
  "Voluntary Skeletal Muscle Contraction",
  "Cardiac Muscle Membrane Repolarization",
  "     ",
  "Hip flexor Weakness",
  "Ankle Weakness",
  "Long Finger Extensor Weakness"
)
names(MuscleFunctionPathwaysPretty) = c("Stat", "P-value", "P-value (MHT Corrected)", "Gene Number")
MuscleFunctionPathwaysPretty[,1] = signif(as.numeric(MuscleFunctionPathwaysPretty[,1]), 5)
MuscleFunctionPathwaysPretty[,2] = signif(as.numeric(MuscleFunctionPathwaysPretty[,2]), 5)
MuscleFunctionPathwaysPretty[,3] = signif(as.numeric(MuscleFunctionPathwaysPretty[,3]), 5)
MuscleFunctionPathwaysPretty[,4] = signif(as.numeric(MuscleFunctionPathwaysPretty[,4]), 5)
MuscleFunctionPathwaysPretty[c(6,10),] = ""


#
tissueSignificantPretty = tissue_significant
rownames(tissueSignificantPretty) = c("Liver", "Thyroid", "Cerebellar Hemisphere", "Skeletal Muscle", "Left Ventricle")
names(tissueSignificantPretty) = c("Stat", "P-value", "P-value (MHT Corrected)", "Gene Number")
tissueSignificantPretty[,1] = signif(as.numeric(tissueSignificantPretty[,1]), 5)
tissueSignificantPretty[,2] = signif(as.numeric(tissueSignificantPretty[,2]), 5)
tissueSignificantPretty[,3] = signif(as.numeric(tissueSignificantPretty[,3]), 5)
tissueSignificantPretty[,4] = signif(as.numeric(tissueSignificantPretty[,4]), 5)

tissueSignificantPretty = tissueSignificantPretty[,1:4]

tissueSignificantPrettyLIver = tissueSignificantPretty[1,]
tissueSignificantPrettyMuscle = tissueSignificantPretty[c(4,5),]

##

AAPathwaysWithGeneStrength = pathways[which(rownames(pathways) %in% rownames(AAMetabolismPathways)),]
MusclePathwaysWithGeneStrength = pathways[which(rownames(pathways) %in% rownames(MuscleDevelopmentPathways)),]
http://127.0.0.1:33747/graphics/plot_zoom_png?width=601&height=519
##

RERs = readRDS("Data/CVHRemakeRERFile.rds")
phenotypeVector = readRDS("Data/CVHRemakeBinaryTreeForegroundSpecies.rds")
plotRers(RERs, "PRODH", phenotypeVector)
source("Src/Reu/rerViolinPlot.R")





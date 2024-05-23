library(RERconverge)


mainData = readRDS("Data/DemoData.rds")
masterTree = mainData$masterTree


plotTree(masterTree)


phenotypeTree = click_select_foreground_branches(masterTree)

plotTree(phenotypeTree) 

plotTreeHighlightBranches(masterTree, hlspecies = which(phenotypeTree$edge.length==1), hlcols = "blue")


saveRDS(phenotypeTree, "Output/PhenotypeTreeExample.rds")                        

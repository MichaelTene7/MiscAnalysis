library(RERconverge)

hiller4PhenTree = readRDS("../RunRER/Output/NewHiller4Phen/NewHiller4PhenCategoricalTree.rds")
write.tree(CVHTree, "Results/NewHiller4PhenTweaks/NewHiller4PhenCategoricalTree.tree")
plotTree(hiller4PhenTree)


plotTreHi
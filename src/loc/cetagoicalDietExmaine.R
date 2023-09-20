cors = readRDS("Data/dietCors.rds")

names(cors[[2]])

categoryNames = c(1,2,3)
names(categoryNames) = c("Carnivore", "Generalist", "Herbivore")

pairwiseCategorical = cors[[2]]

pairwiseTableNames = names(pairwiseCategorical)
for(i in 1:length(categoryNames)){
  pairwiseTableNames = gsub(i, names(categoryNames)[i], pairwiseTableNames)
}
names(pairwiseCategorical) = pairwiseTableNames

names(pairwiseCategorical)

library(devtools)
install_github("nclark-lab/RERconverge", ref = "New_Functions_For_Categorical_Traits")


perms = readRDS("Data/dietPerms.rds")
library(RERconverge)
phenvec = readRDS("Data/dietPhens.rds")
phenvec <- phenvec[!is.na(phenvec)]
trees = readRDS("Data/mam120aa_trees.rds")
permTrees = categoricalPermulations(trees, phenvec, rm = "SYM", ntrees = 10, extantOnly = FALSE)

plotTreeCategorical()
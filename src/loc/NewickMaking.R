library(RERconverge)

madeTree = read.tree("Data/MadeNewick.txt")
plot(madeTree)

masterTree = read.tree("Data/hypotheticalMasterTree.txt")
geneTree = read.tree("Data/hypotheticalGeneTree.txt")
plot(geneTree)

png("HypotheticalMasterTree.png")
plotTreeHighlightBranches(masterTree, hlspecies = "Chihuahua", main = "Overall Genome Similarity")
dev.off()
png("hypotheticalGeneTree.png")
plotTreeHighlightBranches(geneTree, hlspecies = "Chihuahua", main = "Hypothetical Size Gene")
dev.off()

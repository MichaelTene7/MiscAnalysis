library(RERconverge)

nexusTree= read.nexus("Data/UphamDNAOnlyNodeBasedTree.tre")
write.tree(nexusTree, "Data/UphamDNAOnlyNodeBasedTree.nwk")

testTree =read.tree("Data/UphamDNAOnlyNodeBasedTree.nwk")

unroot(testTree)
plotTree(testTree)
testTree$tip.label[1]

dataSet$DebugNewMatchingName[i] = sub('^([^_]+_[^_]+).*', '\\1', dataSet$DebugNewMatchingName[i]) #remove anything  after a second underscore


uphamTips = data.frame(testTree$tip.label)
uphamTips$scientific = sub('^([^_]+_[^_]+).*', '\\1', uphamTips$testTree.tip.label)
names(uphamTips)[1] = "uphamName"
write.csv(uphamTips, "Results/UphmanNameConversion.csv")

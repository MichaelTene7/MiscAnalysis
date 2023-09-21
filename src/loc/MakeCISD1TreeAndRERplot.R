#Goal: Plot species tree for CISD1, highlighting phenotype evolution
#Plot distribution of RER for foreground and background branches (violin + strip)
mydir <- "/Volumes/GoogleDrive/My Drive/MeyerLabLehigh/MammalDiet/Zoonomia/PlotRERs/Laura"
setwd(mydir)

if (!require(phangorn)) {
  install.packages("phangorn") #this is a package that has helpful functions
  #for reading the alignment
}
library(phangorn)
if (!require(phytools)) {
  install.packages("phytools") #this is a package that has helpful functions
  #for reading the tree
}
library(phytools)
if (!require(RERconverge)) {
  library(devtools)
  install_github("nclark-lab/RERconverge")
}
library(RERconverge)

mamrer <- readRDS("mamRERCMU_Laura.rds")
mamtrees <- readRDS("Phylotrees_Laura_BoreoObj.rds")
phentree <- read.tree("Culled_LauraTree_Phen.txt")
cisd1tree <- mamtrees$trees$CISD1
length(intersect(cisd1tree$tip.label, phentree$tip.label))
length(phentree$tip.label)
pphentree <- drop.tip(phentree, setdiff(phentree$tip.label,cisd1tree$tip.label))
cisd1tree <- drop.tip(cisd1tree,setdiff(cisd1tree$tip.label, pphentree$tip.label))
#check whether tree order is the same
par(mfrow=c(1,2))
plot(cisd1tree, cex=0.5)
plot(pphentree,use.edge.length=F,cex=0.5)
sum(pphentree$tip.label == cisd1tree$tip.label)
length(pphentree$tip.label)
length(cisd1tree$tip.label)
#Try reordering
#Very helpful: https://stackoverflow.com/questions/62756096/is-there-a-way-to-reorder-tip-labels-of-a-phylo-object-so-that-they-are-consiste
trees <- c(cisd1tree, pphentree) 
trees <- .compressTipLabel(trees)
pphentree <- trees[[2]]
all.equal(cisd1tree$tip.label, pphentree$tip.label)
par(mfrow=c(1,2))
plot(cisd1tree, cex=0.5)
plot(pphentree,use.edge.length=F,cex=0.5)
#Hooray, this works!
#First, convert the names
spnamefile <- "MA_Laura.csv" 
#This is from the Manual Annotations spreadsheet
spnames <- read.csv(spnamefile)
ccisd1tree <- cisd1tree
for (i in c(1:length(ccisd1tree$tip.label))) {
  whichrow <- match(ccisd1tree$tip.label[i], spnames$FaName) #finds the row containing the FaName
  if (length(whichrow) == 1){
    #Replace the name with the appropriate common name, using whichrow
    ccisd1tree$tip.label[i] <- spnames$Common.Name.or.Group[whichrow]
  } else {
    print(paste("Issue with",ccisd1tree$tip.label[i]," - found",length(whichrow),"times"))
  }
}
mybranchcols <- c(rep("darkgreen",length(ccisd1tree$edge.length)))
mybranchcols[which(pphentree$edge.length > 0)] <- "lightsalmon"
#Make the zero branches non-zero (or prune them entirely?)
mblength <- min(ccisd1tree$edge.length[ccisd1tree$edge.length>0.001])
ccisd1tree$edge.length[ccisd1tree$edge.length < 0.001] <- mblength/5
pdf("../CISD1TreeColoredByPhenotype.pdf", width=6, height=6)
par(mar=c(0.5,0.5,.5,.5))
plot.phylo(ccisd1tree, edge.color=mybranchcols, cex=0.5,
           type="tidy")
legend("bottomleft", legend = c("carnivore", "herbivore"),
       lty=c(1,1), col=c("lightsalmon","darkgreen"), bty="n",
       cex=0.5)
dev.off()

#Plot RERs for herbivores and carnivores separately
phenvdiet <- tree2Paths(phentree, mamtrees)
rertree <- returnRersAsTree(mamtrees,mamrer,index="CISD1",phenv=phenvdiet)
#again do the reordering with the phenotype tree to find foreground and background branches
pphentree2 <- drop.tip(phentree, setdiff(phentree$tip.label, rertree$tip.label))
prertree <- drop.tip(rertree,setdiff(rertree$tip.label,pphentree2$tip.label))
trees2 <- c(prertree, pphentree2) 
trees2 <- .compressTipLabel(trees2)
pphentree2 <- trees2[[2]]
all.equal(prertree$tip.label, pphentree2$tip.label)
sum(!is.na(prertree$edge.length))
sum(!is.na(rertree$edge.length))
#Blerg, this eliminates *many* edges
#Try looking into returnRersAsTree bc it also produces colors for foreground/background
trgene <- mamtrees$trees[["CISD1"]]
trgene$edge.length <- rep(2,nrow(trgene$edge))
#Run edgeIndexRelativeMaster, matchNodesInject, and matchAllNodes from RERfuncs.R
ee=edgeIndexRelativeMaster(trgene, mamtrees$masterTree)
ii= mamtrees$matIndex[ee[, c(2,1)]]
#Now the indices for foreground branches are which(phenv[ii] == 1)
RelativeRate <- rertree$edge.length
Phenotype <- c(rep(NA, length(RelativeRate)))
Phenotype[phenvdiet[ii] == 1] <- "Carnivore"
Phenotype[phenvdiet[ii] == 0] <- "Herbivore"
rtoplot <- data.frame(RelativeRate, Phenotype)
rtoplot <- rtoplot[!is.na(rtoplot$Phenotype),]
library(ggplot2)
cisd1plot <- ggplot(rtoplot, aes(x=Phenotype, y=RelativeRate, col=Phenotype)) + 
  geom_violin(adjust=1/3) +
  geom_jitter(position=position_jitter(0.2)) +
  theme_classic() +
  scale_color_manual(values=c("lightsalmon","darkgreen")) +
  theme(text = element_text(size = 20))
ggsave("../CISD1RelativeRatesByPhenotype.pdf", plot=cisd1plot)


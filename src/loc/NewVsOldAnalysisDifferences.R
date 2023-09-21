library(RERconverge)

# ---------
#Check differences in binary foreground tree: 
# --------

oldBinaryTree = read.tree("Data/Culled_0511_FishCarnvHerb.txt")
newBinaryTree = readRDS("Data/carnvHerbsBinaryForegroundTree.rds")

all.equal(oldBinaryTree, newBinaryTree)

plotTree(oldBinaryTree)
plotTree(newBinaryTree)
#YES, THE BINARY TREES ARE THE SAME. 
#HOwever, they are not actually truly binary -- they have a few instaces of a 2 edge length

truelyBinaryTree = newBinaryTree
truelyBinaryTree$edge.length[which(truelyBinaryTree$edge.length >1)] = 1

plotTreeHighlightBranches(newBinaryTree, hlspecies = which(newBinaryTree$edge.length >1))

newBinaryTree[which(newBinaryTree$edge.length == 2)]

#---- 
#Look into the branches with length 2 in the binary tree 
#-----
debugPlotTree = function(debugTree){dev.off(); dev.new(); dev.new(); testplot2 = plotTreeHighlightBranches(debugTree,hlspecies=which(debugTree$edge.length== 2), main="Input tree"); edgelabels(cex = 0.7, frame="none", font=2, adj=c(0,-0.2), col="blue"); nodelabels(cex = 0.7, frame="none", font=2, adj=c(-0.2,0.3), col="dark green"); tiplabels(cex = 0.8, frame="none", font=2, adj=c(0.2,0), col="dark red")}
debugPlotTree(newBinaryTree)

# ----
# Check differences in the species filter: 
# ----
oldSpeciesFilter = 
  {manualAnnot <- read.csv("Data/MA_Laura.csv") #read in spreadsheet
  lauraManualAnnot <- manualAnnot[manualAnnot$Laurasiatheria==1,] #subset data frame to just Laurasiatheria species
  laurasp <- lauraManualAnnot$FaName[lauraManualAnnot$FaName %in% oldBinaryTree$tip.label] #get overlap of Laurasiatheria and species in phenotype tree
  laurasp}
newSpeciesFilter = readRDS("Data/carnvHerbsSpeciesFilter2-2-23.rds")

lauraIncludedSpeciesFilter = readRDS("Data/carnvHerbsSpeciesFilter.rds")

all.equal(oldSpeciesFilter, newSpeciesFilter)
length(newSpeciesFilter)
length(lauraIncludedSpeciesFilter)

length(which(newSpeciesFilter %in% lauraIncludedSpeciesFilter))

#YES, THE SPECIES FILTERS ARE THE SAME 

# ------
# Check differences in the Master Tree: 
# ------
oldMasterTree = load("Data/Phylotrees_FishCarn.Rdata")
oldMasterTree = FishTree

newMasterTree = readRDS("../RunRER/Data/RemadeTreesAllZoonomiaSpecies.rds")

all.equal(oldMasterTree, newMasterTree)

# NO   THE MASTER TREES ARE NOTABLY DIFFERENT 

# -----
# Check Differences in the RERs
# -----

oldRERs = readRDS("Data/RERFiles/mamRERCMU_FishCarn.rds")
newRERs = readRDS("Data/RERFiles/carnvHerbsRERFile2-2-23.rds")

all.equal(oldRERs, newRERs)

# NO   THE RERS ARE DIFFERENT 

# --------------------
# --------------------
# Check to see if the difference in the RERs is due to the difference in the master tree. 
# Generating RERs using the new script, using FishTree as the master tree. 
#saveRDS(FishTree, "Data/FishTree.rds")


remakeOldRERs = readRDS("Data/RERFiles/carnvHerbsOldRebuildRERFile.rds")
all.equal(oldRERs, remakeOldRERs)
# NO, THESE RERS ARE DIFFRENT
#This is using the larua-only species filter for the generation of both the RERs and the Paths, and it looks like the new results

remakeOldRERsNonlauraIncluded = readRDS("Data/RERFiles/carnvHerbsOldRebuildRERFileWrongFilter.rds")

oldRERs = readRDS("Data/RERFiles/mamRERCMU_FishCarn.rds")
remakeOldRERsNonlauraIncluded = getAllResiduals(FishTree, useSpecies = lauraIncludedSpeciesFilter, plot = F)

all.equal(oldRERs, remakeOldRERsNonlauraIncluded)
# SORTOF, THESE RERS ARE SLIGHTLY DIFFERENT. 
#They have the same number of NAs, but the values have changed slightly. Updates to the package?

remakeNewRERsWithOldTree = getAllResiduals(FishTree, useSpecies = newSpeciesFilter, plot = F)

all.equal(newRERs, remakeNewRERsWithOldTree)
all.equal(oldRERs, remakeNewRERsWithOldTree)

# ----

plotRers(remakeOldRERsDifferentFilter, "TBCB")
plotRers(remakeOldRERs, "TBCB")

#The "different"/"Old" filter seems to produce similar results to the new RERs, despite using the old tree. That's very odd, and worth investigating further at a later point. 

# ---- 
# Compare the RERs
# ----
par(mfrow = c(1,1))
par(mfrow = c(1,3))
treePlotRers(FishTree, remakeOldRERsDifferentFilter, "TBCB")

treePlotRers(FishTree, remakeOldRERs, "TBCB")

treePlotRers(FishTree, oldRERs, "TBCB")

#INTERESTING< the old data appears to include RERs for the non-laura species; but the entire fishtree. 

# ---------
# Calculate test statistics to ensure that they are the same as in the file being used
# ---------

#--
#Make Paths files 
#--

otherPathsObject = tree2Paths(oldBinaryTree, FishTree, binarize=T, useSpecies = otherSpeciesFilter)
newPathsObject = tree2Paths(newBinaryTree, newMasterTree, binarize=T, useSpecies = newSpeciesFilter)

oldTreesNewFilterPathsObject = tree2Paths(oldBinaryTree, FishTree, binarize=TRUE, useSpecies = newSpeciesFilter)

binarizedOTNFPathsObject = oldTreesNewFilterPathsObject
binarizedOTNFPathsObject[which(binarizedOTNFPathsObject >1)] = 1
binarizedOTNFPathsObject

trueBinaryTreeOldFilterPaths = tree2Paths(truelyBinaryTree, FishTree, binarize=TRUE, useSpecies = newSpeciesFilter)
trueBinaryNewTreePaths =  tree2Paths(truelyBinaryTree, newMasterTree, binarize=TRUE, useSpecies = newSpeciesFilter)

all.equal(trueBinaryTreeOldFilterPaths, binarizedOTNFPathsObject)

?tree2Paths
#--
#Makes correlations 
#--
otherCorrelation = correlateWithBinaryPhenotype(oldRERs, otherPathsObject, min.sp =35)
newCorrelation = correlateWithBinaryPhenotype(newRERs, newPathsObject, min.sp =35)

oldRERsNewPathCorrelation = correlateWithBinaryPhenotype(oldRERs, oldTreesNewFilterPathsObject, min.sp =35)

newRERsWithOldTreeNewPathCorrelation = correlateWithBinaryPhenotype(remakeNewRERsWithOldTree, oldTreesNewFilterPathsObject, min.sp =35)

binarizedPathCorrelation = correlateWithBinaryPhenotype(oldRERs, binarizedOTNFPathsObject, min.sp =35)


#Load in old correlation results 
previousAnalysisData = read.csv("Data/pValues/CorrelationFishCarnHerbLaurasiatheria_wminsp_wnopermp_wcount_waddlperms0907.csv")
previousAnalysisDataCorrelation = previousAnalysisData[,(c(2,3,4,5))]
rownames(previousAnalysisDataCorrelation) = previousAnalysisData$X
newCorrelationFile = readRDS("Data/carnvHerbsCorrelationFile.rds")

all.equal(oldRERsNewPathCorrelation, previousAnalysisDataCorrelation)
all.equal(newRERsWithOldTreeNewPathCorrelation, previousAnalysisDataCorrelation)
all.equal(newRERsWithOldTreeNewPathCorrelation, newCorrelationFile)
all.equal(oldRERsNewPathCorrelation, binarizedPathCorrelation)
?all.equal

#-------------------------
#Compare the RERs in more detail 
# ---------------------

plotRers(oldRERs, "TBCB", phenv = oldTreesNewFilterPathsObject)
plotRers(newRERs, "TBCB", phenv = oldTreesNewFilterPathsObject)
plotRers(oldRERs, "TBCB", phenv = trueBinaryTreeOldFilterPaths)
plotRers(newRERs, "TBCB", phenv = trueBinaryNewTreePaths)

?plotRers

rermat = oldRERs
index = "TBCB"
phenv = NULL
rers = newSpeciesFilter 
useSpecies = NULL
useSpecies = newSpeciesFilter
rers = NULL
method = "k"
xlims = NULL
plot = 1
xextend = 0.2
sortrers = F

remadePlotRERs = function (rermat = NULL, index = NULL, phenv = NULL, rers = NULL, 
          method = "k", xlims = NULL, plot = 1, xextend = 0.2, sortrers = F, useSpecies = NULL) 
{
  if (!is.null(phenv) && length(unique(phenv[!is.na(phenv)])) > 
      2) {
    categorical = TRUE
    if (method != "aov") {
      method = "kw"
    }
  }else {
    categorical = FALSE
  }
  if (is.null(rers)) {
    e1 = rermat[index, ][!is.na(rermat[index, ])]
    colids = !is.na(rermat[index, ])
    e1plot <- e1
    if (exists("speciesNames")) {
      names(e1plot) <- speciesNames[names(e1), ]
    }
    if (is.numeric(index)) {
      gen = rownames(rermat)[index]
    }else {
      gen = index
    }
    e2 = e1[!is.na(names(e1)) & names(e1) %in% useSpecies]
    e2 = append(e2, e1[is.na(names(e1))])
    if(!is.null(useSpecies)){
      e1plot = e2
    } 
    message("Number of genes with RERs = ", length(e1plot[!is.na(names(e1plot))]))
  }else {
    e1plot = rers
    gen = "rates"
  }
  names(e1plot)[is.na(names(e1plot))] = ""
  if (!is.null(phenv)) {
    phenvid = phenv[colids]
    if (categorical) {
      fgdcor = getAllCor(rermat[index, , drop = F], phenv, 
                         method = method)[[1]]
    }
    else {
      fgdcor = getAllCor(rermat[index, , drop = F], phenv, 
                         method = method)
    }
    plottitle = paste0(gen, ": rho = ", round(fgdcor$Rho, 
                                              4), ", p = ", round(fgdcor$P, 4))
    if (categorical) {
      n = length(unique(phenvid))
      if (n > length(palette())) {
        pal = colorRampPalette(palette())(n)
      }
      else {
        pal = palette()[1:n]
      }
    }
    if (categorical) {
      df <- data.frame(species = names(e1plot), rer = e1plot, 
                       stringsAsFactors = FALSE) %>% mutate(mole = as.factor(phenvid))
    }
    else {
      df <- data.frame(species = names(e1plot), rer = e1plot, 
                       stringsAsFactors = FALSE) %>% mutate(mole = as.factor(ifelse(phenvid > 
                                                                                      0, 2, 1)))
    }
  }else {
    plottitle = gen
    df <- data.frame(species = names(e1plot), rer = e1plot, 
                     stringsAsFactors = FALSE) %>% mutate(mole = as.factor(ifelse(0, 
                                                                                  2, 1)))
  }
  if (sortrers) {
    df = filter(df, species != "") %>% arrange(desc(rer))
  }
  if (is.null(xlims)) {
    ll = c(min(df$rer) * 1.1, max(df$rer) + xextend)
  }else {
    ll = xlims
  }
  if (categorical) {
    g <- ggplot(df, aes(x = rer, y = factor(species, levels = unique(ifelse(rep(sortrers, 
                                                                                nrow(df)), species[order(rer)], sort(unique(species))))), 
                        col = mole, label = species)) + scale_size_manual(values = c(1, 
                                                                                     1, 1, 1)) + geom_point(aes(size = mole)) + scale_color_manual(values = pal) + 
      scale_x_continuous(limits = ll) + geom_text(hjust = 1, 
                                                  size = 2) + ylab("Branches") + xlab("relative rate") + 
      ggtitle(plottitle) + geom_vline(xintercept = 0, linetype = "dotted") + 
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
            legend.position = "none", panel.background = element_blank(), 
            axis.text = element_text(size = 18, face = "bold", 
                                     colour = "black"), axis.title = element_text(size = 24, 
                                                                                  face = "bold"), plot.title = element_text(size = 24, 
                                                                                                                            face = "bold")) + theme(axis.line = element_line(colour = "black", 
                                                                                                                                                                             size = 1)) + theme(axis.line.y = element_blank())
  }else {
    g <- ggplot(df, aes(x = rer, y = factor(species, levels = unique(ifelse(rep(sortrers, 
                                                                                nrow(df)), species[order(rer)], sort(unique(species))))), 
                        col = mole, label = species)) + scale_size_manual(values = c(1, 
                                                                                     1, 1, 1)) + geom_point(aes(size = mole)) + scale_color_manual(values = c("deepskyblue3", 
                                                                                                                                                                            "brown1")) + scale_x_continuous(limits = ll) + geom_text(hjust = 1, 
                                                                                                                                                                            size = 2) + ylab("Branches") + xlab("relative rate") + 
      ggtitle(plottitle) + geom_vline(xintercept = 0, linetype = "dotted") + 
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
            legend.position = "none", panel.background = element_blank(), 
            axis.text = element_text(size = 18, face = "bold", 
                                     colour = "black"), axis.title = element_text(size = 24, 
                                                                                  face = "bold"), plot.title = element_text(size = 24, 
                                                                                                                            face = "bold")) + theme(axis.line = element_line(colour = "black", 
                                                                                                                                                                             size = 1)) + theme(axis.line.y = element_blank())
  }
  if (plot) {
    print(g)
    g
  }else {
    g
  }
}


plotRers(oldRERs, "TBCB")
plotRers(newRERs, "TBCB")
remadePlotRERs(oldRERs, "TBCB")
remadePlotRERs(oldRERs, "TBCB", useSpecies = newSpeciesFilter)
remadePlotRERs(newRERs, "TBCB")

standaloneOldRERsFilteredPlot = remadePlotRERs(oldRERs, "TBCB", useSpecies = newSpeciesFilter)

newRERPlot = remadePlotRERs(newRERs, "TBCB")

phenv = NULL
rermat = oldRERs
{
  if (!is.null(phenv) && length(unique(phenv[!is.na(phenv)])) > 
      2) {
    categorical = TRUE
    if (method != "aov") {
      method = "kw"
    }
  }else {
    categorical = FALSE
  }
  if (is.null(rers)) {
    e1 = rermat[index, ][!is.na(rermat[index, ])]
    colids = !is.na(rermat[index, ])
    e1plot <- e1
    if (exists("speciesNames")) {
      names(e1plot) <- speciesNames[names(e1), ]
    }
    if (is.numeric(index)) {
      gen = rownames(rermat)[index]
    }else {
      gen = index
    }
    e2 = e1[!is.na(names(e1)) & names(e1) %in% useSpecies]
    e2 = append(e2, e1[is.na(names(e1))])
    if(!is.null(useSpecies)){
      e1plot = e2
    } 
    message("Number of genes with RERs = ", length(e1plot[!is.na(names(e1plot))]))
  }else {
    e1plot = rers
    gen = "rates"
  }
  names(e1plot)[is.na(names(e1plot))] = ""
  if (!is.null(phenv)) {
    phenvid = phenv[colids]
    if (categorical) {
      fgdcor = getAllCor(rermat[index, , drop = F], phenv, 
                         method = method)[[1]]
    }
    else {
      fgdcor = getAllCor(rermat[index, , drop = F], phenv, 
                         method = method)
    }
    plottitle = paste0(gen, ": rho = ", round(fgdcor$Rho, 
                                              4), ", p = ", round(fgdcor$P, 4))
    if (categorical) {
      n = length(unique(phenvid))
      if (n > length(palette())) {
        pal = colorRampPalette(palette())(n)
      }
      else {
        pal = palette()[1:n]
      }
    }
    if (categorical) {
      df <- data.frame(species = names(e1plot), rer = e1plot, 
                       stringsAsFactors = FALSE) %>% mutate(mole = as.factor(phenvid))
    }
    else {
      df <- data.frame(species = names(e1plot), rer = e1plot, 
                       stringsAsFactors = FALSE) %>% mutate(mole = as.factor(ifelse(phenvid > 
                                                                                      0, 2, 1)))
    }
  }else {
    plottitle = gen
    df <- data.frame(species = names(e1plot), rer = e1plot, 
                     stringsAsFactors = FALSE) %>% mutate(mole = as.factor(ifelse(0, 
                                                                                  2, 1)))
  }
  if (sortrers) {
    df = filter(df, species != "") %>% arrange(desc(rer))
  }
  if (is.null(xlims)) {
    ll = c(min(df$rer) * 1.1, max(df$rer) + xextend)
  }else {
    ll = xlims
  }
  if (categorical) {
    g <- ggplot(df, aes(x = rer, y = factor(species, levels = unique(ifelse(rep(sortrers, 
                                                                                nrow(df)), species[order(rer)], sort(unique(species))))), 
                        col = mole, label = species)) + scale_size_manual(values = c(1, 
                                                                                     1, 1, 1)) + geom_point(aes(size = mole)) + scale_color_manual(values = pal) + 
      scale_x_continuous(limits = ll) + geom_text(hjust = 1, 
                                                  size = 2) + ylab("Branches") + xlab("relative rate") + 
      ggtitle(plottitle) + geom_vline(xintercept = 0, linetype = "dotted") + 
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
            legend.position = "none", panel.background = element_blank(), 
            axis.text = element_text(size = 18, face = "bold", 
                                     colour = "black"), axis.title = element_text(size = 24, 
                                                                                  face = "bold"), plot.title = element_text(size = 24, 
                                                                                                                            face = "bold")) + theme(axis.line = element_line(colour = "black", 
                                                                                                                                                                             size = 1)) + theme(axis.line.y = element_blank())
  }else {
    g <- ggplot(df, aes(x = rer, y = factor(species, levels = unique(ifelse(rep(sortrers, 
                                                                                nrow(df)), species[order(rer)], sort(unique(species))))), 
                        col = mole, label = species)) + scale_size_manual(values = c(1, 
                                                                                     1, 1, 1)) + geom_point(aes(size = mole)) + scale_color_manual(values = c("deepskyblue3", 
                                                                                                                                                                            "brown1")) + scale_x_continuous(limits = ll) + geom_text(hjust = 1, 
                                                                                                                                                                            size = 2) + ylab("Branches") + xlab("relative rate") + 
      ggtitle(plottitle) + geom_vline(xintercept = 0, linetype = "dotted") + 
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
            legend.position = "none", panel.background = element_blank(), 
            axis.text = element_text(size = 18, face = "bold", 
                                     colour = "black"), axis.title = element_text(size = 24, 
                                                                                  face = "bold"), plot.title = element_text(size = 24, 
                                                                                                                            face = "bold")) + theme(axis.line = element_line(colour = "black", 
                                                                                                                                                                             size = 1)) + theme(axis.line.y = element_blank())
  }
  if (plot) {
    print(g)
    g
  }else {
    g
  }
}
standaloneOldRERsFilteredPlotDF = df 

phenv = trueBinaryNewTreePaths
rermat = newRERs
{
  if (!is.null(phenv) && length(unique(phenv[!is.na(phenv)])) > 
      2) {
    categorical = TRUE
    if (method != "aov") {
      method = "kw"
    }
  }else {
    categorical = FALSE
  }
  if (is.null(rers)) {
    e1 = rermat[index, ][!is.na(rermat[index, ])]
    colids = !is.na(rermat[index, ])
    e1plot <- e1
    if (exists("speciesNames")) {
      names(e1plot) <- speciesNames[names(e1), ]
    }
    if (is.numeric(index)) {
      gen = rownames(rermat)[index]
    }else {
      gen = index
    }
    e2 = e1[!is.na(names(e1)) & names(e1) %in% useSpecies]
    e2 = append(e2, e1[is.na(names(e1))])
    if(!is.null(useSpecies)){
      e1plot = e2
    } 
    message("Number of genes with RERs = ", length(e1plot[!is.na(names(e1plot))]))
  }else {
    e1plot = rers
    gen = "rates"
  }
  names(e1plot)[is.na(names(e1plot))] = ""
  if (!is.null(phenv)) {
    phenvid = phenv[colids]
    if (categorical) {
      fgdcor = getAllCor(rermat[index, , drop = F], phenv, 
                         method = method)[[1]]
    }
    else {
      fgdcor = getAllCor(rermat[index, , drop = F], phenv, 
                         method = method)
    }
    plottitle = paste0(gen, ": rho = ", round(fgdcor$Rho, 
                                              4), ", p = ", round(fgdcor$P, 4))
    if (categorical) {
      n = length(unique(phenvid))
      if (n > length(palette())) {
        pal = colorRampPalette(palette())(n)
      }
      else {
        pal = palette()[1:n]
      }
    }
    if (categorical) {
      df <- data.frame(species = names(e1plot), rer = e1plot, 
                       stringsAsFactors = FALSE) %>% mutate(mole = as.factor(phenvid))
    }
    else {
      df <- data.frame(species = names(e1plot), rer = e1plot, 
                       stringsAsFactors = FALSE) %>% mutate(mole = as.factor(ifelse(phenvid > 
                                                                                      0, 2, 1)))
    }
  }else {
    plottitle = gen
    df <- data.frame(species = names(e1plot), rer = e1plot, 
                     stringsAsFactors = FALSE) %>% mutate(mole = as.factor(ifelse(0, 
                                                                                  2, 1)))
  }
  if (sortrers) {
    df = filter(df, species != "") %>% arrange(desc(rer))
  }
  if (is.null(xlims)) {
    ll = c(min(df$rer) * 1.1, max(df$rer) + xextend)
  }else {
    ll = xlims
  }
  if (categorical) {
    g <- ggplot(df, aes(x = rer, y = factor(species, levels = unique(ifelse(rep(sortrers, 
                                                                                nrow(df)), species[order(rer)], sort(unique(species))))), 
                        col = mole, label = species)) + scale_size_manual(values = c(1, 
                                                                                     1, 1, 1)) + geom_point(aes(size = mole)) + scale_color_manual(values = pal) + 
      scale_x_continuous(limits = ll) + geom_text(hjust = 1, 
                                                  size = 2) + ylab("Branches") + xlab("relative rate") + 
      ggtitle(plottitle) + geom_vline(xintercept = 0, linetype = "dotted") + 
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
            legend.position = "none", panel.background = element_blank(), 
            axis.text = element_text(size = 18, face = "bold", 
                                     colour = "black"), axis.title = element_text(size = 24, 
                                                                                  face = "bold"), plot.title = element_text(size = 24, 
                                                                                                                            face = "bold")) + theme(axis.line = element_line(colour = "black", 
                                                                                                                                                                             size = 1)) + theme(axis.line.y = element_blank())
  }else {
    g <- ggplot(df, aes(x = rer, y = factor(species, levels = unique(ifelse(rep(sortrers, 
                                                                                nrow(df)), species[order(rer)], sort(unique(species))))), 
                        col = mole, label = species)) + scale_size_manual(values = c(1, 
                                                                                     1, 1, 1)) + geom_point(aes(size = mole)) + scale_color_manual(values = c("deepskyblue3", 
                                                                                                                                                                            "brown1")) + scale_x_continuous(limits = ll) + geom_text(hjust = 1, 
                                                                                                                                                                            size = 2) + ylab("Branches") + xlab("relative rate") + 
      ggtitle(plottitle) + geom_vline(xintercept = 0, linetype = "dotted") + 
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
            legend.position = "none", panel.background = element_blank(), 
            axis.text = element_text(size = 18, face = "bold", 
                                     colour = "black"), axis.title = element_text(size = 24, 
                                                                                  face = "bold"), plot.title = element_text(size = 24, 
                                                                                                                            face = "bold")) + theme(axis.line = element_line(colour = "black", 
                                                                                                                                                                             size = 1)) + theme(axis.line.y = element_blank())
  }
  if (plot) {
    print(g)
    g
  }else {
    g
  }
}
standaloneNewRERsPlotDF = df 


h <- ggplot(standaloneNewRERsPlotDF, aes(x = rer, y = factor(species, levels = unique(ifelse(rep(sortrers, 
                                                                            nrow(df)), species[order(rer)], sort(unique(species))))), 
                    col = mole, label = species)) + scale_size_manual(values = c(1, 
                                                                                 1, 1, 1)) + geom_point(aes(size = mole)) + scale_color_manual(values = c("deepskyblue3", 
                                                                                                                                                                        "brown1")) + scale_x_continuous(limits = ll) + geom_text(hjust = 1, 
                                                                                                                                                                        size = 2) + ylab("Branches") + xlab("relative rate") + 
  ggtitle(plottitle) + geom_vline(xintercept = 0, linetype = "dotted") + geom_point(data = standaloneOldRERsFilteredPlotDF, color = "green") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
        legend.position = "none", panel.background = element_blank(), 
        axis.text = element_text(size = 18, face = "bold", 
                                 colour = "black"), axis.title = element_text(size = 24, 
                                                                              face = "bold"), plot.title = element_text(size = 24, 
                                                                                                                        face = "bold")) + theme(axis.line = element_line(colour = "black", 
                                                                                                                                                                         size = 1)) + theme(axis.line.y = element_blank())
h


# ----
# Testing difference between species filter old and from manul annots 
# ----

manualSpeciesFilter = readRDS("Data/CVHRemakespeciesFilter.rds")
manualSpeciesFilter
oldSpeciesFilter
oldSpeciesFilter %in% manualSpeciesFilter

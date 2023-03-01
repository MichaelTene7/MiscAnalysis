library(RERconverge)
oldBinaryTree = read.tree("Data/Culled_0511_FishCarnvHerb.txt")

truelyBinaryTree = oldBinaryTree
truelyBinaryTree$edge.length[which(truelyBinaryTree$edge.length >1)] = 1

oldTreesNewFilterPathsObject = tree2Paths(oldBinaryTree, FishTree, binarize=TRUE, useSpecies = newSpeciesFilter, isbinarypheno = T)
trueBinaryTreeOldFilterPaths = tree2Paths(truelyBinaryTree, FishTree, binarize=TRUE, useSpecies = newSpeciesFilter)

oldRERsNewPathCorrelation = correlateWithBinaryPhenotype(oldRERs, oldTreesNewFilterPathsObject, min.sp =35)
binarizedPathCorrelation = correlateWithBinaryPhenotype(oldRERs, trueBinaryTreeOldFilterPaths, min.sp =35)

all.equal(oldRERsNewPathCorrelation, previousAnalysisDataCorrelation)
  #TRUE

all.equal(oldRERsNewPathCorrelation, binarizedPathCorrelation)
  #FALSE

truelyBinaryTree$edge.length 
oldBinaryTree$edge.length
is.rooted(truelyBinaryTree)
is.rooted(oldBinaryTree)



#-----------------------------------------

tree = oldBinaryTree
treesObj = FishTree
binarize = TRUE
useSpecies = newSpeciesFilter
categorical = F

tree2Paths = function (tree, treesObj, binarize = NULL, useSpecies = NULL, 
          categorical = F, isbinarypheno = NULL) 
{
  stopifnot(class(tree)[1] == "phylo")
  stopifnot(class(treesObj)[2] == "treesObj")
  if(is.null(isbinarypheno)){
    isbinarypheno <- sum(tree$edge.length %in% c(0, 1)) == length(tree$edge.length)
  }
  if (is.null(binarize)) {
    if (isbinarypheno) {
      binarize = T
    }
    else {
      binarize = F
    }
  }
  if (is.rooted(tree)) {
    tree = unroot(tree)
  }
  sp.miss = setdiff(tree$tip.label, union(treesObj$masterTree$tip.label, 
                                          useSpecies))
  if (length(sp.miss) > 0) {
    message(paste0("Species from tree not present in master tree or useSpecies: ", 
                   paste(sp.miss, collapse = ",")))
  }
  if (!is.null(useSpecies)) {
    tree = pruneTree(tree, intersect(intersect(tree$tip.label, 
                                               treesObj$masterTree$tip.label), useSpecies))
  }else {
    tree = pruneTree(tree, intersect(tree$tip.label, treesObj$masterTree$tip.label))
  }
  treePaths = allPaths(tree, categorical = categorical)
  map = matchAllNodes(tree, treesObj$masterTree)
  treePaths$nodeId[, 1] = map[treePaths$nodeId[, 1], 2]
  treePaths$nodeId[, 2] = map[treePaths$nodeId[, 2], 2]
  ii = treesObj$ap$matIndex[(treePaths$nodeId[, 2] - 1) * nrow(treesObj$ap$matIndex) + 
                              treePaths$nodeId[, 1]]
  vals = double(length(treesObj$ap$dist))
  vals[] = NA
  vals[ii] = treePaths$dist
  if (binarize) {
    if (isbinarypheno) {
      vals[vals > 0] = 1
    }else {
      mm = mean(vals, na.rm = T)
      vals[vals <= mm] = 0
      vals[vals > mm] = 1
    }
  }
  vals
}

allPaths=function(tree, categorical = F){
  if (!categorical){
    dd=dist.nodes(tree)
  }
  allD=double()
  nn=matrix(nrow=0, ncol=2)
  nA=length(tree$tip.label)+tree$Nnode
  matIndex=matrix(nrow=nA, ncol=nA)
  index=1
  for ( i in 1:nA){
    ia=getAncestors(tree,i)
    if(length(ia)>0){
      if(categorical) {
        # add the state of node i to allD length(ia) times
        x = which(tree$edge[,2] == i)
        state = tree$edge.length[x]
        allD = c(allD, rep(state, length(ia)))
      }
      else {
        allD=c(allD, dd[i, ia])
      }
      nn=rbind(nn,cbind(rep(i, length(ia)), ia))
      for (j in ia){
        matIndex[i,j]=index
        index=index+1
      }
    }
  }
  return(list(dist=allD, nodeId=nn, matIndex=matIndex))
}


getAncestors=function(tree, nodeN){
  if(is.character(nodeN)){
    nodeN=which(tree$tip.label==nodeN)
  }
  im=which(tree$edge[,2]==nodeN)
  if(length(im)==0){
    return()
  }
  else{
    anc=tree$edge[im,1]
    return(c(anc, getAncestors(tree, anc)))
  }
  
}

matchAllNodes=function(tree1, tree2){
  map=matchNodesInject(tree1,tree2)
  map=map[order(map[,1]),]
  map
}

matchNodesInject=function (tr1, tr2){
  if(length(tmpsp<-setdiff(tr1$tip.label, tr2$tip.label))>0){
    #stop(paste(paste(tmpsp, ","), "in tree1 do not exist in tree2"))
    stop(c("The following species in tree1 do not exist in tree2: ",paste(tmpsp, ", ")))
  }
  commontiplabels <- intersect(tr1$tip,tr2$tip)
  if(RF.dist(pruneTree(tr1,commontiplabels),pruneTree(tr2,commontiplabels))>0){
    stop("Discordant tree topology detected - gene/trait tree and treesObj$masterTree have irreconcilable topologies")
  }
  #if(RF.dist(tr1,tr2)>0){
  #  stop("Discordant tree topology detected - trait tree and treesObj$masterTree have irreconcilable topologies")
  #}
  
  toRm=setdiff(tr2$tip.label, tr1$tip.label)
  desc.tr1 <- lapply(1:tr1$Nnode + length(tr1$tip), function(x) extract.clade(tr1,
                                                                              x)$tip.label)
  names(desc.tr1) <- 1:tr1$Nnode + length(tr1$tip)
  desc.tr2 <- lapply(1:tr2$Nnode + length(tr2$tip), function(x) extract.clade(tr2,
                                                                              x)$tip.label)
  names(desc.tr2) <- 1:tr2$Nnode + length(tr2$tip)
  Nodes <- matrix(NA, length(desc.tr1), 2, dimnames = list(NULL,
                                                           c("tr1", "tr2")))
  for (i in 1:length(desc.tr1)) {
    Nodes[i, 1] <- as.numeric(names(desc.tr1)[i])
    for (j in 1:length(desc.tr2)) if (all(desc.tr1[[i]] %in%
                                          desc.tr2[[j]]))
      Nodes[i, 2] <- as.numeric(names(desc.tr2)[j])
  }
  
  iim=match(tr1$tip.label, tr2$tip.label)
  Nodes=rbind(cbind(1:length(tr1$tip.label),iim),Nodes)
  if(any(table(Nodes[,2])>1)){
    stop("Incorrect pseudorooting detected - use fixPseudoroot() function to correct trait tree topology")
  }
  
  Nodes
}

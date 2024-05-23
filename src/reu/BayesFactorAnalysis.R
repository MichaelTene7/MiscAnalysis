#CREDIT THIS SCRIPT TO AMANDA Kowalczyk, the original version was hers, I have since cleaned it up

#this script shows how to calculate Bayes factors from RERs and a phenotype
#Bayes factors indicate the level of support for a particular model (between 0 and 1 indicates more support for the null model)
#ratios of Bayes factors indicate relative support for one model over another
#bigger Bayes factor = better; different sources give different thresholds for a "good" Bayes factor

#what you need:
#RERs
#phenotype tree
#trees

#what you need to change in this script:
#filenames
#phenotype names/labels
library(BayesFactor)
calculateBayesFactor = function(mainTrees, RERObject, combinationTree, subclade1Name, subclade1Tree, subclade2Name, subclade2Tree){
  #make paths for each phenotype
  class(mainTrees)[2]="treesObj"
  combinationPath = tree2Paths(combinationTree, mainTrees)
  subclade1Path = tree2Paths(subclade1Tree, mainTrees)
  subclade2Path = tree2Paths(subclade2Tree, mainTrees)
  
  #make dataframe to store Bayes factors
  allBayesFactors=data.frame(matrix(nrow=nrow(RERObject), ncol=3))
  colnames(allBayesFactors)=c("Combination", subclade1Name, subclade2Name)
  rownames(allBayesFactors)=rownames(RERObject)
  
  #calculate Bayes factors per gene
  for(i in 1:nrow(RERObject)){
    #combination
    tempdf=data.frame(trait=combinationPath, RER=RERObject[i,])
    tempdf=na.omit(tempdf)
    bfmod=lmBF(trait~RER, data=tempdf)
    bf=as.numeric(extractBF(bfmod)[1])
    allBayesFactors[i,1]=bf
    
    #subclade1
    tempdf=data.frame(trait=subclade1Path, RER=RERObject[i,])
    tempdf=na.omit(tempdf)
    bfmod=lmBF(trait~RER, data=tempdf)
    bf=as.numeric(extractBF(bfmod)[1])
    allBayesFactors[i,2]=bf 
    
    #subclade2
    tempdf=data.frame(trait=subclade2Path, RER=RERObject[i,])
    tempdf=na.omit(tempdf)
    bfmod=lmBF(trait~RER, data=tempdf)
    bf=as.numeric(extractBF(bfmod)[1])
    allBayesFactors[i,3]=bf 
    
    message(i)
  }
  return(allBayesFactors)
}


#---------------------------------------------


# combinationTree= "marinetree.rds"
# subclade1Tree ="subcladeTree1.rds"
# subclade2Tree = "subcladeTree2.rds"
# 
# mainTrees = "finaltreesv4.rds"
# RERObject = "weightresidRER.rds"
# 
# #read in phenotype trees
# marinetree=readRDS("marinetree.rds")
# hairlesstree=readRDS("statetree.rds")
# hairlessnomarinetree=readRDS("hairlessnomarinetree.rds")
# marinenohairlesstree=readRDS("marinenohairlesstree.rds")
# 
# #read in trees and RERs
# trees=readRDS("finaltreesv4.rds")
# class(trees)[2]="treesObj"
# newRERmat=readRDS("weightresidRER.rds")
# #create phenotype paths using trees and phenotype trees
# marinepath=tree2Paths(marinetree, trees)
# hairlesspath=tree2Paths(hairlesstree, trees)
# hairlessnomarinepath=tree2Paths(hairlessnomarinetree, trees)
# marinenohairlesspath=tree2Paths(marinenohairlesstree, trees)
# 
# #make dataframe to store Bayes factors
# library(BayesFactor)
# allBayesFactors=data.frame(matrix(nrow=nrow(RERmat), ncol=4))
# colnames(allBayesFactors)=c("marine", "hairless", "hairlessnomarine", "marinenohairless")
# rownames(allBayesFactors)=rownames(newRERmat)
# 
# #calculate Bayes factors per gene
# count=1
# while(count<=nrow(newRERmat)){
#   #marine
#   tempdf=data.frame(trait=marinepath, RER=newRERmat[count,])
#   tempdf=na.omit(tempdf)
#   bfmod=lmBF(trait~RER, data=tempdf)
#   bf=as.numeric(extractBF(bfmod)[1])
#   allBayesFactors$marine[count]=bf
#   
#   #hairless
#   tempdf=data.frame(trait=hairlesspath, RER=newRERmat[count,])
#   tempdf=na.omit(tempdf)
#   bfmod=lmBF(trait~RER, data=tempdf)
#   bf=as.numeric(extractBF(bfmod)[1])
#   allBayesFactors$hairless[count]=bf 
#   
#   #hairlessnomarine
#   tempdf=data.frame(trait=hairlessnomarinepath, RER=newRERmat[count,])
#   tempdf=na.omit(tempdf)
#   bfmod=lmBF(trait~RER, data=tempdf)
#   bf=as.numeric(extractBF(bfmod)[1])
#   allBayesFactors$hairlessnomarine[count]=bf 
#   
#   #marinenohairless
#   tempdf=data.frame(trait=marinenohairlesspath, RER=newRERmat[count,])
#   tempdf=na.omit(tempdf)
#   bfmod=lmBF(trait~RER, data=tempdf)
#   bf=as.numeric(extractBF(bfmod)[1])
#   allBayesFactors$marinenohairless[count]=bf
#   
#   count=count+1
#   print(count)
# }
# saveRDS(allBayesFactors, "allBayesFactors.rds")
# 
# 
# allBayesFactors$marine[2] = 5
# 
# allBayesFactors[2,1]

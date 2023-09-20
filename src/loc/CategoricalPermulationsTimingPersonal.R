library(RERconverge)
phenotypeVectors = readRDS("CategoricalPermuationsTimingPhenotypes.rds")
mainTrees = readRDS("CategoricalPermulationsTimingTrees.rds")

phenotypeVector = phenotypeVectors$phenotypeCH_OG

#Code used to get the times
permsStartTime = Sys.time()                                                     #get the time before start of permulations
permulationData = categoricalPermulations(mainTrees, phenotypeVector, rm = "SYM", rp = "auto", ntrees = 5)
permsEndTime = Sys.time()                                                       #get time at end of permulations

timeCH_OG = permsEndTime-permsStartTime

premadeTimes = readRDS("CategoricalPermulationsTimes.rds")



# - Length effect - 
length(phenotypeVectors$phenotypeGPC)
premadeTimes$timeGPC
length(phenotypeVectors$phenotypeCHO)
premadeTimes$timeCHO
length(phenotypeVectors$phenotypeCH_OG)
premadeTimes$timeCH_OG


# - Percentage Effect -
percentCategories = function(input){
  categories = unique(input)
  output = NULL
  for(i in 1:length(categories)){
  percent = length(which(input ==categories[i]))/length(input)
  result = paste(categories[i], paste(substr(as.character(percent*100), 1, 2), "%", sep=''))
  output = append(output, result)
  }
  output
}

#All of these are fairly similar
premadeTimes$timeGPH
premadeTimes$timeGPO
premadeTimes$timeGPC
#Note the increase in this case
percentCategories(phenotypeVectors$phenotypeGPI) 
premadeTimes$timeGPI
#
char2TreeCategorical(phenotypeVectors$phenotypeGPC, mainTrees, plot=T)
premadeTimes$timeGPC

char2TreeCategorical(phenotypeVectors$phenotypeGPI, mainTrees, plot=T)
premadeTimes$timeGPI
#
#Note the decrease in time when percentage is reduced
#Even though the overall tree is larger
length(phenotypeVectors$phenotypeGPI)
percentCategories(phenotypeVectors$phenotypeGPI) 
premadeTimes$timeGPI

length(phenotypeVectors$phenotypeHOI)
percentCategories(phenotypeVectors$phenotypeHOI) 
char2TreeCategorical(phenotypeVectors$phenotypeHOI, mainTrees, plot=T)
premadeTimes$timeHOI


# - Phenotype number effect - 
premadeTimes$timeGPC
premadeTimes$timeGPO
premadeTimes$timeGPCO

premadeTimes$timeCHO
premadeTimes$timeCH_OG
premadeTimes$timeCHOG


# ------------ 



phentotypeHOI = phenotypeVectorMain[which(phenotypeVectorMain %in% c("Herbivore", "Omnivore", "Insectivore"))]

phenotypeVector = phentotypeHOI

#Code used to get the times
permsStartTime = Sys.time()                                                     #get the time before start of permulations
permulationData = categoricalPermulations(mainTrees, phenotypeVector, rm = "SYM", rp = "auto", ntrees = 5)
permsEndTime = Sys.time()  
  
timeHOI = permsEndTime-permsStartTime
timeHOI


timeHOIconvert = list(timeHOI)
premadeTimes[10] = timeHOIconvert
names(premadeTimes)[10] = "timeHOI"


phenotypeVectors2 = phenotypeVectors
phenotypeVectors2 = append(phenotypeVectors2, "", after=0)
phenotypeVectors2[1] = phenotypeVectors[12]
names(phenotypeVectors2)[1] = "phenotypeVectorAllCategories"  
phenotypeVectors2[13] = list(phentotypeHOI)
names(phenotypeVectors2)[13] = "phenotypeHOI"


phenotypeVectors = phenotypeVectors2

#saveRDS(phenotypeVectors, "CategoricalPermuationsTimingPhenotypes.rds")
#saveRDS(premadeTimes, "CategoricalPermulationsTimes.rds")




char2TreeCategorical()

intlabels = map_to_state_space(phenotypeVectorMain)
states = getStatesFromPhenTree(mainTrees, root_state, intlabels$mapped_states)
plotTreeCategorical(tree = mainTrees, category_names = intlabels$state_names,
                    master = toyTrees$masterTree, node_states = states)



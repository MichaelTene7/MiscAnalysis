library(RERconverge)
library(cowplot)
makePHistogram = function(data, column, titleVal){
  phist = ggplot(data, aes(x=data[[column]]))+
    geom_histogram(binwidth = 0.02, alpha=0.9, col="white", boundary=0)+
    scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1))+
    labs(title = titleVal)+
    xlab("p Values")+
    theme(
      #axis.title.x = element_blank(),
      plot.title = element_text(size=18, hjust = 0.5),
      plot.title.position = "plot"
    )
}
pairwiseHistogram = function(set, index, rm1=F){
  target = set[[index]]
  if(rm1){
    target = target[-which(target$P == 1),]
  }
  output = makePHistogram(as.data.frame(target), "P", names(set)[index])
}
fixNames = function(set, replacements, header){
  nameSet = names(set)
  for(i in 1:length(replacements)){
    nameSet = gsub(i, replacements[i], nameSet)
  }
  for(i in 1:length(nameSet)){
    nameSet[i] = paste(header,": ", nameSet[i], sep="")
  }
  nameSet 
}

phen3Overall = readRDS("Data/CategoricalComponentData/CategoricalDiet3PhenCorrelationFile.rds")
phen4Overall = readRDS("Data/CategoricalComponentData/CategoricalDiet4PhenCorrelationFile.rds")
phen5Overall = readRDS("Data/CategoricalComponentData/CategoricalDiet5PhenCorrelationFile.rds")



phen3Pairwise = readRDS("Data/CategoricalComponentData/CategoricalDiet3PhenPairwiseCorrelationFile.rds")
phen3PairwiseClean = phen3Pairwise[1:3]
names(phen3PairwiseClean) = fixNames(phen3PairwiseClean, c("Omnivore"),"3Phen")

phen4Pairwise = readRDS("Data/CategoricalComponentData/CategoricalDiet4PhenPairwiseCorrelationFile.rds")
phen4PairwiseClean = phen4Pairwise[1:6]
names(phen4PairwiseClean) = fixNames(phen4PairwiseClean, c("Omnivore"),"4Phen")

phen5Pairwise = readRDS("Data/CategoricalComponentData/CategoricalDiet5PhenPairwiseCorrelationFile.rds")
phen5PairwiseClean = phen5Pairwise[1:10]
names(phen5PairwiseClean) = fixNames(phen5PairwiseClean, c("Omnivore"),"5Phen")

# -- overall values -- 

p3Overall = makePHistogram(phen3Overall, "P", "3 Phenotype Overall")
p4Overall = makePHistogram(phen4Overall, "P", "4 Phenotype Overall")
p5Overall = makePHistogram(phen5Overall, "P", "5 Phenotype Overall")
p3Overall
p4Overall
p5Overall

overallHistograms = plot_grid(p3Overall, p4Overall, p5Overall, nrow = 3)
overallHistograms

# -- 3 phenotypes -- 
p3OC = pairwiseHistogram(phen3PairwiseClean, 1)
p3OH = pairwiseHistogram(phen3PairwiseClean, 2)
p3CH = pairwiseHistogram(phen3PairwiseClean, 3)

p3OCrm1 = pairwiseHistogram(phen3PairwiseClean, 1, T)
p3OHrm1 = pairwiseHistogram(phen3PairwiseClean, 2, T)
p3CHrm1 = pairwiseHistogram(phen3PairwiseClean, 3, T)

phen3with1 = plot_grid(p3OC, p3OH, p3CH, ncol=1)
phen3no1 = plot_grid(p3OCrm1, p3OHrm1, p3CHrm1, ncol=1)

phen3 = plot_grid(phen3with1, phen3no1)

phen3_Overall = plot_grid(overallHistograms, phen3, ncol=1)

# -- 4 phenotypes -- 
p4OC = pairwiseHistogram(phen4PairwiseClean, 1)
p4OH = pairwiseHistogram(phen4PairwiseClean, 2)
p4CH = pairwiseHistogram(phen4PairwiseClean, 3)
p4OI = pairwiseHistogram(phen4PairwiseClean, 4)
p4CI = pairwiseHistogram(phen4PairwiseClean, 5)
p4HI = pairwiseHistogram(phen4PairwiseClean, 6)


p4OCrm1 = pairwiseHistogram(phen4PairwiseClean, 1,T)
p4OHrm1 = pairwiseHistogram(phen4PairwiseClean, 2,T)
p4CHrm1 = pairwiseHistogram(phen4PairwiseClean, 3,T)
p4OIrm1 = pairwiseHistogram(phen4PairwiseClean, 4,T)
p4CIrm1 = pairwiseHistogram(phen4PairwiseClean, 5,T)
p4HIrm1 = pairwiseHistogram(phen4PairwiseClean, 6,T)

phen4with1 = plot_grid(p4OC, p4OH, p4CH, p4OI, p4CI, p4HI, ncol=1)
phen4no1 = plot_grid(p4OCrm1, p4OHrm1, p4CHrm1, p4OIrm1, p4CIrm1, p4HIrm1, ncol=1)

phen4 = plot_grid(phen4with1, phen4no1)

phen4_phen3_Overall = plot_grid(phen3_Overall, phen4, ncol=1)

# -- 5 phenotypes -- 
p5OC = pairwiseHistogram(phen5PairwiseClean, 1)
p5OH = pairwiseHistogram(phen5PairwiseClean, 2)
p5CH = pairwiseHistogram(phen5PairwiseClean, 3)
p5OI = pairwiseHistogram(phen5PairwiseClean, 4)
p5CI = pairwiseHistogram(phen5PairwiseClean, 5)
p5HI = pairwiseHistogram(phen5PairwiseClean, 6)
p5OP = pairwiseHistogram(phen5PairwiseClean, 7)
p5CP = pairwiseHistogram(phen5PairwiseClean, 8)
p5HP = pairwiseHistogram(phen5PairwiseClean, 9)
p5IP = pairwiseHistogram(phen5PairwiseClean, 10)

p5OCrm1 = pairwiseHistogram(phen5PairwiseClean, 1,T)
p5OHrm1 = pairwiseHistogram(phen5PairwiseClean, 2,T)
p5CHrm1 = pairwiseHistogram(phen5PairwiseClean, 3,T)
p5OIrm1 = pairwiseHistogram(phen5PairwiseClean, 4,T)
p5CIrm1 = pairwiseHistogram(phen5PairwiseClean, 5,T)
p5HIrm1 = pairwiseHistogram(phen5PairwiseClean, 6,T)
p5OPrm1 = pairwiseHistogram(phen5PairwiseClean, 7,T)
p5CPrm1 = pairwiseHistogram(phen5PairwiseClean, 8,T)
p5HPrm1 = pairwiseHistogram(phen5PairwiseClean, 9,T)
p5IPrm1 = pairwiseHistogram(phen5PairwiseClean, 10,T)

phen5with1 = plot_grid(p5OC, p5OH, p5CH, p5OI, p5CI, p5HI, p5OP, p5CP, p5HP, p5IP, ncol=1)
phen5no1 = plot_grid(p5OCrm1, p5OHrm1, p5CHrm1, p5OIrm1, p5CIrm1, p5HIrm1, p5OPrm1, p5CPrm1, p5HPrm1, p5IPrm1, ncol=1)

phen5 = plot_grid(phen5with1, phen5no1, ncol=2)

phen5_phen4_pen3_overall = plot_grid(phen4_phen3_Overall, phen5, ncol=1)

pdf("Output/PairwisePValueContributions.pdf", height = 35)
phen5_phen4_pen3_overall
dev.off()

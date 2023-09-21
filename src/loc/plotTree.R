library(RERconverge)
install.packages("rphylopic")
install.packages("ggtree")
install.packages("gg")
library(ggtree)
install.packages("ggimage")
library("ggimage")
library(rphylopic)
install.packages("taxize")
library(taxize)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")


longevityTree = read.tree("Data/LongevityTree.txt")

longevityTree$tip.label

imgdir <- system.file("extdata/frogs", package = "TDbook")

img <- pick_phylopic(name = "Canis lupus", n = 5)

phylop

plot(x = 1, y = 1, type = "n")
add_phylopic_base(img = img, x = 1.25, y = 1.25, ysize = 0.25)
?pick_phylopic


manualAnnot = read.csv("Data/manualAnnotationsSheet.csv")





ZonomNameConvertVectorCommonReverse = function(namesVector){
  names = namesVector                                                    #make a vector of the names
  manualAnnot = read.csv("Data/manualAnnotationsSheet.csv")                     #improt manual annots file
  for(i in 1:length(names)){                                                    #for each name: 
    currentName = names[i]                                                      #use the 'i'th name in the list
    currentRow = manualAnnot[manualAnnot$Common.Name.or.Group %in% currentName, ]             #find a row with the zonom name that matches the current name 
    currentSize = dim(currentRow)                                               #Part of "does row exist check": get the dimensions of the currentRow dataframe
    obsNumber = currentSize[1]                                                  #set "size" equal to the number of observations in 'currentRow'; which is the number of matches to the current name. If none exist it will be 0, if more than one it will be greater than 1. 
    if(obsNumber == 1){                                                         #if only one match exists:
      currentName = currentRow$FaName                            #get the name from that row
    }else{
      currentName = names[i]                                                    #Otherwise, keep the name the same
    }
    names[i] = currentName                                                      #update the main name list with the name chose
  }
  #colnames(nMatrix) = names                                                     #update the matrix with the new names. 
  return(names)
}

ZonomNameConvertVectorScientific= function(namesVector){
    names = namesVector                                                    #make a vector of the names
    manualAnnot = read.csv("Data/manualAnnotationsSheet.csv")                     #improt manual annots file
    for(i in 1:length(names)){                                                    #for each name: 
      currentName = names[i]                                                      #use the 'i'th name in the list
      currentRow = manualAnnot[manualAnnot$FaName %in% currentName, ]             #find a row with the zonom name that matches the current name 
      currentSize = dim(currentRow)                                               #Part of "does row exist check": get the dimensions of the currentRow dataframe
      obsNumber = currentSize[1]                                                  #set "size" equal to the number of observations in 'currentRow'; which is the number of matches to the current name. If none exist it will be 0, if more than one it will be greater than 1. 
      if(obsNumber == 1){                                                         #if only one match exists:
        currentName = currentRow$Species.Name                            #get the name from that row
      }else{
        currentName = names[i]                                                    #Otherwise, keep the name the same
      }
      names[i] = currentName                                                      #update the main name list with the name chose
    }
    #colnames(nMatrix) = names                                                     #update the matrix with the new names. 
    return(names)
  }

converted1Tips = ZonomNameConvertVectorCommonReverse(longevityTree$tip.label)
converted2Tips = ZonomNameConvertVectorScientific(converted1Tips)


longevityTree$tip.label

scienctificTips = c("homo_sapiens", "Pan troglodytes", "Gorilla_gorilla_gorilla", "Pongo pygmaeus", "Hylobates lar", "Cebus", "Macaca sylvanus",
                    "Macaca mulatta", )
converted2Tips 


convertedTipsRedo = comm2sci(longevityTree$tip.label, )

newick <- paste0("((Pongo_abelii,(Gorilla_gorilla_gorilla,(Pan_paniscus,",
                 "Pan_troglodytes)Pan,Homo_sapiens)Homininae)Hominidae,",
                 "Nomascus_leucogenys)Hominoidea;")

tree <- read.tree(text=newick)

d <- ggimage::phylopic_uid(tree$tip.label)
d$body_mass <- c(52, 114, 47, 45, 58, 6)

p <- ggtree(tree) %<+% d + 
  geom_tiplab(aes(image=uid, colour=body_mass), geom="phylopic", offset=2.5) +
  geom_tiplab(aes(label=label), offset = .2) + xlim(NA, 7) +
  scale_color_viridis_c()

taxize_options(ncbi_sleep = 0.335)


newTree = read.tree("Data/LEISRtree.txt")
plotTree(newTree)

inVector = c(0.000439909, 0.000533502, 0.000998439, 0.00120168, 0.001218402, 0.001342745, 0.001387027, 0.001434225,0.001757379,0.00194638,0.002099242, 0.002104286)
outvector = rep(NA, 12)

i=1
for(i in 1:12){
outvector[i] = inVector[i] + runif(n=1, min = 0-(inVector[i]/10), max = (inVector[i]/10))
}
outvector
write.csv(outvector, "outvector.csv")

inVector2 = c(0.040798527, 0.076864562, 0.103053385, 0.011583751, 0.053683874, 0.058175568, 0.014480013, 0.044356376, 0.134512259, 0.063080223, 0.031521284, 0.060699432)
outvector2 = rep(NA, 12)
for(i in 1:12){
  outvector2[i] = inVector2[i] + runif(n=1, min = 0-(inVector2[i]/10), max = (inVector2[i]/10))
}
write.csv(outvector2, "outvector2.csv")



inVector3 = read.csv("inVector3.csv")
inVector4 = inVector3[[1]]
outVector3 = rep(NA, length(inVector4))
for(i in 1:length(inVector4)){
  outVector3[i] = inVector4[i] + runif(n=1, min = 0-(inVector4[i]/10), max = (inVector4[i]/10))
}
write.csv(outVector3, "outvector3.csv")


plot(inVector4, outVector3)

df = data.frame(inVector4, outVector3)

library(ggplot2)

lm_eqn <- function(df){
  m <- lm(inVector4 ~ outVector3, df);
  eq <- substitute(italic(inVector4) == a + b %.% italic(outVector3)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

p1 <- p + geom_text(outVector3 = 25, inVector4 = 300, label = lm_eqn(df), parse = TRUE)


png("R2Plot.png", width = 300, height = 300)

plot(inVector4, outVector3, pch = 16, cex = 1.3, col = "black", xlab = "Genetic Correlation", ylab = "Protein Correlation")
abline(lm(inVector4 ~ outVector3))

Model<-lm(outVector3~inVector4,data=df)
legend("topleft",legend=paste("R2 is", format(summary(Model)$r.squared,digits=3)))


dev.off()
dev.new()

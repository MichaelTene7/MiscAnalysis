#For the various sources of phenotype data, determine how many species in the
#manual annotations spreadsheet (with FaName in Zoonomia alignments) have
#phenotypes from that source.
library(dplyr) #for combining data frames
#Change below to your working directory:
setwd("/Volumes/GoogleDrive/My Drive/MeyerLabLehigh/MammalDiet/PhenotypeData/")
#Read in manual annotations spreadsheet
ma <- read.csv("ManualAnnotations061522.csv")
ma <- ma[ma$Common.Name.or.Group != "",] #remove extra rows
names(ma)[1] <- "NowakPage"
write.csv(ma, file="ManualAnnotations061522.csv", row.names=F)
#Read in phenotype dataset of interest
pantheria05 <- read.table("PanTHERIA_1-0_WR05_Aug2008.txt", row.names=NULL, sep="\t",
                          header=T) #Binomial has spaces so need tab delimiter
pantheria93 <- read.table("PanTHERIA_1-0_WR93_Aug2008.txt", row.names=NULL, sep="\t",
                          header=T)
#Is 93 a subset of 05?
sum(colnames(pantheria93)==colnames(pantheria05)) #48
dim(pantheria05) #56
colnames(pantheria05)[which(colnames(pantheria05) %in% colnames(pantheria93) == F)]
colnames(pantheria93)[which(colnames(pantheria93) %in% colnames(pantheria05) == F)]
#These are basically the same, just with different capitalization and absbreviation
trymerge <- merge(pantheria05, pantheria93, by.x="MSW05_Binomial",
                  by.y="MSW93_Binomial")
dim(trymerge) #[1] 4080  109
dim(pantheria93) #[1] 4629   55
#Not quite.... some 93 species are not in 05 (it appears)
#Combine the two pantheria data frames
names(pantheria05)
wnamestofix <- which(colnames(pantheria05) %in% colnames(pantheria93) == F)
colnames(pantheria05)[wnamestofix]
newnames <- c("MSWC_Order", "MSWC_Family","MSWC_Genus","MSWC_Species","MSWC_Binomial",
              colnames(pantheria05)[wnamestofix][6:8])
newnames
colnames(pantheria93)[wnamestofix] <- newnames
colnames(pantheria05)[wnamestofix] <- newnames
pantheriacomb <- union(pantheria93, pantheria05) #only unique rows
#Identifying which values differ:
head(pantheriacomb$MSWC_Binomial)
pantheriacomb[pantheriacomb$MSWC_Binomial=="Abditomys latidens",]
sum(pantheriacomb[1,] == pantheriacomb[9451,]) #[1] 52
ncol(pantheriacomb) #[1] 55
pantheriacomb[1,] == pantheriacomb[9451,]
#Differences in X13.1_AdultHeadBodyLen_mm, X5.1_AdultBodyMass_g, & X26.1_GR_Area_km2
#Take just 05 dataset, then add any species from 93 not in 05
tocomb <- pantheria05
tocomb$WhichPantheria <- "WR05"
toadd <- pantheria93[which(pantheria93$MSWC_Binomial %in% tocomb$MSWC_Binomial == F),]
toadd$WhichPantheria <- "WR93"
pantheriacomb <- rbind(tocomb, toadd)
dim(pantheriacomb)
dim(pantheria05)
#write.table(pantheriacomb, file="Combined_PanTHERIA_1-0_WR05_WR93_Aug2008.txt",
#            row.names=F, quote=F)
#write as csv to prevent issues with spaces in binomial names
write.csv(pantheriacomb, file="Combined_PanTHERIA_1-0_WR05_WR93_Aug2008.csv")
pantheriacomb <- read.csv("Combined_PanTHERIA_1-0_WR05_WR93_Aug2008.csv")
#Find overlap with Zoonomia species
#Add new species from tree!
convtable <- read.csv("/Volumes/GoogleDrive/My Drive/MeyerLabLehigh/MammalDiet/Zoonomia/FullTree/ConvertFile.csv",header=F)
head(convtable$treeName)
library(stringr)
ma$spname_wund <- str_replace_all(ma$Species.Name, " ", "_")
head(ma$spname_wund)
sum(ma$spname_wund %in% convtable$treeName) #226 - still only those w/FaName
#To do: Add other 10 species to manual annotations sheet.
pantheriacomb$MSWC_Binomial_wund <- str_replace_all(pantheriacomb$MSWC_Binomial, " ", "_")
sum(pantheriacomb$MSWC_Binomial_wund %in% convtable$treeName)
ma_inzoo <- ma[ma$FaName != "",]
dim(ma_inzoo) #[1] 226  65 - also find species missing from tree....
library(phytools)
mtree <- read.tree("../../ProteinStructure/AllSpeciesMasterTree.tre")
head(mtree$tip.label)
length(mtree$tip.label) #[1] 219 - actually *more* than in spreadsheet! 
mamissfromtree <- ma_inzoo$FaName[ma_inzoo$FaName %in% mtree$tip.label == F]
write(mamissfromtree, file="../FaNamesMissingFromMasterTree.txt", ncolumns=1)
#Deal with this later (and the 14 species in neither spreadsheet nor tree)
sum(ma_inzoo$Species.Name %in% pantheriacomb$MSWC_Binomial) #[1] 206
#This is promising!
ma_wpantheria <- merge(ma_inzoo, pantheriacomb, by.x = "Species.Name",
                       by.y="MSWC_Binomial", all.x=T)
dim(ma_wpantheria)
write.csv(ma_wpantheria, file="MA_WithPantheria.csv", row.names=F)
tofindpan <- ma_inzoo$Species.Name[ma_inzoo$Species.Name %in% pantheriacomb$MSWC_Binomial == F]
head(names(ma_inzoo))
names(ma_inzoo)[1:15]
subma <- ma_inzoo[ma_inzoo$Species.Name %in% tofindpan,c(2,3,5,7,8)]
write.csv(subma, file="SpeciesToFindInPantheria.csv", row.names=F)

#Look for overlap with Stephens TDF study
tdflit <- read.csv("/Volumes/GoogleDrive/My Drive/MeyerLabLehigh/MammalDiet/PhenotypeData/Stephens2022Dataset/Literature_Review_TDF_data.csv")
ma_wpantheria <- read.csv("MA_WithPantheria.csv")
sum(ma_inzoo$Common.Name.or.Group %in% tdflit$Common_name)
head(ma_inzoo$Species.Name)
head(tdflit$Species)
tdflit$BinomialName <- paste(tdflit$Genus, tdflit$Species)
head(tdflit$BinomialName)
sum(ma_inzoo$Species.Name %in% tdflit$BinomialName)
tofindtdf <- unique(tdflit$BinomialName[tdflit$BinomialName %in% ma_inzoo$Species.Name == F])
head(tofindtdf)
length(tofindtdf)
head(tdflit)
table(tdflit$Study)
tdflitfieldonly <- tdflit[tdflit$Study == "Field",]
length(unique(tdflitfieldonly$Common_name))

#Find overlap with stomach content study, MammalDIET, and microbiome studies
#(are the microbiome data downloadable?)
stom <- read.csv("Pineda-MunozSuppTable_StomachContents.csv")
stom$Species <- trimws(stom$Species)
convtable$spwspace <- str_replace_all(convtable$treeName, "_", " ")
sum(stom$Species %in% convtable$spwspace) #8! seems very low
head(stom$Species)
table(stom$Order) #super enriched for rodents.... prob not Zoonomia

#Better luck with microbiome?
micro <- read.csv("Diaz2021Table1_wMcKenzie.csv")
sum(ma$Common.Name.or.Group %in% micro$CommonName) #5 (depressing)
sum(convtable$spwspace %in% micro$ScientificName) #14 - OK
zoo_wcomm <- read.csv("200m_species_withCommonNames.csv")
sum(zoo_wcomm$speciesName_scientific %in% convtable$treeName)
zoo_wcomm_inaln <- zoo_wcomm[zoo_wcomm$speciesName_scientific %in% convtable$treeName,]
sum(zoo_wcomm_inaln$speciesName_common %in% micro$CommonName) #2!
head(micro$CommonName)
sum(convtable$spwspace %in% micro$ScientificName) #37 - argh!

#Track these down manually?
convtable <- merge(convtable, zoo_wcomm_inaln, by.x="treeName",
                   by.y="speciesName_scientific")
convtable$HasMicrobiome <- convtable$spwspace %in% micro$ScientificName
write.csv(convtable, file="ForFindingMicrobiomeData.csv",row.names=F)

#New paper!
#https://journals.asm.org/doi/10.1128/mBio.02901-19
songdata <- read.csv("Song2020Supp.csv")
names(songdata)
head(songdata$Species_name)
head(songdata$Winker.Corrected.Species.name)
sum(convtable$spwspace %in% songdata$Species_name) #72! This is great!
sum(convtable$HasMicrobiome)
tofind <- convtable$spwspace[convtable$HasMicrobiome == F]
sum(tofind %in% songdata$Species_name)
37+42

#A tangent: See how many Zoonomia species are in the alignment
library(phytools)
fulltree <- read.tree("/Volumes/GoogleDrive/My Drive/MeyerLabLehigh/MammalDiet/Zoonomia/FullTree/RenamedZoonomia_ChrX_lessGC40_241species_30Consensus.tree")
spnames <- scan("/Volumes/GoogleDrive/My Drive/MeyerLabLehigh/MammalDiet/Zoonomia/SpeciesNames_fromLargestFiles.txt",
                what="character")
library(stringr)
spnamesonly <- str_replace_all(spnames,">","")
missingbutintree <- fulltree$tip.label[fulltree$tip.label %in% spnamesonly == F]
write(missingbutintree, file="/Volumes/GoogleDrive/My Drive/MeyerLabLehigh/MammalDiet/Zoonomia/FullTree/SpeciesToCheckInAlignments.txt",ncolumns=1)
#some renamed incorrectly (blank tip labels)
#some without Fa names
inittree <- read.tree("/Volumes/GoogleDrive/My Drive/MeyerLabLehigh/MammalDiet/Zoonomia/FullTree/Zoonomia_ChrX_lessGC40_241species_30Consensus.tree")
inittree$tip.label[which(fulltree$tip.label %in% missingbutintree)]
spnotconverted <- inittree$tip.label[which(fulltree$tip.label %in% missingbutintree)]
"REFERENCE" %in% spnamesonly #human called REFERENCE in alignments but vs_REFERENCE in tree.... is this an issue?
inalnnottree <- spnamesonly[spnamesonly %in% fulltree$tip.label==F]
length(inalnnottree)
head(inalnnottree)
setwd("/Volumes/GoogleDrive/My Drive/MeyerLabLehigh/MammalDiet/Zoonomia/FullTree")
write(spnotconverted, file="UnconvertedSpeciesNames.txt",ncolumns=1)
write(inalnnottree, file="SpeciesInAlignmentsNotMasterTree.txt",ncolumns=1)
#Reconvert species names
convtable <- read.csv("ConvertFile.csv",header=F)
names(convtable) <- c("treeName","FaName")
newfulltree <- inittree
sum(newfulltree$tip.label %in% convtable$treeName)
sptorem <- ""
for (i in c(1:length(newfulltree$tip.label))) {
  if (newfulltree$tip.label[i] %in% convtable$treeName) {
    newfulltree$tip.label[i] <- convtable$FaName[which(convtable$treeName
                                                       == newfulltree$tip.label[i])]
  } else {
    print(paste(newfulltree$tip.label[i],"not found in convtable"))
    sptorem <- c(sptorem, newfulltree$tip.label[i])
  }
}
write.tree(newfulltree, file="WithUnconv_RenamedZoonomia_ChrX_lessGC40_241species_30Consensus.tree")
convtree <- drop.tip(newfulltree, sptorem)
write.tree(convtree, file="236sp_RenamedZoonomia_ChrX_lessGC40_241species_30Consensus.tree")
dim(ma)
sum(ma$FaName != "")

#Combine ma, pantheria, and song data in a file to use for PCA
#use just relevant columns
names(ma_inzoo)
names(pantheriacomb)
names(songdata)
tknames <- c(names(ma_inzoo)[c(2,3,8:22)],"MSWC_Binomial","X5.2_BasalMetRateMass_g",
             "X6.1_DietBreadth","X6.2_TrophicLevel","Species_name","pd_5k","shannon_5k")
masub <- ma_inzoo[,which(names(ma_inzoo) %in% tknames)]
pasub <- pantheriacomb[,which(names(pantheriacomb) %in% tknames)]
sosub <- songdata[,which(names(songdata) %in% tknames)]
dim(sosub)
head(sosub)
#Take mean diversity indices (probably a better way to summarize!)
meansosub <- sosub[1,]
for (i in c(1:length(unique(sosub$Species_name)))) {
  minsosub <- sosub[sosub$Species_name == unique(sosub$Species_name)[i],]
  newrow <- minsosub[1,]
  newrow$pd_5k <- mean(minsosub$pd_5k,na.rm=T)
  newrow$shannon_5k <- mean(minsosub$shannon_5k,na.rm=T)
  meansosub <- rbind(meansosub,newrow)
}
meansosub <- meansosub[-1,]
dim(meansosub)
#Fix panthera to remove -999
pasub[pasub==-999] <- NA
names(masub)
names(pasub)
names(sosub)
dietmerge <- merge(masub, pasub, by.x = "Species.Name", by.y = "MSWC_Binomial", all.x = T)
dietmerge <- merge(dietmerge, meansosub, by.x = "Species.Name", by.y = "Species_name", all.x=T)
write.csv(dietmerge, file="PrelimDietDataForPCA_MA_Pan_Song.csv",row.names=F)
#Manually fix column names

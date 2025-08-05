
# PANTHERIA 
combinedPantheria = read.csv("Data/CombinedPanTHERIA.csv")
combinedPantheriaUseful = combinedPantheria[!combinedPantheria$X23.1_SexualMaturityAge_d == -999,]

conversionTable = read.csv("Data/speciesNameConversionTable.csv")


conversionTable$BinomialPretty %in% combinedPantheria$MSWC_Binomial
missingSpecies = which(!conversionTable$BinomialPretty %in% combinedPantheria$MSWC_Binomial)
conversionTable$Common.Name.or.Group[missingSpecies]

speciesWithMaturityData = conversionTable$BinomialPretty[conversionTable$BinomialPretty %in% combinedPantheriaUseful$MSWC_Binomial]
speciesMissingMaturityData = which(!conversionTable$BinomialPretty %in% combinedPantheriaUseful$MSWC_Binomial)
conversionTable$Common.Name.or.Group[speciesMissingMaturityData]
speciesMissingPantheriaMaturity = conversionTable$BinomialPretty[speciesMissingMaturityData]

length(speciesWithData)

ageTable = conversionTable
ageTable$PantheriaMaturity = NA
ageTable$PantheriaMaturity = combinedPantheriaUseful$X23.1_SexualMaturityAge_d[match(ageTable$BinomialPretty, combinedPantheriaUseful$MSWC_Binomial)]

ageTable$pantheriaMaxAge = combinedPantheria$X17.1_MaxLongevity_m[match(ageTable$BinomialPretty, combinedPantheriaUseful$MSWC_Binomial)]
ageTable$pantheriaMaxAge[ageTable$pantheriaMaxAge == -999] = NA
ageTable$pantheriaMaxAge = ageTable$pantheriaMaxAge*30
ageTable$pantheriaMaxAge
ageTable$pantheriaBodysize = combinedPantheriaUseful$X5.1_AdultBodyMass_g[match(ageTable$BinomialPretty, combinedPantheriaUseful$MSWC_Binomial)]


# Life Histories 

lifeHistories = read.table("Data/mammalLifeHistoriesV2.tsv", sep="\t", header = T)
lifeHistories$binomial = paste(lifeHistories$Genus, lifeHistories$species)
lifeHistoriesUseful = lifeHistories[!lifeHistories$AFR.mo. == -999,]
lifeHistoriesUseful$AfrDays = lifeHistoriesUseful$AFR.mo.*30

conversionTable$BinomialPretty %in% lifeHistories$binomial

ageTable$lifeHistoriesMaturity = lifeHistoriesUseful$AfrDays[match(ageTable$BinomialPretty, lifeHistoriesUseful$binomial)]

ageTable$lifeHistoryMaxAge = lifeHistories$max..life.mo.[match(ageTable$BinomialPretty, lifeHistories$binomial)]
ageTable$lifeHistoryMaxAge[ageTable$lifeHistoryMaxAge == -999] = NA
ageTable$lifeHistoryMaxAge = ageTable$lifeHistoryMaxAge * 30
ageTable$lifeHistoryBodysize = lifeHistories$mass.g.[match(ageTable$BinomialPretty, lifeHistories$binomial)]


cor(ageTable$PantheriaMaturity, ageTable$lifeHistoriesMaturity, use = "complete.obs")
 #This shows that the two measures of maturity are pretty good amtches for eachother. 


# AnAge 
anAge = read.csv("Data/anage_data.csv")

anAge$binomial = paste(anAge$Genus, anAge$Species, sep = " ")

anAgeUseful = anAge[!is.na(anAge$Female.maturity..days.),]


length(conversionTable$Common.Name.or.Group[!conversionTable$BinomialPretty %in% anAge$binomial])
length(conversionTable$Common.Name.or.Group[!conversionTable$BinomialPretty %in% anAgeUseful$binomial])

ageTable$anAgeMaturity = anAgeUseful$Female.maturity..days.[match(ageTable$BinomialPretty, anAgeUseful$binomial)]

ageTable$anAgeMaxAge = anAge$Maximum.longevity..yrs.[match(ageTable$BinomialPretty, anAge$binomial)]
ageTable$anAgeMaxAge = ageTable$anAgeMaxAge * 365
ageTable$anAgeBodysize = anAge$Adult.weight..g.[match(ageTable$BinomialPretty, anAge$binomial)]



cor(ageTable$PantheriaMaturity, ageTable$anAgeMaturity, use = "complete.obs")
cor(ageTable$lifeHistoriesMaturity, ageTable$anAgeMaturity, use = "complete.obs")


#Combine the values across datasets 

length(which(is.na(ageTable$PantheriaMaturity)))
length(which(is.na(ageTable$lifeHistoriesMaturity)))
length(which(is.na(ageTable$anAgeMaturity)))

ageTable$combinedMaturity = ageTable$anAgeMaturity
length(which(is.na(ageTable$combinedMaturity)))

ageTable$combinedMaturity[is.na(ageTable$combinedMaturity)] = ageTable$lifeHistoryBodysize[is.na(ageTable$combinedMaturity)]
length(which(is.na(ageTable$combinedMaturity)))

ageTable$combinedMaturity[is.na(ageTable$combinedMaturity)] = ageTable$PantheriaMaturity[is.na(ageTable$combinedMaturity)]
length(which(is.na(ageTable$combinedMaturity)))

ageTable$Binomial[which(is.na(ageTable$combinedMaturity))]

# - 
ageTable$combinedBodysize= ageTable$anAgeBodysize
length(which(is.na(ageTable$combinedMaturity)))

ageTable$combinedBodysize[is.na(ageTable$combinedBodysize)] = ageTable$lifeHistoriesMaturity[is.na(ageTable$combinedBodysize)]
length(which(is.na(ageTable$combinedMaturity)))

ageTable$combinedBodysize[is.na(ageTable$combinedBodysize)] = ageTable$pantheriaBodysize[is.na(ageTable$combinedBodysize)]
length(which(is.na(ageTable$combinedBodysize)))

ageTable$Binomial[which(is.na(ageTable$combinedBodysize))]


ageTable$Binomial[which(is.na(ageTable$combinedBodysize))] %in% ageTable$Binomial[which(is.na(ageTable$combinedMaturity))]

#Add maxAge collumn 
ageTable$MaximumAge = ageTable$anAgeMaxAge
length(which(is.na(ageTable$MaximumAge)))

ageTable$MaximumAge[is.na(ageTable$MaximumAge)] = ageTable$lifeHistoryMaxAge[is.na(ageTable$MaximumAge)]
length(which(is.na(ageTable$MaximumAge)))

ageTable$MaximumAge[is.na(ageTable$MaximumAge)] = ageTable$pantheriaMaxAge[is.na(ageTable$MaximumAge)]
length(which(is.na(ageTable$MaximumAge)))

which(is.na(ageTable$MaximumAge))


# Maturity as lifespan Fraction 

ageTable$MaturityPercentage = ageTable$combinedMaturity / ageTable$MaximumAge


length(which(!is.na(ageTable$MaturityPercentage)))




# ---- compare against the ELL are 3L phenotypes ---- 

elifeData = read.csv("Data/elife-51089-supp1.csv")

elifeData$longevityDays = elifeData$Maximum.longevity..yrs.*365

ageTable$elifeLongevity = elifeData$longevityDays[match(ageTable$FaName, elifeData$Code)]

cor(ageTable$MaximumAge, ageTable$elifeLongevity, use = "complete.obs")
#Those agree


ageTable$L3 = elifeData$X3L.phenotype..PC1.[match(ageTable$FaName, elifeData$Code)]
ageTable$ELL = elifeData$ELL.phenotype..PC2.[match(ageTable$FaName, elifeData$Code)]

cor(ageTable$MaturityPercentage, ageTable$L3, use = "complete.obs")
cor(ageTable$MaturityPercentage, ageTable$ELL, use = "complete.obs")


write.csv(ageTable, "Output/MaturityLifespanData.csv")
saveRDS(ageTable, "Output/MaturityLifespanData.rds")



# ----- Making the values into a vector for RERConverge ----


ageData = readRDS("Output/MaturityLifespanData.rds")

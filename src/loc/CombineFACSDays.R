library(xlsx)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

inputFileLocation = "Data/CountsExcel.xlsx"
outputFileLocation = "Results/competitionPilotLns.csv"

inputFileLocation = "Data/Run2Counts.xlsx"
outputFileLocation = "Results/competitionRun2Lns.csv"
outputpngLocation = "Results/YeastRatioPlots2.png"
slopepngLocation = "Results/YeastSlopePlot2.png"
slopeCsvLoaction = "Results/YeastSlopes2.csv"


day0 = read.xlsx(inputFileLocation, 1) 
day1 = read.xlsx(inputFileLocation, 2)
day2 = read.xlsx(inputFileLocation, 3)
day3 = read.xlsx(inputFileLocation, 4)
day4 = read.xlsx(inputFileLocation, 5)
day5 = read.xlsx(inputFileLocation, 6)
key= read.xlsx(inputFileLocation, 7)




combinations = expand.grid(letters = LETTERS[1:8], numbers = 1:12)
combinations$combo <- paste0(combinations$letters, sprintf("%02d", combinations$numbers))
sorted_combinations = combinations[order(combinations$letters, combinations$numbers), "combo"]

logValues = data.frame(sorted_combinations, rep(NA, 96),rep(NA, 96), rep(NA, 96), rep(NA, 96), rep(NA, 96), rep(NA, 96), rep(NA, 96),rep(NA, 96))
names(logValues) = c("well", "info", "version", "day0", "day1", "day2", "day3", "day4", "day5")


logValues$info = key$MediaStrain[match(logValues$well, key$Well)]
logValues$version = key$Version[match(logValues$well, key$Well)]





logValues$day0 = day0$ln.E.R.[match(logValues$well, day0$Well)]
logValues$day1 = day1$ln.E.R.[match(logValues$well, day1$Well)]
logValues$day2 = day2$ln.E.R.[match(logValues$well, day2$Well)]
logValues$day3 = day3$ln.E.R.[match(logValues$well, day3$Well)]
logValues$day4 = day4$ln.E.R.[match(logValues$well, day4$Well)]
logValues$day5 = day5$ln.E.R.[match(logValues$well, day5$Well)]

logValues

write.csv(logValues, outputFileLocation)



# -- Code for making the info column for wells with missing data, which would otherwise be completely missing because no column was in the dataset.  not needed if all wells have data -- 
length(which(is.na(logValues$info)))

startingNALogs = (which(is.na(logValues$info)))

for(i in which(is.na(logValues$info))){ #The targets not refreshing each loop is fine, because if there is a match, there will be information, info will never be overwritten with NA
  logValues$info[i] = day1$Info[match(logValues$well[i], day1$Well)]
  logValues$info[i] = day2$Info[match(logValues$well[i], day2$Well)]
  logValues$info[i] = day3$Info[match(logValues$well[i], day3$Well)]
  logValues$info[i] = day4$Info[match(logValues$well[i], day4$Well)]
}


dataLong <- logValues %>% gather(key = "day", value = "value", day1:day4)


dataLong$generations = substr(dataLong$day, 4,4)
dataLong$generations = as.numeric(dataLong$generations)*5
dataLong$media = substr(dataLong$info, 1,3)
dataLong$mediaVersion = paste(dataLong$media, dataLong$version)


gluData = dataLong %>% filter(media == "Glu")
serData = dataLong %>% filter(media == "Ser")
thrData = dataLong %>% filter(media == "Thr")


ggplot(data = logValues, aes())

ggplot(dataLong, aes(x = generations, y = value, group = well, color = mediaVersion)) +
  geom_line() +
  labs(x = "Generations", y = "ln(E/R)", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

colorOrder = c( "red", "green", "pink", "darkgreen", "gray", "yellow", "black", "orange")
palette(colorOrder)
scale_color_manual(values = colorOrder)


gluPlot = ggplot(gluData, aes(x = generations, y = value, group = well, color = version)) +
  geom_line(show.legend = F, size=1.5) +
  labs(x = "Generations", y = "ln(E/R)", title = "Glutamine (control)") +
  theme_minimal() +
  ylim(-3.5, 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(values = colorOrder)

serPlot = ggplot(serData, aes(x = generations, y = value, group = well, color = version)) +
  geom_line(show.legend = F, size=1.5) +
  labs(x = "Generations", y = "ln(E/R)", title = "Serine") +
  theme_minimal() +
  ylim(-3.5, 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(values = colorOrder)

thrPlot = ggplot(thrData, aes(x = generations, y = value, group = well, color = version)) +
  geom_line(size=1.5) +
  labs(x = "Generations", y = "ln(E/R)", title = "Threonine") +
  theme_minimal() +
  ylim(-3.5, 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(values = colorOrder)

grid.arrange(gluPlot, serPlot, thrPlot, ncol = 3)

png(outputpngLocation, 1920, 540)
grid.arrange(gluPlot, serPlot, thrPlot, ncol = 3)
dev.off()

dataLongClean = dataLong %>% filter(!is.na(value))

slopes <- dataLongClean %>%
  group_by(well) %>%
  do({
    model <- lm(value ~ generations, data = .)
    tibble(slope = coef(model)[2])  # Get the slope (the coefficient for 'day_numeric')
  })

#View(slopes)

slopes$mediaVersion = dataLong$mediaVersion[match(slopes$well, dataLong$well)]
slopes$media = dataLong$media[match(slopes$well, dataLong$well)]
slopes$version = dataLong$version[match(slopes$well, dataLong$well)]


palette(c( "red", "green", "purple", "darkgreen", "gray", "yellow", "black", "gold"))

slopeColors = c( "red", "green", "pink", "darkgreen", "gray", "yellow", "black", "orange", "red3", "green3", "pink3", "seagreen", "lightgray", "yellow2", "black", "orange3", "red4", "palegreen2", "pink4", "springgreen4", "ivory3", "yellow4", "black", "orange4")
slopeColors = c( "red", "green", "pink", "darkgreen", "gray", "yellow", "black", "orange","red", "green", "pink", "darkgreen", "gray", "yellow", "black", "orange","red", "green", "pink", "darkgreen", "gray", "yellow", "black", "orange", "red3", "green3", "pink3", "seagreen", "lightgray", "yellow2", "black", "orange3", "red4", "palegreen2", "pink4", "springgreen4", "ivory3", "yellow4", "black", "orange4")



slopeplot = ggplot(slopes, aes(x=mediaVersion, y=slope, group=well, color=version))+
  geom_point(size=5)+
  scale_color_manual(values = slopeColors)+
  theme_minimal()

png(slopepngLocation, 1920, 1080)
slopeplot
dev.off()

write.csv(slopes, slopeCsvLoaction)

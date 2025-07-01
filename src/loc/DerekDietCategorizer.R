
library(tidyverse)
#change to your filepath
Elton_full <- read.csv(combinedDataOutLocation)



classify_diet <- function(df, upper_cutoff=70, lower_cutoff=50) {
  
  diet_vars <- c("Diet.Inv", "Diet.Vend", "Diet.Vect", "Diet.Vfish", 
                 "Diet.Vunk", "Diet.Scav", "Diet.Fruit", "Diet.Nect", 
                 "Diet.Seed", "Diet.PlantO")
  ivore_names <- c("C-Invertebrate-eater", "C-Endotherm-Carnivore", "C-Herpetivore", "C-Piscivore", 
                   "C-Nonspecific-Vertebrate-eater", "O-Scavenger", "H-Frugivore", 
                   "H-Nectarivore", "H-Granivore", "H-Nonspecific-Herbivore")
  
  df <- df %>%
    mutate(Diet.VertTerrestrial = Diet.Vend + Diet.Vect + Diet.Vunk,
           Diet.VertAll = Diet.VertTerrestrial + Diet.Vfish,
           Diet.AnimalsAll = Diet.VertAll + Diet.Inv,
           Diet.PlantHighSugar = Diet.Fruit + Diet.Nect,
           Diet.PlantLowSugar = Diet.Seed + Diet.PlantO,
           Diet.PlantAll = Diet.PlantHighSugar + Diet.PlantLowSugar)
  
  df$Diet_Class <- apply(df[diet_vars], 1, function(row) {
    above_cutoff <- row >= upper_cutoff
    between_lower_and_upper <- row >= lower_cutoff & row < upper_cutoff
    below_cutoff <- row < lower_cutoff
    
    if (any(above_cutoff, na.rm = TRUE)) {
      return(ivore_names[which(above_cutoff)[1]])
    } else if (all(below_cutoff, na.rm = TRUE)) {
      return("Generalist")
    } else if (any(between_lower_and_upper, na.rm = TRUE)) {
      return("For Examination")
    }
  })
  
  combined_diet_vars <- c("Diet.VertTerrestrial", "Diet.VertAll", "Diet.AnimalsAll",
                          "Diet.PlantHighSugar", "Diet.PlantLowSugar", "Diet.PlantAll")
  combined_ivore_names <- c("C-Terrestrial-vertebrates-eater", "C-All-vertebrate-eater", "C-All-Animals-Eater",
                            "H-High-sugar-plants-Eater", "H-Low-sugar-plants-Eater", "H-All-plants-Eater")
  
  generalists <- df %>% filter(Diet_Class == "Generalist")
  generalists$Diet_Class <- apply(generalists[combined_diet_vars], 1, function(row) {
    above_cutoff <- row >= upper_cutoff
    if (any(above_cutoff, na.rm = TRUE)) {
      return(combined_ivore_names[which(above_cutoff)[1]])
    }
    return("Generalist")
  })
  
  for_examination <- df %>% filter(Diet_Class == "For Examination")
  for_examination$Diet_Class <- apply(for_examination[combined_diet_vars], 1, function(row) {
    above_cutoff <- row >= upper_cutoff
    if (any(above_cutoff, na.rm = TRUE)) {
      return(combined_ivore_names[which(above_cutoff)[1]])
    }
    return("For Examination")
  })
  
  df <- df %>% filter(Diet_Class != "Generalist") %>% bind_rows(generalists)
  
  df <- df %>% filter(Diet_Class != "For Examination") %>% bind_rows(for_examination)
  
  return(df)
}

classified_df <- classify_diet(Elton_full, 70, 50)

classification_summary <- classified_df %>%
  group_by(Diet_Class) %>%
  summarise(Count = n(), .groups = "drop")

print(classification_summary)

further_examination <- filter(classified_df, Diet_Class == "For Examination")
print(further_examination)

generalist <- filter(classified_df, Diet_Class == "Generalist")
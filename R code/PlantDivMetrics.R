#Paleoherbivory vs. modern paper
#Purpose: Plant diversity metrics for leafpack localities
#R version: 4.2.3
#Script author: EDC

#load packages
library(vegan)

#load datafile
plant.counts <- read.csv("rawplantcount.csv")

#Create a separate dataframe for each locality with the number of leaves of each taxon
plant.counts.bylocality <- split(plant.counts, plant.counts$locality)
list2env(plant.counts.bylocality, envir = .GlobalEnv)

HF1901_ag <- aggregate(n ~ species, data = HF1901, FUN = sum)
HF1902_ag <- aggregate(n ~ species, data = HF1902, FUN = sum)
HF1903_ag <- aggregate(n ~ species, data = HF1903, FUN = sum)

LS1901_ag <- aggregate(n ~ species, data = LS1901, FUN = sum)
LS1902_ag <- aggregate(n ~ species, data = LS1902, FUN = sum)
LS1903_ag <- aggregate(n ~ species, data = LS1903, FUN = sum)

MD1901_ag <- aggregate(n ~ species, data = MD1901, FUN = sum)
MD1902_ag <- aggregate(n ~ species, data = MD1902, FUN = sum)
MD1903_ag <- aggregate(n ~ species, data = MD1903, FUN = sum)

#Calculate Plant Diversity Metrics
#Set up matrix to hold values
PlantDivMet <- matrix(data = NA, nrow = 9, ncol=3)
row.names(PlantDivMet) <- c("HF1901", "HF1902", "HF1903", "LS1901", "LS1902", "LS1903", "MD1901", "MD1902", "MD1903")
colnames(PlantDivMet) <- c("Shannon", "Pj", "div.plant")

#compute shannon diversity index
PlantDivMet[1,1] <- diversity(HF1901_ag$n, index = "shannon")
PlantDivMet[2,1] <- diversity(HF1902_ag$n, index = "shannon")
PlantDivMet[3,1] <- diversity(HF1903_ag$n, index = "shannon")
PlantDivMet[4,1] <- diversity(LS1901_ag$n, index = "shannon")
PlantDivMet[5,1] <- diversity(LS1902_ag$n, index = "shannon")
PlantDivMet[6,1] <- diversity(LS1903_ag$n, index = "shannon")
PlantDivMet[7,1] <- diversity(MD1901_ag$n, index = "shannon")
PlantDivMet[8,1] <- diversity(MD1902_ag$n, index = "shannon")
PlantDivMet[9,1] <- diversity(MD1903_ag$n, index = "shannon")

#compute Pielou's J
PlantDivMet[1,2] <- diversity(HF1901_ag$n, index = "shannon")/log(nrow(HF1901_ag))
PlantDivMet[2,2] <- diversity(HF1902_ag$n, index = "shannon")/log(nrow(HF1902_ag))
PlantDivMet[3,2] <- diversity(HF1903_ag$n, index = "shannon")/log(nrow(HF1903_ag))
PlantDivMet[4,2] <- diversity(LS1901_ag$n, index = "shannon")/log(nrow(LS1901_ag))
PlantDivMet[5,2] <- diversity(LS1902_ag$n, index = "shannon")/log(nrow(LS1902_ag))
PlantDivMet[6,2] <- diversity(LS1903_ag$n, index = "shannon")/log(nrow(LS1903_ag))
PlantDivMet[7,2] <- diversity(MD1901_ag$n, index = "shannon")/log(nrow(MD1901_ag))
PlantDivMet[8,2] <- diversity(MD1902_ag$n, index = "shannon")/log(nrow(MD1902_ag))
PlantDivMet[9,2] <- diversity(MD1903_ag$n, index = "shannon")/log(nrow(MD1903_ag))

#Rarefy each sample to 300 leaves
PlantDivMet[1,3] <- rarefy(HF1901_ag$n, 300)
PlantDivMet[2,3] <- rarefy(HF1902_ag$n, 300)
PlantDivMet[3,3] <- rarefy(HF1903_ag$n, 300)
PlantDivMet[4,3] <- rarefy(LS1901_ag$n, 300)
PlantDivMet[5,3] <- rarefy(LS1902_ag$n, 300)
PlantDivMet[6,3] <- rarefy(LS1903_ag$n, 300)
PlantDivMet[7,3] <- rarefy(MD1901_ag$n, 300)
PlantDivMet[8,3] <- rarefy(MD1902_ag$n, 300)
PlantDivMet[9,3] <- rarefy(MD1903_ag$n, 300)
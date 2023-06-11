
# Using Biomass Package to compare ABG biomass estimates across REDD+ projects 
# with alternative allometric equations

#Installing and calling the package from the github repo (under development)###
install.packages("remotes")
remotes::install_github('umr-amap/BIOMASS')

#latest version released on CRAN
install.packages("BIOMASS")
#run to use the package
library("BIOMASS")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gghighlight)
#library(ggbreak)
library(ggpubr)
library(patchwork)
library(readr)

#To cite the package
citation("BIOMASS")

##  1396 and 1392 data #####
# DUC A01 2016 data from Amazonas

DUC_A01_2016_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/DUC_A01_2016_Inventory.csv")
duc_a01 <- data.frame(DUC_A01_2016_Inventory)
str(duc_a01) # 1089 individuals of  11 variables - total of 20 0.25-ha plots in a total of 5 ha total area

duc_a01_ed <- duc_a01 #%>% filter(scientific.name!="NA") %>% separate(scientific.name, c("genus", "species"))
duc_a01_ed$type = as.factor(duc_a01_ed$type)
names(duc_a01_ed) # 1169 individuals

names(duc_a01_ed)[6] <- 'family'
names(duc_a01_ed)[7] <- 'D'

summary(duc_a01_ed)
duc_a01_ed <- duc_a01_ed %>% filter(type == "O") #%>% filter(scientific.name!="NA") %>% separate(scientific.name, c("genus", "species"))

duc_a01_ed$plotID <- as.factor(ifelse(duc_a01_ed$plot<5, '1',
                                   ifelse(duc_a01_ed$plot == 5, '2',
                                          ifelse(duc_a01_ed$plot>5 & duc_a01_ed$plot<9, '2',
                                                 ifelse(duc_a01_ed$plot>8 & duc_a01_ed$plot<13, '3', 
                                                        ifelse(duc_a01_ed$plot>12 & duc_a01_ed$plot<17, '4','5'))))))

# print data frame

str(duc_a01_ed)

vcs1392 <- duc_a01_ed %>% filter(scientific.name!="NA") %>% separate(scientific.name, c("genus", "species"))
str(vcs1392) # 931

vcs1396 <- duc_a01_ed %>% filter(scientific.name!="NA") %>% separate(scientific.name, c("genus", "species"))
str(vcs1396) # 931

# 1359 Data####

JAM_A02_2013_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/JAM_A02_2013_Inventory.csv")
jam_a02 <- data.frame(JAM_A02_2013_Inventory)# JAM_A02_2013_Inventory Rondonia data
str(jam_a02) # 499 individuals, 17 variables - total of 24 0.25-ha plots in a total of 6 ha total area

JAM_A03_2013_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/JAM_A03_2013_Inventory.csv")
jam_a03 <- data.frame(JAM_A03_2013_Inventory)# JAM_A03_2013_Inventory Rondonia data
str(jam_a03) # 97 individuals, 21 variables - total of 4 0.25-ha plots in a total of 1 ha total area

# Manipulating jam_a02 data set
md_jam_a02 <- data.frame(jam_a02) #renaming to keep original data frame as uploaded

md_jam_a02$plotID <- as.factor(md_jam_a02$group_code)
md_jam_a02$type <- as.factor(md_jam_a02$type)

md_jam_a02 %>%
  group_by(plot) %>%
  summarise(mean = mean(DBH), n = n())

md_jam_a02_f <- md_jam_a02 %>% filter(Dead!= "D") %>% filter(type== "O")
str(md_jam_a02_f) #450 individuals

# Data wrangling to adjust species names
md_jam_a02_f <- md_jam_a02_f %>% separate(scienfic_name, c("genus", "species"))
names(md_jam_a02_f)
str(md_jam_a02_f) #1089
names(md_jam_a02_f)[7] <- 'family'
names(md_jam_a02_f)[10] <- 'D'

# Manipulating jam_a03 data set
md_jam_a03 <- data.frame(jam_a03)
md_jam_a03$plotID <- as.factor(md_jam_a03$group_code)
md_jam_a03$type <- as.factor(md_jam_a03$type)
unique(levels(md_jam_a03$type))

md_jam_a03_f <- md_jam_a03 %>% filter(Dead!= "D") %>% filter(type == "O")
str(md_jam_a03_f) # 88 individuals
unique(levels(as.factor(md_jam_a03_f$scienfic_name)))

md_jam_a03_f <- md_jam_a03_f %>% separate(scienfic_name, c("genus", "species"))
names(md_jam_a03_f)
str(md_jam_a03_f) #1089
names(md_jam_a03_f)[7] <- 'family'
names(md_jam_a03_f)[10] <- 'D'

# combine both data sets
names(md_jam_a02_f)
jam02 <- md_jam_a02_f[,c('area', 'plotID', 'genus', 'species', 'family', 
                     'D')]
jam02

names(md_jam_a03_f)
md_jam_a03_f$plotID
jam03 <- md_jam_a03_f[,c('area', 'plotID', 'genus', 'species', 'family', 
                         'D')]
jam03

# 1359 data frame
vcs_1359 <- rbind(jam02, jam03) # 7 1-ha plots
str(vcs_1359) #538 trees in 7 ha
unique(levels(vcs_1359$plotID))

vcs_1359 %>%
  group_by(plotID) %>%
  summarise(min = min(D), n = n())

## Manipulating plot data####

# Filter out NA's in D column

md_jam_a02_1359_filt <- vcs_1359 %>% drop_na(D)
str(md_jam_a02_1359_filt) # 538individuals in 7 ha
summary(md_jam_a02_1359_filt)

# Filter out dbh sizes lower than 10 cm

wet_1396_higher10 <- vcs1396 %>% filter (D > 9.999)
str(wet_1396_higher10) # 1674 trees
summary(wet_1396_higher10) #Max DBH = 98.8

md_jam_1359_higher10 <- md_jam_a02_1359_filt %>% filter (D > 9.999)
str(md_jam_1359_higher10) # 538 trees
summary(md_jam_1359_higher10) #Max DBH = 140 cm

wet_1392_higher10 <- vcs1392 %>% filter (D > 9.999)
str(wet_1392_higher10) # 984 trees
summary(wet_1392_higher10) #Max DBH = 98.8

# Checking and correcting taxonomy####

Taxo_1396 <- correctTaxo(genus = wet_1396_higher10$genus, 
                  species=wet_1396_higher10$species,
                  score = 0.5,
                  useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
wet_1396_higher10$genusCorr <- Taxo_1396$genusCorrected
wet_1396_higher10$speciesCorr <- Taxo_1396$speciesCorrected

Taxo_1392 <- correctTaxo(genus = wet_1392_higher10$genus, 
                         species=wet_1392_higher10$species,
                         score = 0.5,
                         useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
wet_1392_higher10$genusCorr <- Taxo_1392$genusCorrected
wet_1392_higher10$speciesCorr <- Taxo_1392$speciesCorrected

Taxomd_1359<-correctTaxo(genus = md_jam_1359_higher10$genus, 
                    species = md_jam_1359_higher10$species,
                    score = 0.5,
                    useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
md_jam_1359_higher10$genusCorr <- Taxomd_1359$genusCorrected
md_jam_1359_higher10$speciesCorr <- Taxomd_1359$speciesCorrected

# Retrieving APG III Families and Orders from Genus names####

APG_1396 <- getTaxonomy(wet_1396_higher10$genusCorr, findOrder = T)
wet_1396_higher10$familyAPG <- APG_1396$family
wet_1396_higher10$orderAPG <- APG_1396$order

APG_1392 <- getTaxonomy(wet_1392_higher10$genusCorr, findOrder = T)
wet_1392_higher10$familyAPG <- APG_1392$family
wet_1392_higher10$orderAPG <- APG_1392$order

APGmd_1359 <- getTaxonomy(md_jam_1359_higher10$genusCorr, findOrder = T)
md_jam_1359_higher10$familyAPG <- APGmd_1359$family
md_jam_1359_higher10$orderAPG <- APGmd_1359$order

#Getting wood density data####
#The WD can either be attributed to an individual at a species, genus, family or stand level.

# Computing the Wood Density up to the genus level 
# and then giving the mean wood density per stand

WDdata_1396 <- getWoodDensity(genus = Taxo_1396$genusCorrected, 
                       species = Taxo_1396$speciesCorrected, 
                       stand = wet_1396_higher10$plotID,
                       region = "SouthAmericaTrop")
# adding WD column to plot dataframe
wet_1396_higher10$WD <- WDdata_1396$meanWD
wet_1396_higher10$sdWD <- WDdata_1396$sdWD

WDdata_1392 <- getWoodDensity(genus = Taxo_1392$genusCorrected, 
                              species = Taxo_1392$speciesCorrected, 
                                 stand = wet_1392_higher10$plotID,
                                 region = "SouthAmericaTrop")
wet_1392_higher10$WD <- WDdata_1392$meanWD
wet_1392_higher10$sdWD <- WDdata_1392$sdWD

WDdatamd_1359<-getWoodDensity(genus = Taxomd_1359$genusCorrected, 
                         species = Taxomd_1359$speciesCorrected, 
                         stand = md_jam_1359_higher10$plotID,
                         region = "SouthAmericaTrop")
md_jam_1359_higher10$WD <- WDdatamd_1359$meanWD
md_jam_1359_higher10$sdWD <- WDdatamd_1359$sdWD

#Tree Height ####

#Retrieve height data##

# If the site misses tree height measurements, 
# it is used a continent or region specific H-D model
# or generic H-D model based on single bio-climatic predictor E
# from Chavel et al 2014, retrieved by combining lat and long data

# Retrieve height data from a Feldpaush et al. (2012) averaged model

dataHfeld_1396 <- retrieveH(D = wet_1396_higher10$D, region = "SAmerica")
wet_1396_higher10$Hfeld <- dataHfeld_1396$H
wet_1396_higher10$HfeldRSE <- dataHfeld_1396$RSE

dataHfeld_1392 <- retrieveH(D = wet_1392_higher10$D, region = "SAmerica")
wet_1392_higher10$Hfeld <- dataHfeld_1392$H
wet_1392_higher10$HfeldRSE <- dataHfeld_1392$RSE

dataHfeld_1359 <- retrieveH(D = md_jam_1359_higher10$D, region = "SAmerica")
md_jam_1359_higher10$Hfeld <- dataHfeld_1359$H
md_jam_1359_higher10$HfeldRSE <- dataHfeld_1359$RSE

# Using coordinates to obtain height data
# from Chave et al. (2012) equation 6

# 1359 
long <- -63.00
lat <- -9.13
coord_jam <- c(long, lat)
coord_jam
md_jam_1359_higher10$lat <- rep(-9.13)
md_jam_1359_higher10$long <- rep(-63.00)

dataHchave_1359 <- retrieveH(D = md_jam_1359_higher10$D,
                             coord = coord_jam)#, #cbind(md_brana01_higher10$lat, md_brana01_higher10$long))#,
#region = "SAmerica")
md_jam_1359_higher10$Hchave<- dataHchave_1359$H

# 1396
dataHchave_1396 <- retrieveH(D = wet_1396_higher10$D,
                        coord = cbind(wet_1396_higher10$UTM.Northing, wet_1396_higher10$UTM.Easting))
wet_1396_higher10$Hchave<- dataHchave_1396$H

# 1392
dataHchave_1392 <- retrieveH(D = wet_1392_higher10$D,
                             coord = cbind(wet_1392_higher10$UTM.Northing, wet_1392_higher10$UTM.Easting))
wet_1392_higher10$Hchave<- dataHchave_1392$H

####
# Once diameter, wood density and height values ##
# have been retrieved for each tree ##
###

#Calculate AGB###

# Compute AGB(Mg) per plot Chave 2014 #####
AGBPlot_Hfeld_1396<- by(wet_1396_higher10, wet_1396_higher10$plotID,
                        function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                        simplify = F)
AGBplot_D_Hf_WD_1396 <- sapply(AGBPlot_Hfeld_1396, sum)
AGBplot_D_Hf_WD_1396 # Mg per 1 ha

AGBplot_D_Hf_WD_1396_c <- sapply(AGBPlot_Hfeld_1396, length)

# Calculating carbon for mean and CI using error propagation
AGB_MgC_ha_alt1_1396 = (AGBplot_D_Hf_WD_1396) * 0.456
AGB_MgC_ha_alt1_1396
SDAGB_alt1_1396 = AGB_MgC_ha_alt1_1396 * 0.20
SEAGB_alt1_1396 = SDAGB_alt1_1396/sqrt(AGBplot_D_Hf_WD_1396_c)
HighCI_alt1_1396 = AGB_MgC_ha_alt1_1396 + (1.96*SEAGB_alt1_1396)
LowCI_alt1_1396 = AGB_MgC_ha_alt1_1396 - (1.96*SEAGB_alt1_1396)

Wet_AGBPlot_Chave2014_Hf_1396 <-  data.frame (PlotID = unique(levels(wet_1396_higher10$plotID)),
                                              AGB_MgC_ha = AGB_MgC_ha_alt1_1396,
                                              HighCI = HighCI_alt1_1396,
                                              LowCI = LowCI_alt1_1396,
                                              Project_ID = rep("VCS1396"),
                                              EquationType = rep("Chave (2014) [6]"))
Wet_AGBPlot_Chave2014_Hf_1396

p1396_er <- data.frame(Wet_AGBPlot_Chave2014_Hf_1396 %>%
                      summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                se = sd(AGB_MgC_ha_alt1_1396)/sqrt(n()),
                                HCI = mAGB_MgC_ha + (1.96*se), 
                                LCI = mAGB_MgC_ha - (1.96*se),
                                N = n()), 
                      Project_ID = rep("VCS1396"),
                      EquationType = rep("Chave (2014) [6]"))
p1396_er

# 1392 ###

AGBPlot_Hfeld_1392<- by(wet_1392_higher10, wet_1392_higher10$plotID,
                        function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                        simplify = F)
AGBplot_D_Hf_WD_1392 <- sapply(AGBPlot_Hfeld_1392, sum)
AGBplot_D_Hf_WD_1392 # Mg per 1 ha

AGBplot_D_Hf_WD_1392_c <- sapply(AGBPlot_Hfeld_1392, length)

# Calculating carbon for mean and CI 
AGB_MgC_ha_alt1_1392 = (AGBplot_D_Hf_WD_1392) * 0.456
SDAGB_alt1_1392 = AGB_MgC_ha_alt1_1392 * 0.20
SEAGB_alt1_1392 = SDAGB_alt1_1392/sqrt(AGBplot_D_Hf_WD_1392_c)
HighCI_alt1_1392 = AGB_MgC_ha_alt1_1392 + (1.96*SEAGB_alt1_1392)
LowCI_alt1_1392 = AGB_MgC_ha_alt1_1392 - (1.96*SEAGB_alt1_1392)

Wet_AGBPlot_Chave2014_Hf_1392 <-  data.frame (PlotID = unique(levels(wet_1392_higher10$plotID)),
                                              AGB_MgC_ha = AGB_MgC_ha_alt1_1392,
                                              HighCI = HighCI_alt1_1392,
                                              LowCI = LowCI_alt1_1392,
                                              Project_ID = rep("VCS1392"),
                                              EquationType = rep("Chave (2014) [6]"))
Wet_AGBPlot_Chave2014_Hf_1392

p1392_er <- data.frame(Wet_AGBPlot_Chave2014_Hf_1392 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1_1392),
                                   se = sd(AGB_MgC_ha_alt1_1392)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1392"),
                       EquationType = rep("Chave (2014) [6]"))
p1392_er

# 1359 ###
AGBPlot_Hfeld_1359<- by(md_jam_1359_higher10, md_jam_1359_higher10$plotID,
                        function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                        simplify = F)
AGBplot_D_Hf_WD_1359 <- sapply(AGBPlot_Hfeld_1359, sum)
AGBplot_D_Hf_WD_1359 # Mg per 1 ha

AGBplot_D_Hf_WD_1359_c <- sapply(AGBPlot_Hfeld_1359, length)

# Calculating carbon for mean and CI 
AGB_MgC_ha_alt1_1359 = (AGBplot_D_Hf_WD_1359) * 0.456
SDAGB_alt1_1359 = AGB_MgC_ha_alt1_1359 * 0.20
SEAGB_alt1_1359 = SDAGB_alt1_1359/sqrt(AGBplot_D_Hf_WD_1359_c)
HighCI_alt1_1359 = AGB_MgC_ha_alt1_1359 + (1.96*SEAGB_alt1_1359)
LowCI_alt1_1359 = AGB_MgC_ha_alt1_1359 - (1.96*SEAGB_alt1_1359)

Dry_AGBPlot_Chave2014_Hf_1359 <-  data.frame (PlotID = unique(levels(md_jam_1359_higher10$plotID)),
                                              AGB_MgC_ha = AGB_MgC_ha_alt1_1359,
                                              HighCI = HighCI_alt1_1359,
                                              LowCI = LowCI_alt1_1359,
                                              Project_ID = rep("VCS1359"),
                                              EquationType = rep("Chave (2014) [6]"))
Dry_AGBPlot_Chave2014_Hf_1359

p1359_er <- data.frame(Dry_AGBPlot_Chave2014_Hf_1359 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1_1359),
                                   se = sd(AGB_MgC_ha_alt1_1359)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1359"),
                       EquationType = rep("Chave (2014) [6]"))
p1359_er

## Compute AGB(Mg) using Chave 2005 forest type specific ####

# AGB = 0.0776 * (WD * D^2 * H))^0.940 Chave 2005 type I wet

VCS1396_AGB_function_r5 <- function(WD, D, H) { # create a function with the name my_function
  AGB = 0.0776 * (WD * D^2 * H)^0.940 # correction factor already embedded in Chave equation
  print(AGB)
}
AGBPlot_r5_1396 <- by(wet_1396_higher10, wet_1396_higher10$plotID,
                            function (x) VCS1396_AGB_function_r5(WD = x$WD, D = x$D,H = x$Hfeld),
                            simplify = F)
AGBPlot_r5_1396
AGBplot_D_WD_1396 <- sapply(AGBPlot_r5_1396, sum)
AGBplot_D_WD_1396_c <- sapply(AGBPlot_r5_1396, length)

# Calculating carbon for mean and CI using error propagation
AGB_MgC_ha_alt2_1396 = (AGBplot_D_WD_1396/1000) * 0.456
SDAGB_alt2_1396 = AGB_MgC_ha_alt2_1396 * 0.20
SEAGB_alt2_1396 = SDAGB_alt2_1396/sqrt(AGBplot_D_WD_1396_c)
HighCI_alt2_1396 = AGB_MgC_ha_alt2_1396 + (1.96*SEAGB_alt2_1396)
LowCI_alt2_1396 = AGB_MgC_ha_alt2_1396 - (1.96*SEAGB_alt2_1396)

Wet_AGBPlot_Chave2005_r5_1396 <-  data.frame (PlotID = unique(levels(wet_1396_higher10$plotID)),
                                              AGB_MgC_ha = AGB_MgC_ha_alt2_1396,
                                              HighCI = HighCI_alt2_1396,
                                              LowCI = LowCI_alt2_1396,
                                              Project_ID = rep("VCS1396"),
                                              EquationType = rep("Chave (2005) [5]"))
Wet_AGBPlot_Chave2005_r5_1396

p1396_r5 <- data.frame(Wet_AGBPlot_Chave2005_r5_1396 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt2_1396),
                                   se = sd(AGB_MgC_ha_alt2_1396)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1396"),
                       EquationType = rep("Chave (2005) [5]"))
p1396_r5

# 1392 ###

VCS1392_AGB_function_r5 <- function(D, WD) { # Chave et al 2005 - II.5 wet forest model
  AGB = exp(-1.554 + 2.420 * log(D) + log(WD))# correction factor already includes correction factor
  print(AGB)
}
AGBPlot_r5_1392 <- by(wet_1392_higher10, wet_1392_higher10$plotID,
                      function (x) VCS1392_AGB_function_r5(D = x$D, WD = x$WD),
                      simplify = F)
AGBplot_D_WD_1392 <- sapply(AGBPlot_r5_1392, sum)
AGBplot_D_WD_1392 # Mg per 1 ha #aboveground biomass in kg

AGBplot_D_WD_1392_c <- sapply(AGBPlot_r5_1392, length)

# Calculating carbon for mean and CI
AGB_MgC_ha_alt2_1392 = (AGBplot_D_WD_1392/1000) * 0.456
SDAGB_alt2_1392 = AGB_MgC_ha_alt2_1392 * 0.20
SEAGB_alt2_1392 = SDAGB_alt2_1392/sqrt(AGBplot_D_WD_1392_c)
HighCI_alt2_1392 = AGB_MgC_ha_alt2_1392 + (1.96*SEAGB_alt2_1392)
LowCI_alt2_1392 = AGB_MgC_ha_alt2_1392 - (1.96*SEAGB_alt2_1392)

Wet_AGBPlot_Chave2005_r5_1392 <-  data.frame (PlotID = unique(levels(wet_1392_higher10$plotID)),
                                              AGB_MgC_ha = AGB_MgC_ha_alt2_1392,
                                              HighCI = HighCI_alt2_1392,
                                              LowCI = LowCI_alt2_1392,
                                              Project_ID = rep("VCS1392"),
                                              EquationType = rep("Chave (2005) [5]"))
Wet_AGBPlot_Chave2005_r5_1392

p1392_r5 <- data.frame(Wet_AGBPlot_Chave2005_r5_1392 %>%
                              summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt2_1392),
                                        se = mAGB_MgC_ha * 0.24,
                                        HCI = mAGB_MgC_ha + (1.96*se), 
                                        LCI = mAGB_MgC_ha - (1.96*se),
                                        N = n()), 
                            Project_ID = rep("VCS1392"),
                            EquationType = rep("Chave (2005) [5]"))
p1392_r5

# 1359

#AGB dry model in Chave et al (2005)

VCS1359_AGB_function_r5 <- function(WD,D,H) { # create a function with the name my_function
  AGB = 0.112 * (WD * D^2 * H)^0.916
  print(AGB)
}
AGBPlot_r5_1359 <- by(md_jam_1359_higher10, md_jam_1359_higher10$plotID,
                      function (x) VCS1359_AGB_function_r5(WD = x$WD, D = x$D, H = x$Hfeld),
                      simplify = F)
AGBplot_D_WD_1359 <- sapply(AGBPlot_r5_1359, sum)
AGBplot_D_WD_1359 # in kg

AGBplot_D_WD_1359_c <- sapply(AGBPlot_r5_1359, length)

# Calculating carbon for mean and CI
AGB_MgC_ha_alt2_1359 = (AGBplot_D_WD_1359/1000) * 0.456
SDAGB_alt2_1359 = AGB_MgC_ha_alt2_1359 * 0.20
SEAGB_alt2_1359 = SDAGB_alt2_1359/sqrt(AGBplot_D_WD_1359_c)
HighCI_alt2_1359 = AGB_MgC_ha_alt2_1359 + (1.96*SEAGB_alt2_1359)
LowCI_alt2_1359 = AGB_MgC_ha_alt2_1359 - (1.96*SEAGB_alt2_1359)

# combining into data frame ###
Dry_AGBPlot_Chave2005_r5_1359 <-  data.frame (PlotID = unique(levels(md_jam_1359_higher10$plotID)),
                                               AGB_MgC_ha = AGB_MgC_ha_alt2_1359,
                                               HighCI = HighCI_alt2_1359,
                                               LowCI = LowCI_alt2_1359,
                                               Project_ID = rep("VCS1359"),
                                               EquationType = rep("Chave (2005) [5]"))
Dry_AGBPlot_Chave2005_r5_1359

p1359_r5 <- data.frame(Dry_AGBPlot_Chave2005_r5_1359 %>%
                              summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt2_1359),
                                        se = mAGB_MgC_ha * 0.24,
                                        HCI = mAGB_MgC_ha + (1.96*se), 
                                        LCI = mAGB_MgC_ha - (1.96*se),
                                        N = n()), 
                            Project_ID = rep("VCS1359"),
                            EquationType = rep("Chave (2005) [5]"))
p1359_r5

## Calculating AGB with Project equation ##

# Project VCS1396 (Colombia) ####

VCS1396_AGB_function <- function(D,H,WD) { # create a function with the name my_function
  AGB = exp(-2.130 + 2.015*log(D) + 0.724*log(H) + 1.002*log(WD)) * exp((0.336^2)/2) # the latter is the correction factor
  print(AGB)
}
VCS1396_AGBPlot_projv1 <- by(wet_1396_higher10, wet_1396_higher10$plotID,
                            function (x) VCS1396_AGB_function(D = x$D, H = x$Hfeld, WD = x$WD),
                            simplify = F)
VCS1396_AGBPlot_Proj <- sapply(VCS1396_AGBPlot_projv1, sum)
VCS1396_AGBPlot_Proj_c <- sapply(VCS1396_AGBPlot_projv1, length)

# Calculating carbon for mean and CI
AGB_MgC_ha_proj_1396 = (VCS1396_AGBPlot_Proj/1000) * 0.485 # carbon fraction used by the project
SDAGB_proj_1396 = AGB_MgC_ha_proj_1396 * 0.20
SEAGB_proj_1396 = SDAGB_proj_1396/sqrt(VCS1396_AGBPlot_Proj_c)#calculate the length of the list
HighCI_proj_1396 = AGB_MgC_ha_proj_1396 + (1.96*SEAGB_proj_1396)
LowCI_proj_1396 = AGB_MgC_ha_proj_1396 - (1.96*SEAGB_proj_1396)

Wet_AGBPlot_proj_1396 <-  data.frame (PlotID = unique(levels(wet_1396_higher10$plotID)),
                                              AGB_MgC_ha = AGB_MgC_ha_proj_1396,
                                              HighCI = HighCI_proj_1396,
                                              LowCI = LowCI_proj_1396,
                                              Project_ID = rep("VCS1396"),
                                              EquationType = rep("Project"))
Wet_AGBPlot_proj_1396

p1396_pr <- data.frame(Wet_AGBPlot_proj_1396 %>%
                              summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_proj_1396),
                                        se = sd(AGB_MgC_ha_proj_1396)/sqrt(n()),
                                        HCI = mAGB_MgC_ha + (1.96*se), 
                                        LCI = mAGB_MgC_ha - (1.96*se),
                                        N = n()), 
                            Project_ID = rep("VCS1396"),
                            EquationType = rep("Project [3]"))
p1396_pr

## Alternative equations for VCS1396 ####

## 1396 Alt 1 - Duque et al 2017 equation ####

VCS1396_AGB_alt_function1 <- function(D,H,WD) { 
  AGB = (0.089 * (D^2 * H * WD)^0.951) * exp((0.340^2)/2) # the latter is the correction factor
  print(AGB)
}
VCS1396_AGB_Alt_1 <- by(wet_1396_higher10, wet_1396_higher10$plotID,
                                function (x) VCS1396_AGB_alt_function1(D = x$D, H = x$Hfeld, WD = x$WD),
                                simplify = F)
VCS1396_AGB_alt1 <- sapply(VCS1396_AGB_Alt_1, sum)
VCS1396_AGB_alt1_c <- sapply(VCS1396_AGB_Alt_1, length)

# Calculating carbon for mean and CI
AGB_MgC_ha_alt1_1396 = (VCS1396_AGB_alt1/1000) * 0.456 # carbon fraction by Martin 2018
SDAGB_alt1.1_1396 = AGB_MgC_ha_alt1_1396 * 0.20
SEAGB_alt1.1_1396 = SDAGB_alt1.1_1396/sqrt(VCS1396_AGB_alt1_c)
SEAGB_alt1.1_1396
HighCI_alt1.1a_1396 = AGB_MgC_ha_alt1_1396 + (1.96*SEAGB_alt1.1_1396)
LowCI_alt1.1a_1396 = AGB_MgC_ha_alt1_1396 - (1.96*SEAGB_alt1.1_1396)

Wet_AGBPlot_alt1_1396 <-  data.frame (PlotID = unique(levels(wet_1396_higher10$plotID)),
                                         AGB_MgC_ha = AGB_MgC_ha_alt1_1396,
                                         HighCI = HighCI_alt1.1a_1396,
                                         LowCI = LowCI_alt1.1a_1396,
                                         Project_ID = rep("VCS1396"),
                                         EquationType = rep("Duque (2017) [4]"))
Wet_AGBPlot_alt1_1396

p1396_alt1 <- data.frame(Wet_AGBPlot_alt1_1396 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1.1a_1396),
                                   se = sd(AGB_MgC_ha_alt1.1a_1396)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1396"),
                       EquationType = rep("Duque (2017) [4]"))
p1396_alt1

## 1396 Alt 2 - Alvarez (2012) equation ####
# Alvarez wet equation II.1

VCS1396_AGB_alt_function2 <- function(D,WD) { # create a function with the name my_function
  AGB = exp(1.662-(1.114*log(D))+(1.169*log(D)^2)-(0.122*log(D)^3) +(0.331*log(WD))) * exp((0.336^2)/2) # the latter is the correction factor
  print(AGB)
}
VCS1396_AGB_Alt_2 <- by(wet_1396_higher10, wet_1396_higher10$plotID,
                       function (x) VCS1396_AGB_alt_function2(D = x$D, WD = x$WD),
                       simplify = F)
VCS1396_AGB_Alt_2
VCS1396_AGB_alt2 <- sapply(VCS1396_AGB_Alt_2, sum)
VCS1396_AGB_alt2_c <- sapply(VCS1396_AGB_Alt_2, length)

# Calculating carbon for mean and CI
AGB_MgC_ha_alt2_1396 = (VCS1396_AGB_alt2/1000) * 0.456 # carbon fraction by Martin 2018
SDAGB_alt2_1396 = AGB_MgC_ha_alt2_1396 * 0.20
SEAGB_alt2_1396 = SDAGB_alt2_1396/sqrt(VCS1396_AGB_alt2_c)
HighCI_alt2_1396 = AGB_MgC_ha_alt2_1396 + (1.96*SEAGB_alt2_1396)
LowCI_alt2_1396 = AGB_MgC_ha_alt2_1396 - (1.96*SEAGB_alt2_1396)

Wet_AGBPlot_alt2_1396 <-  data.frame (PlotID = unique(levels(wet_1396_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt2_1396,
                                      HighCI = HighCI_alt2_1396,
                                      LowCI = LowCI_alt2_1396,
                                      Project_ID = rep("VCS1396"),
                                      EquationType = rep("Alvarez (2012) [3]"))
Wet_AGBPlot_alt2_1396

p1396_alt2 <- data.frame(Wet_AGBPlot_alt2_1396 %>%
                                      summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt2_1396),
                                                se = sd(AGB_MgC_ha_alt2_1396)/sqrt(n()),
                                                HCI = mAGB_MgC_ha + (1.96*se), 
                                                LCI = mAGB_MgC_ha - (1.96*se),
                                                N = n()), 
                                    Project_ID = rep("VCS1396"),
                                    EquationType = rep("Alvarez (2012) [3]"))
p1396_alt2

## 1396 Alt 3 - Alvarez (2012)-II.5 equation ####

VCS1396_AGB_alt_function3 <- function(D,WD) {
  AGB = exp(-1.482 + (2.499 * log(D)) + log(WD)) * exp((0.364^2)/2)
  print(AGB)
}
VCS1396_AGB_Alt_3 <- by(wet_1396_higher10, wet_1396_higher10$plotID,
                       function (x) VCS1396_AGB_alt_function3(D = x$D, WD = x$WD),
                       simplify = F)
VCS1396_AGB_alt3 <- sapply(VCS1396_AGB_Alt_3, sum)
VCS1396_AGB_alt3_c <- sapply(VCS1396_AGB_Alt_3, length)

# Calculating carbon for mean and CI
AGB_MgC_ha_alt3_1396 = (VCS1396_AGB_alt3/1000) * 0.456 # carbon fraction by Martin 2018
SDAGB_alt3_1396 = AGB_MgC_ha_alt3_1396 * 0.20
SEAGB_alt3_1396 = SDAGB_alt3_1396/sqrt(VCS1396_AGB_alt3_c)
HighCI_alt3_1396 = AGB_MgC_ha_alt3_1396 + (1.96*SEAGB_alt3_1396)
LowCI_alt3_1396 = AGB_MgC_ha_alt3_1396 - (1.96*SEAGB_alt3_1396)

Wet_AGBPlot_alt3_1396 <-  data.frame (PlotID = unique(levels(wet_1396_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt3_1396,
                                      HighCI = HighCI_alt3_1396,
                                      LowCI = LowCI_alt3_1396,
                                      Project_ID = rep("VCS1396"),
                                      EquationType = rep("Alvarez (2012)-II.5 [3]"))
Wet_AGBPlot_alt3_1396

p1396_alt3 <- data.frame(Wet_AGBPlot_alt4_1396 %>%
                                   summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt4_1396),
                                             se = sd(AGB_MgC_ha_alt4_1396)/sqrt(n()),
                                             HCI = mAGB_MgC_ha + (1.96*se), 
                                             LCI = mAGB_MgC_ha - (1.96*se),
                                             N = n()), 
                                 Project_ID = rep("VCS1396"),
                                 EquationType = rep("Alvarez (2012)-II.5 [3]"))
p1396_alt3

## 1396 Alt 4 - Sierra et al. 2007 ####
#(Rank 4, DBH > 1) AGB = exp(-2.286 + 2.471 * log(D))

VCS1396_AGB_alt_function4 <- function(D) { 
  AGB = exp(-2.286 + (2.471 * log(D))) * 0.091 # the latter is the correction factor and has been added to the equation based on Sierra et al 2007
  print(AGB)
}
VCS1396_AGB_Alt_4 <- by(wet_1396_higher10, wet_1396_higher10$plotID,
                       function (x) VCS1396_AGB_alt_function4(D = x$D),
                       simplify = F)
VCS1396_AGB_alt4 <- sapply(VCS1396_AGB_Alt_4, sum)
VCS1396_AGB_alt4 # in kg

VCS1396_AGB_alt4_c <- sapply(VCS1396_AGB_alt4, length)

# Calculating carbon for mean and CI
AGB_MgC_ha_alt4_1396 = (VCS1396_AGB_alt4/1000) * 0.456 # carbon fraction by Martin 2018
SDAGB_alt4_1396 = AGB_MgC_ha_alt4_1396 * 0.20
SEAGB_alt4_1396 = SDAGB_alt4_1396/sqrt(VCS1396_AGB_alt4_c)
HighCI_alt4_1396 = AGB_MgC_ha_alt4_1396 + (1.96*SEAGB_alt4_1396)
LowCI_alt4_1396 = AGB_MgC_ha_alt4_1396 - (1.96*SEAGB_alt4_1396)

Wet_AGBPlot_alt4_1396 <-  data.frame (PlotID = unique(levels(wet_1396_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt4_1396,
                                      Project_ID = rep("VCS1396"),
                                      EquationType = rep("Sierra (2007) [4]"))
Wet_AGBPlot_alt4_1396

p1396_alt4 <- data.frame(Wet_AGBPlot_alt5_1396 %>%
                                   summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt5_1396),
                                             se = sd(AGB_MgC_ha_alt5_1396)/sqrt(n()),
                                             HCI = mAGB_MgC_ha + (1.96*se), 
                                             LCI = mAGB_MgC_ha - (1.96*se),
                                             N = n()), 
                                 Project_ID = rep("VCS1396"),
                                 EquationType = rep("Sierra (2007) [4]"))
p1396_alt4

## 1396 Compiling data for plotting ####

VCS1396_AGB_Data2 <- rbind(
  p1396_er,
  p1396_r5,
  p1396_alt1,
  p1396_alt2,
  p1396_alt3,
  p1396_alt4)
VCS1396_AGB_Data2

#Calcularing tCO2e/ha
vcs1396_toplot2 <-VCS1396_AGB_Data2

vcs1396_toplot2$AGC_tCO2_ha = as.numeric(vcs1396_toplot2$mAGB_MgC_ha * (44/12))
vcs1396_toplot2$HCI_tCO2_ha = as.numeric(vcs1396_toplot2$HCI * (44/12))
vcs1396_toplot2$LCI_tCO2_ha = as.numeric(vcs1396_toplot2$LCI * (44/12))
vcs1396_toplot2$EquationType = as.factor(vcs1396_toplot2$EquationType)

vcs1396_toplot2

# 1396 plotting ####

p_1396 <- ggplot(vcs1396_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha, decreasing=FALSE), 
                                   y = AGC_tCO2_ha, 
                                   #ymin = LCI_tCO2_ha, 
                                   #ymax = HCI_tCO2_ha, 
                                   color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,300)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS1396 (VM0006)", 
       subtitle = "5 1-ha wet forest plots")+
  theme_pubr()+
  #coord_flip()+#scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0006] VCS1396 Colombia")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))
p_1396 

# Project / Alternative stats ####

VCS1396_AGB_Data3 <- rbind(
  p1396_er,
  p1396_r5,
  p1396_alt1,
  p1396_alt2,
  p1396_alt3,
  p1396_alt4,
  p1396_pr)
VCS1396_AGB_Data3

vcs1396_toplot3 <-VCS1396_AGB_Data3

vcs1396_toplot3$AGC_tCO2_ha = as.numeric(vcs1396_toplot3$mAGB_MgC_ha * (44/12))
vcs1396_toplot3$HCI_tCO2_ha = as.numeric(vcs1396_toplot3$HCI * (44/12))
vcs1396_toplot3$LCI_tCO2_ha = as.numeric(vcs1396_toplot3$LCI * (44/12))
vcs1396_toplot3$EquationType = as.factor(vcs1396_toplot3$EquationType)

vcs1396_toplot3$condition <- c(rep("Alternative",6), rep("Project", 1))
vcs1396_toplot3

#vcs1396_toplot3_pr <- vcs1396_toplot3 %>% filter(EquationType == "Project [3]")
#vcs1396_toplot3_pr

# New plot for new Figure 2 including project's estimate as a red dot

p_1396_vred <- ggplot(vcs1396_toplot3, aes(x = reorder(EquationType, AGC_tCO2_ha, decreasing=FALSE), 
                                      y = AGC_tCO2_ha)) +
  geom_point(aes(fill = condition),
             alpha = 0.9, size = 4, color = "black", shape = 21)+ylim(0,300)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "", fill = "", title = "VCS1396 (VM0006)", 
       subtitle = "5 1-ha wet forest plots")+
  scale_fill_manual(values = c("darkgrey", "red"), limits = c("Alternative", "Project"))+
  theme_pubr()+
  ggtitle("[VM0006] VCS1396 Colombia")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"),
        legend.position = "none")
p_1396_vred

## Project VCS1392 (Colombia) ####
# Equation used ln(AGB) = -2.130+2.015*ln(D)+0.724*ln(H)+1.002*ln(WD)

VCS1392_AGB_function <- function(D,H,WD) {
  AGB = exp(-2.130 + 2.015*log(D) + 0.724*log(H) + 1.002*log(WD)) * exp((0.336^2)/2)
  print(AGB)
}
VCS1392_AGBPlot_projv1 <- by(wet_1392_higher10, wet_1392_higher10$plotID,
                            function (x) VCS1392_AGB_function(D = x$D, H = x$Hfeld, WD = x$WD),
                            simplify = F)
VCS1392_AGBPlot_Proj <- sapply(VCS1392_AGBPlot_projv1, sum)
VCS1392_AGBPlot_Proj 

VCS1392_AGBPlot_Proj_c <- sapply(VCS1392_AGBPlot_projv1, length)
VCS1392_AGBPlot_Proj_c

AGB_MgC_ha_proj_1392 = (VCS1392_AGBPlot_Proj/1000) * 0.485 # carbon fraction used by the project
AGB_MgC_ha_proj_1392
SDAGB_proj_1392 = AGB_MgC_ha_proj_1392 * 0.20
SEAGB_proj_1392 = SDAGB_proj_1392/sqrt(VCS1392_AGBPlot_Proj_c)
HighCI_proj_1392 = AGB_MgC_ha_proj_1392 + (1.96*SEAGB_proj_1392)
LowCI_proj_1392 = AGB_MgC_ha_proj_1392 - (1.96*SEAGB_proj_1392)

Wet_AGBPlot_proj_1392 <-  data.frame (PlotID = unique(levels(wet_1392_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_proj_1392,
                                      HighCI = HighCI_proj_1392,
                                      LowCI = LowCI_proj_1392,
                                      Project_ID = rep("VCS1392"),
                                      EquationType = rep("Project"))
Wet_AGBPlot_proj_1392

p1392_pr <- data.frame(Wet_AGBPlot_proj_1392 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1392"),
                       EquationType = rep("Project [3]"))
p1392_pr

## Alternative equation 1 VCS1392 ####
### Duque et al 2017 ##

VCS1392_AGB_alt_function1a <- function(D,H,WD) { 
  AGB = 0.089 * (D^2 * H * WD)^0.951 * exp((0.340^2)/2)
  print(AGB)
}
VCS1392_AGB_alt1.1 <- by(wet_1392_higher10, wet_1392_higher10$plotID,
                                      function (x) VCS1392_AGB_alt_function1a(D = x$D, H = x$Hfeld, WD = x$WD),
                                      simplify = F)
VCS1392_AGB_alt1.1a_Feld <- sapply(VCS1392_AGB_alt1.1, sum)
VCS1392_AGB_alt1.1a_Feld 

AGBplot_D_WD_1392_m <- sapply(VCS1392_AGB_alt1.1, mean)
AGBplot_D_WD_1392_m 

AGBplot_D_WD_1392_c <- sapply(VCS1392_AGB_alt1.1, length)
AGBplot_D_WD_1392_c

AGB_MgC_ha_alt1.1a_1392 = (VCS1392_AGB_alt1.1a_Feld/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt1.1a_1392
SDAGB_alt1.1_1392 = AGB_MgC_ha_alt1.1a_1392 * 0.20
SDAGB_alt1.1_1392
SEAGB_alt1.1_1392 = SDAGB_alt1.1_1392/sqrt(AGBplot_D_WD_1392_c)
SEAGB_alt1.1_1392
HighCI_alt1.1a_1392 = AGB_MgC_ha_alt1.1a_1392 + (1.96*SEAGB_alt1.1_1392)
LowCI_alt1.1a_1392 = AGB_MgC_ha_alt1.1a_1392 - (1.96*SEAGB_alt1.1_1392)

Wet_AGBPlot_alt1.1a_1392 <-  data.frame (PlotID = unique(levels(wet_1392_higher10$plotID)),
                                        AGB_MgC_ha = AGB_MgC_ha_alt1.1a_1392,
                                        HighCI = HighCI_alt1.1a_1392,
                                        LowCI = LowCI_alt1.1a_1392,
                                        Project_ID = rep("VCS1392"),
                                        EquationType = rep("Duque (2017) [4]"))
Wet_AGBPlot_alt1.1a_1392

p1392_er_alt1.1a <- data.frame(Wet_AGBPlot_alt1.1a_1392 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1.1a_1392),
                                   se = mAGB_MgC_ha * 0.24,
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1392"),
                       EquationType = rep("Duque (2017) [4]"))
p1392_er_alt1.1a

# Alternative equation 2 1392 #####
#Alvarez 2012 eq II.5 
# AGB = exp(-1.482 + (2.499* log(DBH)) + log(WD))

VCS1392_AGB_alt_function2 <- function(D, WD) { 
  AGB = exp(-1.482 + (2.499* log(D)) + log(WD)) * exp((0.336^2)/2)
  print(AGB)
}
VCS1392_AGB_alt2.1 <- by(wet_1392_higher10, wet_1392_higher10$plotID,
                               function (x) VCS1392_AGB_alt_function2(D = x$D, WD = x$WD),
                               simplify = F)
VCS1392_AGB_alt_2.1 <- sapply(VCS1392_AGB_alt2.1, sum)
VCS1392_AGB_alt_2.1 

VCS1392_AGB_alt_2.1_c <- sapply(VCS1392_AGB_alt2.1, length)
VCS1392_AGB_alt_2.1_c

AGB_MgC_ha_alt2.1_1392 = (VCS1392_AGB_alt_2.1/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt2.1_1392
SDAGB_alt2.1_1392 = AGB_MgC_ha_alt2.1_1392 * 0.20
SDAGB_alt2.1_1392
SEAGB_alt2.1_1392 = SDAGB_alt2.1_1392/sqrt(VCS1392_AGB_alt_2.1_c)
SEAGB_alt2.1_1392
HighCI_alt2.1_1392 = AGB_MgC_ha_alt2.1_1392 + (1.96*SEAGB_alt2.1_1392)
LowCI_alt2.1_1392 = AGB_MgC_ha_alt2.1_1392 - (1.96*SEAGB_alt2.1_1392)

# combining into project data frame ###
Wet_AGBPlot_alt2.1_1392 <-  data.frame (PlotID = unique(levels(wet_1392_higher10$plotID)),
                                        AGB_MgC_ha = AGB_MgC_ha_alt2.1_1392,
                                        HighCI = HighCI_alt2.1_1392,
                                        LowCI = LowCI_alt2.1_1392,
                                        Project_ID = rep("VCS1392"),
                                        EquationType = rep("Alvarez (2012)-II.5 [3]"))
Wet_AGBPlot_alt2.1_1392

p1392_er_alt2.1a <- data.frame(Wet_AGBPlot_alt2.1_1392 %>%
                                 summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt2.1_1392),
                                           se = mAGB_MgC_ha * 0.24,
                                           HCI = mAGB_MgC_ha + (1.96*se), 
                                           LCI = mAGB_MgC_ha - (1.96*se),
                                           N = n()), 
                               Project_ID = rep("VCS1392"),
                               EquationType = rep("Alvarez (2012)-II.5 [3]"))
p1392_er_alt2.1a

# Alternative eq 3 1392 ####

# Alvarez et al 2012 II.1
VCS1392_AGB_alt_function3 <- function(D,WD) { # create a function with the name my_function
  AGB = exp(1.662-(1.114*log(D))+(1.169*log(D)^2)-(0.122*log(D)^3) +(0.331*log(WD))) * exp((0.336^2)/2)
  print(AGB)
}
VCS1392_AGB_alt3 <- by(wet_1392_higher10, wet_1392_higher10$plotID,
                         function (x) VCS1392_AGB_alt_function3(D = x$D, WD = x$WD),
                         simplify = F)
VCS1392_AGB_Alt3 <- sapply(VCS1392_AGB_alt3, sum)
VCS1392_AGB_Alt3 

VCS1392_AGB_alt3_c <- sapply(VCS1392_AGB_alt3, length)
VCS1392_AGB_alt3_c

AGB_MgC_ha_alt3_1392 = (VCS1392_AGB_Alt3/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt3_1392
SDAGB_alt3_1392 = AGB_MgC_ha_alt3_1392 * 0.20
SDAGB_alt3_1392
SEAGB_alt3_1392 = SDAGB_alt3_1392/sqrt(VCS1392_AGB_alt3_c)
SEAGB_alt3_1392
HighCI_alt3_1392 = AGB_MgC_ha_alt3_1392 + (1.96*SEAGB_alt3_1392)
LowCI_alt3_1392 = AGB_MgC_ha_alt3_1392 - (1.96*SEAGB_alt3_1392)

Wet_AGBPlot_alt3_1392 <-  data.frame (PlotID = unique(levels(wet_1392_higher10$plotID)),
                                        AGB_MgC_ha = AGB_MgC_ha_alt3_1392,
                                        HighCI = HighCI_alt3_1392,
                                        LowCI = LowCI_alt3_1392,
                                        Project_ID = rep("VCS1392"),
                                        EquationType = rep("Alvarez (2012)-II.1 [3]"))
Wet_AGBPlot_alt3_1392

p1392_er_alt3 <- data.frame(Wet_AGBPlot_alt3_1392 %>%
                                 summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt3_1392),
                                           se = mAGB_MgC_ha * 0.24,
                                           HCI = mAGB_MgC_ha + (1.96*se), 
                                           LCI = mAGB_MgC_ha - (1.96*se),
                                           N = n()), 
                               Project_ID = rep("VCS1392"),
                               EquationType = rep("Alvarez (2012)-II.1 [3]"))
p1392_er_alt3

## alternative equation 4, 5, 6
# Alvarez 2012 eq I.1 AGB = exp(-2.857 + (2.081 * ln(DBH)) + (0.587 * ln(H)) + (0.453 * ln(WD)))

# Saldarriaga et al 1998 (Rank 4, DBH > 20) AGB = exp(-1.086 + 0.876 * log(D)^2 + 0.604 * log (H) + 0.871 * log(WD))
# Sierra et al. 2007 (Rank 4, DBH > 1) AGB = exp(2.286 + 2.471 * log(D))

VCS1392_AGB_alt_function4 <- function(D,H, WD) { # create a function with the name my_function
  AGB = exp(-2.857 + (2.081 * log(D)) + (0.587 * log(H)) + (0.453 * log(WD))) * exp((0.320^2)/2)
  print(AGB)
}
VCS1392_AGB_alt4 <- by(wet_1392_higher10, wet_1392_higher10$plotID,
                       function (x) VCS1392_AGB_alt_function4(D = x$D, H = x$Hfeld, WD = x$WD), #check if H or no H
                       simplify = F)
VCS1392_AGB_Alt4 <- sapply(VCS1392_AGB_alt4, sum)
VCS1392_AGB_Alt4 

VCS1392_AGB_alt4_m <- sapply(VCS1392_AGB_alt4, mean)
VCS1392_AGB_alt4_m 

VCS1392_AGB_alt4_c <- sapply(VCS1392_AGB_alt4, length)
VCS1392_AGB_alt4_c

AGB_MgC_ha_alt4_1392 = (VCS1392_AGB_Alt4/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt4_1392
SDAGB_alt4_1392 = AGB_MgC_ha_alt4_1392 * 0.20
SDAGB_alt4_1392
SEAGB_alt4_1392 = SDAGB_alt4_1392/sqrt(VCS1392_AGB_alt4_c)
SEAGB_alt4_1392
HighCI_alt4_1392 = AGB_MgC_ha_alt4_1392 + (1.96*SEAGB_alt4_1392)
LowCI_alt4_1392 = AGB_MgC_ha_alt4_1392 - (1.96*SEAGB_alt4_1392)

Wet_AGBPlot_alt4_1392 <-  data.frame (PlotID = unique(levels(wet_1392_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt4_1392,
                                      HighCI = HighCI_alt4_1392,
                                      LowCI = LowCI_alt4_1392,
                                      Project_ID = rep("VCS1392"),
                                      EquationType = rep("Alvarez (2012)-I.1 [3]"))
Wet_AGBPlot_alt4_1392

p1392_er_alt4 <- data.frame(Wet_AGBPlot_alt4_1392 %>%
                              summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt4_1392),
                                        se = mAGB_MgC_ha * 0.24,
                                        HCI = mAGB_MgC_ha + (1.96*se), 
                                        LCI = mAGB_MgC_ha - (1.96*se),
                                        N = n()), 
                            Project_ID = rep("VCS1392"),
                            EquationType = rep("Alvarez (2012)-I.1 [3]"))
p1392_er_alt4

## Alt equation 5 1392 ####
# # Sierra et al. 2007 (Rank 4, DBH > 1) AGB = exp(-2.286 + 2.471 * log(D))

VCS1392_AGB_alt_function5 <- function(D) { 
  AGB = exp(-2.286 + (2.471 * log(D))) #* 0.091 # the latter is the correction factor and has been added to the equation based on Sierra et al 2007
  print(AGB)
}
VCS1392_AGB_alt5 <- by(wet_1392_higher10, wet_1392_higher10$plotID,
                       function (x) VCS1392_AGB_alt_function5(D = x$D),
                       simplify = F)
VCS1392_AGB_Alt5 <- sapply(VCS1392_AGB_alt5, sum)
VCS1392_AGB_Alt5 

VCS1392_AGB_Alt5_c <- sapply(VCS1392_AGB_alt5, length)
VCS1392_AGB_Alt5_c

AGB_MgC_ha_alt5_1392 = (VCS1392_AGB_Alt5/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt5_1392
SDAGB_alt5_1392 = AGB_MgC_ha_alt5_1392 * 0.20
SEAGB_alt5_1392 = SDAGB_alt5_1392/sqrt(VCS1392_AGB_Alt5_c)
SEAGB_alt5_1392
HighCI_alt5_1392 = AGB_MgC_ha_alt5_1392 + (1.96*SEAGB_alt5_1392)
LowCI_alt5_1392 = AGB_MgC_ha_alt5_1392 - (1.96*SEAGB_alt5_1392)

Wet_AGBPlot_alt5_1392 <-  data.frame (PlotID = unique(levels(wet_1392_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt5_1392,
                                      HighCI = HighCI_alt5_1392,
                                      LowCI = LowCI_alt5_1392,
                                      Project_ID = rep("VCS1392"),
                                      EquationType = rep("Sierra (2007) [4]"))
Wet_AGBPlot_alt5_1392

p1392_er_alt5 <- data.frame(Wet_AGBPlot_alt5_1392 %>%
                              summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt5_1392),
                                        se = mAGB_MgC_ha * 0.24,
                                        HCI = mAGB_MgC_ha + (1.96*se), 
                                        LCI = mAGB_MgC_ha - (1.96*se),
                                        N = n()), 
                            Project_ID = rep("VCS1392"),
                            EquationType = rep("Sierra (2007) [4]"))
p1392_er_alt5

# or Alvarez et al 2012 II.5
#VCS1392_AGB_alt_function4 <- function(D,WD) { # create a function with the name my_function
#  AGB = exp(-1.482 + (2.499* log(D)) + log(WD))
#  print(AGB)
#}

## Compiling data for plotting ####

VCS1392_AGB_Data2 <- rbind(#Wet_AGBPlot_proj_1392,
  p1392_er,
  p1392_r5,
  p1392_er_alt1.1a,
  p1392_er_alt2.1a,
  p1392_er_alt3,
  p1392_er_alt4,
  p1392_er_alt5)
VCS1392_AGB_Data2

#Calcularing tCO2e/ha
vcs1392_toplot2 <- VCS1392_AGB_Data2
vcs1392_toplot2$EquationType = as.factor(vcs1392_toplot2$EquationType)
vcs1392_toplot2$AGC_tCO2_ha = as.numeric(vcs1392_toplot2$mAGB_MgC_ha * (44/12))
vcs1392_toplot2$HCI_tCO2_ha = as.numeric(vcs1392_toplot2$HCI * (44/12))
vcs1392_toplot2$LCI_tCO2_ha = as.numeric(vcs1392_toplot2$LCI * (44/12))
vcs1392_toplot2

# 1392 plotting####
p_1392 <- ggplot(vcs1392_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha), 
                                   y = AGC_tCO2_ha, 
                                   #ymin = LCI_tCO2_ha, 
                                   #ymax = HCI_tCO2_ha, 
                                   color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,300)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS1396 (VM0006)", 
       subtitle = "5 1-ha wet forest plots")+
  theme_pubr()+ #scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0006] VCS1392 Colombia")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p_1392

# 1392 Data Stats ####

VCS1392_AGB_Data3 <- rbind(#Wet_AGBPlot_proj_1392,
  p1392_er,
  p1392_r5,
  p1392_er_alt1.1a,
  p1392_er_alt2.1a,
  p1392_er_alt3,
  p1392_er_alt4,
  p1392_er_alt5,
  p1392_pr)
VCS1392_AGB_Data3

#Calcularing tCO2e/ha
vcs1392_toplot3 <- VCS1392_AGB_Data3
vcs1392_toplot3$EquationType = as.factor(vcs1392_toplot3$EquationType)
vcs1392_toplot3$AGC_tCO2_ha = as.numeric(vcs1392_toplot3$mAGB_MgC_ha * (44/12))
vcs1392_toplot3$HCI_tCO2_ha = as.numeric(vcs1392_toplot3$HCI * (44/12))
vcs1392_toplot3$LCI_tCO2_ha = as.numeric(vcs1392_toplot3$LCI * (44/12))
vcs1392_toplot3

## Project 1359 (DRC) ####
# Equation used AGB = 0.11 * D^2.58

VCS1359_AGB_function <- function(D) { # create a function with the name my_function
  AGB = 0.11 * D^2.58 * exp((0.483^2)/2)
  print(AGB)
}
VCS1359_AGBPlot_Proj_v1 <- by(md_jam_1359_higher10, md_jam_1359_higher10$plotID,
                            function (x) VCS1359_AGB_function(D = x$D),
                            simplify = F)
VCS1359_AGBPlot_Proj <- sapply(VCS1359_AGBPlot_Proj_v1, sum)
VCS1359_AGBPlot_Proj

VCS1359_AGBPlot_Proj_c <- sapply(VCS1359_AGBPlot_Proj_v1, length)
VCS1359_AGBPlot_Proj_c

AGB_MgC_ha_proj_1359 = (VCS1359_AGBPlot_Proj/1000) * 0.5 # carbon fraction used by the project
SDAGB_proj_1359 = AGB_MgC_ha_proj_1359 * 0.20
SEAGB_proj_1359 = SDAGB_proj_1359/sqrt(VCS1359_AGBPlot_Proj_c)
HighCI_proj_1359 = AGB_MgC_ha_proj_1359 + (1.96*SEAGB_proj_1359)
LowCI_proj_1359 = AGB_MgC_ha_proj_1359 - (1.96*SEAGB_proj_1359)

Wet_AGBPlot_proj_1359 <-  data.frame (PlotID = unique(levels(md_jam_1359_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_proj_1359,
                                      HighCI = HighCI_proj_1359,
                                      LowCI = LowCI_proj_1359,
                                      Project_ID = rep("VCS1359"),
                                      EquationType = rep("Project [4]"))
Wet_AGBPlot_proj_1359

p1359_pr <- data.frame(Wet_AGBPlot_proj_1359 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_proj_1359),
                                   se = sd(AGB_MgC_ha_proj_1359)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1359"),
                       EquationType = rep("Project [4]"))
p1359_pr

## Alternative equations for VCS1359 ####

VCS1359_AGB_alt_function1 <- function(D) { # create a function with the name my_function
  AGB = exp(-2.2057 + 2.5841*log(D)) * exp((0.483^2)/2) #Djomo et al 2010 + correction factor
  print(AGB)
}

VCS1359_AGB_alt1_v1 <- by(md_jam_1359_higher10, md_jam_1359_higher10$plotID,
                                      function (x) VCS1359_AGB_alt_function1(D = x$D),
                                      simplify = F)
VCS1359_AGB_alt1.1 <- sapply(VCS1359_AGB_alt1_v1, sum)
VCS1359_AGB_alt1.1

VCS1359_AGB_Alt1.1_c <- sapply(VCS1359_AGB_alt1_v1, length)
VCS1359_AGB_Alt1.1_c

AGB_MgC_ha_alt1.1_1359 = (VCS1359_AGB_alt1.1/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt1.1_1359
SDAGB_alt1.1_1359 = AGB_MgC_ha_alt1.1_1359 * 0.20
SEAGB_alt1.1_1359 = SDAGB_alt1.1_1359/sqrt(VCS1359_AGB_Alt1.1_c)
SEAGB_alt1.1_1359
HighCI_alt1.1_1359 = AGB_MgC_ha_alt1.1_1359 + (1.96*SEAGB_alt1_1359)
LowCI_alt1.1_1359 = AGB_MgC_ha_alt1.1_1359 - (1.96*SEAGB_alt1_1359)

# combining into project data frame ####
Dry_AGBPlot_alt1.1_1359 <-  data.frame (PlotID = unique(levels(md_jam_1359_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt1.1_1359,
                                      HighCI = HighCI_alt1.1_1359,
                                      LowCI = LowCI_alt1.1_1359,
                                      Project_ID = rep("VCS1359"),
                                      EquationType = rep("Djomo (2010)-1 [4]"))
Dry_AGBPlot_alt1.1_1359

p1359_er_alt1.1 <- data.frame(Dry_AGBPlot_alt1.1_1359 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1.1_1359),
                                   se = sd(AGB_MgC_ha_alt1.1_1359)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1359"),
                       EquationType = rep("Djomo (2010)-1 [4]"))
p1359_er_alt1.1

## Alternative function 2 1359 ####

VCS1359_AGB_alt_function2 <- function(D) { # create a function with the name my_function
  AGB = exp(-2.1801 + 2.5624*log(D)) * exp((0.483^2)/2) # Djomo et al 2010 eq. 2 + correction factor
  print(AGB)
}
VCS1359_AGB_alt_v2 <- by(md_jam_1359_higher10, md_jam_1359_higher10$plotID,
                                   function (x) VCS1359_AGB_alt_function2(D = x$D),
                                   simplify = F)
VCS1359_AGB_alt2 <- sapply(VCS1359_AGB_alt_v2, sum)
VCS1359_AGB_alt2

VCS1359_AGB_Alt2_c <- sapply(VCS1359_AGB_alt_v2, length)
VCS1359_AGB_Alt2_c

AGB_MgC_ha_alt2_1359 = (VCS1359_AGB_alt2/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt2_1359
SDAGB_alt2_1359 = AGB_MgC_ha_alt2_1359 * 0.20
SEAGB_alt2_1359 = SDAGB_alt2_1359/sqrt(VCS1359_AGB_Alt2_c)
SEAGB_alt2_1359
HighCI_alt2_1359 = AGB_MgC_ha_alt2_1359 + (1.96*SEAGB_alt2_1359)
LowCI_alt2_1359 = AGB_MgC_ha_alt2_1359 - (1.96*SEAGB_alt2_1359)

# combining into project data frame ##
Dry_AGBPlot_alt2_1359 <-  data.frame (PlotID = unique(levels(md_jam_1359_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt2_1359,
                                      HighCI = HighCI_alt2_1359,
                                      LowCI = LowCI_alt2_1359,
                                      Project_ID = rep("VCS1359"),
                                      EquationType = rep("Djomo (2010)-2 [4]"))
Dry_AGBPlot_alt2_1359

p1359_er_alt2 <- data.frame(Dry_AGBPlot_alt2_1359 %>%
                                summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt2_1359),
                                          se = sd(AGB_MgC_ha_alt2_1359)/sqrt(n()),
                                          HCI = mAGB_MgC_ha + (1.96*se), 
                                          LCI = mAGB_MgC_ha - (1.96*se),
                                          N = n()), 
                              Project_ID = rep("VCS1359"),
                              EquationType = rep("Djomo (2010)-2 [4]"))
p1359_er_alt2

# alternative equation from Djomo et al (2010)

VCS1359_AGB_alt_function3 <- function(D,H) { # Djomo et al 2010 eq. 3 * correction factor
  AGB = exp(-3.2249 + 0.9885 * log(D^2 * H)) * exp((0.483^2)/2)
  print(AGB)
}
VCS1359_AGB_alt3_v1<- by(md_jam_1359_higher10, md_jam_1359_higher10$plotID,
                               function (x) VCS1359_AGB_alt_function3(D = x$D, H = x$Hfeld),
                               simplify = F)
VCS1359_AGB_alt3_v1

VCS1359_AGB_alt3 <- sapply(VCS1359_AGB_alt3_v1, sum)

VCS1359_AGB_Alt3_c <- sapply(VCS1359_AGB_alt3_v1, length)


AGB_MgC_ha_alt3_1359 = (VCS1359_AGB_alt3/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt3_1359
SDAGB_alt3_1359 = AGB_MgC_ha_alt3_1359 * 0.20
SEAGB_alt3_1359 = SDAGB_alt3_1359/sqrt(VCS1359_AGB_Alt3_c)
SEAGB_alt3_1359
HighCI_alt3_1359 = AGB_MgC_ha_alt3_1359 + (1.96*SEAGB_alt3_1359)
LowCI_alt3_1359 = AGB_MgC_ha_alt3_1359 - (1.96*SEAGB_alt3_1359)

# combining into project data frame ###
Dry_AGBPlot_alt3_1359 <-  data.frame (PlotID = unique(levels(md_jam_1359_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt3_1359,
                                      HighCI = HighCI_alt3_1359,
                                      LowCI = LowCI_alt3_1359,
                                      Project_ID = rep("VCS1359"),
                                      EquationType = rep("Djomo (2010)-3 [4]"))
Dry_AGBPlot_alt3_1359
AGB_MgC_ha_alt3_1359
p1359_er_alt3 <- data.frame(Dry_AGBPlot_alt3_1359 %>%
                              summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt3_1359),
                                        se = sd(AGB_MgC_ha_alt3_1359)/sqrt(n()), #calculate the error across the five plots
                                        HCI = mAGB_MgC_ha + (1.96 * se), 
                                        LCI = mAGB_MgC_ha - (1.96 * se),
                                        N = n()), 
                            Project_ID = rep("VCS1359"),
                            EquationType = rep("Djomo (2010)-3 [4]"))
p1359_er_alt3

# Not used - alternative equation from Djomo et al (2010)
# AGB = exp(−1.9644 + 2.3382 * log(D) + 0.3579 * log(WD)) - RSE = 0.325 R2 = 0.96
#VCS1359_AGB_alt_function4 <- function(D,WD) { 
#  AGB = exp(-1.9644 + 2.3382 * log(D) + 0.3579 * log(WD))
#  print(AGB)
#}

# Alternative 5 1359 ####
#Alternative equations 
#VCS1359_AGB_alt_function5 <- function(D,H,WD) { # create a function with the name my_function
#  AGB = -4.22412 + 0.56*(D)^2
#  print(AGB)
#}

## 1359 Compiling data for plotting ####

VCS1359_AGB_Data2 <- rbind(#Wet_AGBPlot_proj_1359,
  p1359_er,
  p1359_r5,
  p1359_er_alt1.1,
  p1359_er_alt2,
  p1359_er_alt3)

VCS1359_AGB_Data2

#Calcularing tCO2e/ha
vcs1359_toplot2 <- VCS1359_AGB_Data2
vcs1359_toplot2$AGC_tCO2_ha = as.numeric(vcs1359_toplot2$mAGB_MgC_ha * (44/12))
vcs1359_toplot2$HCI_tCO2_ha = as.numeric(vcs1359_toplot2$HCI * (44/12))
vcs1359_toplot2$LCI_tCO2_ha = as.numeric(vcs1359_toplot2$LCI * (44/12))
vcs1359_toplot2$EquationType = as.factor(vcs1359_toplot2$EquationType)
vcs1359_toplot2

# 1359 plotting####

p_1359 <- ggplot(vcs1359_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha), 
                                   y = AGC_tCO2_ha, 
                                  # ymin = LCI_tCO2_ha, 
                                  # ymax = HCI_tCO2_ha, 
                                   color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,320)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS1359 (VM0006)", 
       subtitle = "7 1-ha dry forest plots")+
  theme_pubr()+ #scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0006] VCS1359 DRC")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p_1359

# 1359 Data Stats ####

VCS1359_AGB_Data3 <- rbind(#Wet_AGBPlot_proj_1359,
  p1359_er,
  p1359_r5,
  p1359_er_alt1.1,
  p1359_er_alt2,
  p1359_er_alt3,
  p1359_pr)

VCS1359_AGB_Data3

#Calcularing tCO2e/ha
vcs1359_toplot3 <- VCS1359_AGB_Data3
vcs1359_toplot3$AGC_tCO2_ha = as.numeric(vcs1359_toplot3$mAGB_MgC_ha * (44/12))
vcs1359_toplot3$HCI_tCO2_ha = as.numeric(vcs1359_toplot3$HCI * (44/12))
vcs1359_toplot3$LCI_tCO2_ha = as.numeric(vcs1359_toplot3$LCI * (44/12))
vcs1359_toplot3$EquationType = as.factor(vcs1359_toplot3$EquationType)
vcs1359_toplot3

# Plot VM0006 ####
p_VM6 <- (p_1392|p_1359)+p_1396+plot_layout(widths = c(1,1,1), ncol=3)
p_VM6

#p_VM6 <- (p_1392_final|p_1359_final)+p_1396_final+plot_layout(widths = c(2,1), ncol=2)
#p_VM6

# saving VM0006 plot ####

ggsave(filename = "FigVM0006_v8.png",
       plot = p_VM6, width = 18, height = 8, units = 'cm',
       scale = 2, dpi = 800)

## END ####



# Using Biomass Package to compare ABG biomass estimates across REDD+ projects 
# with alternative allometric equations

#Installing and calling the package from the github repo (under development)###
install.packages("remotes")
remotes::install_github('umr-amap/BIOMASS')

#latest version released on CRAN
install.packages("BIOMASS")
clearCache

#run to use the package
library("BIOMASS")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gghighlight)
library(ggbreak)
library(ggpubr)
library(patchwork)

#To cite the package
citation("BIOMASS")

## VCS 1112 Data####

#Uploading Brazil Forest Inventory data ####

BON_A01_2014_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/BON_A01_2014_Inventory.csv")
bon_ana01 <- data.frame(BON_A01_2014_Inventory)# BON_A01_2014.csv 
bon_ana01$dead

bon_ana01$plotID <- as.factor(ifelse(bon_ana01$plot<5, '1',
                                    ifelse(bon_ana01$plot == 5, '2',
                                           ifelse(bon_ana01$plot>5 & bon_ana01$plot<9, '2', '2.5'))))

# print data frame
str(bon_ana01$plotID)

md_bonana01 <- data.frame(bon_ana01) #renaming to keep original data frame as uploaded
str(md_bonana01) # 257

# Data wrangling to adjust species names
md_bonana01 <- md_bonana01 %>% separate(scientific_name, c("genus", "species"))
names(md_bonana01)[7] <- 'family'
names(md_bonana01)[8] <- 'D'

md_bonana01$type = as.factor(md_bonana01$type)

md_bonana01_f <- md_bonana01 %>% filter(dead!= "D") %>% filter(type== "O")
str(md_bonana01_f) # 240 individuals

# HUM data
HUM_A01_2014_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/HUM_A01_2014_Inventory.csv")
hum_a01 <- data.frame(HUM_A01_2014_Inventory) #HUM_A01_2014.csv 

hum_a01$plotID <- as.factor(ifelse(hum_a01$plot<5, '3',
                                     ifelse(hum_a01$plot == 5, '4',
                                            ifelse(hum_a01$plot>5 & hum_a01$plot<9, '4', '2.5'))))

# print data frame
str(hum_a01$plotID)

md_huma01 <- data.frame(hum_a01) #renaming to keep original data frame as uploaded
str(md_huma01) # 241 individuals
summary(md_huma01)

# Data wrangling to adjust species names
md_huma01 <- md_huma01 %>% separate(scientific_name, c("genus", "species"))

names(md_huma01)[7] <- 'family'
names(md_huma01)[8] <- 'D'

str(md_huma01)
md_huma01$type = as.factor(md_huma01$type)

md_huma01_f <- md_huma01 %>% filter(dead!= "D") %>% filter(type== "O")
str(md_huma01_f) # 231 individuals

# TAL data
TAL_A01_2014_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/TAL_A01_2014_Inventory.csv")
tal_a01 <- data.frame(TAL_A01_2014_Inventory)# TAL_A01_2014.csv 

tal_a01$plotID <- as.factor(ifelse(tal_a01$plot<10, '5','6.5'))

# print data frame
str(tal_a01$plotID)

md_tala01 <- data.frame(tal_a01) #renaming to keep original data frame as uploaded
str(md_tala01)
head(md_tala01)

# Data wrangling to adjust species names
md_tala01 <- md_tala01 %>% filter(scientific_name!="NA") %>% separate(scientific_name, c("genus", "species"))

names(md_tala01)[7] <- 'family'
names(md_tala01)[8] <- 'D'

str(md_tala01) # 112 individuals

md_tala01$type = as.factor(md_tala01$type)

md_tala01_f <- md_tala01 %>% filter(dead!= "D") %>% filter(type== "O")
str(md_tala01_f) # 103 individuals

## VCS1112 Data ####
#Moist deciduous forests in Acre - VCS1112 

# merge the three data frames
names(md_bonana01_f)
names(md_tala01_f)
names(md_huma01_f)

bon<- md_bonana01_f[,c('Area', 'plotID', 'genus', 'species', 'family', 
              'D', 'Hcom', 'Htot')]

names(bon)[1] <- 'area'
bon

tal <- md_tala01_f[,c('area', 'plotID', 'genus', 'species', 'family', 
             'D', 'Hcom', 'Htot')]
tal 

hum <- md_huma01_f[,c('area', 'plotID', 'genus', 'species', 'family', 
             'D', 'Hcom', 'Htot')]
hum

# 1112 dataframe
md_vcs1112 <- rbind(bon, tal, hum)
str(md_vcs1112)

md_vcs1112 %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n()) #6 1-ha plots

## VCS1566 and VCS985 data #####
CAU_A01_2014_2018_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/CAU_A01_2014_2018_Inventory.csv")
cau_a01 <- data.frame(CAU_A01_2014_2018_Inventory)# CAU_A01_2014_Inventory.csv # liana deleted and dead trees deleted
names(cau_a01) # 4469

cau_a01$plotID <- as.factor(ifelse(cau_a01$plot_ID<5, '1',
                                    ifelse(cau_a01$plot_ID == 5, '2',
                                           ifelse(cau_a01$plot_ID>5 & cau_a01$plot_ID<9, '2',
                                                  ifelse(cau_a01$plot_ID>8 & cau_a01$plot_ID<13, '3', 
                                                         ifelse(cau_a01$plot_ID>12 & cau_a01$plot_ID<17, '4',
                                                                ifelse(cau_a01$plot_ID>16 & cau_a01$plot_ID<21, '5',
                                                                       ifelse(cau_a01$plot_ID>20 & cau_a01$plot_ID<25, '6',
                                                                              ifelse(cau_a01$plot_ID>24 & cau_a01$plot_ID<29, '7',
                                                                                     ifelse(cau_a01$plot_ID>29 & cau_a01$plot_ID<34, '8','9'))))))))))

# print data frame
str(cau_a01)

cau_a01 <- cau_a01 %>% filter(scienfic_name!="NA") %>% separate(scienfic_name, c("genus", "species"))
names(cau_a01)
names(cau_a01)[8] <- 'family'
names(cau_a01)[10] <- 'D'

str(cau_a01)

cau_a01$type = as.factor(cau_a01$type)

cau_a01_f <- cau_a01 %>% filter(Dead.2014!= "D") %>% filter(type == "O")
str(cau_a01_f) # 2408 individuals

# changing data frame name to allow for data wrangling
str(wet_1566)
wet_1566 <- cau_a01 %>% filter(plotID!="9")
wet_985 <- cau_a01 %>% filter(plotID!="9")

wet_985 %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n()) #8 1-ha plots and the remainder will not be used

# Filter out NA's in D column
md_vcs1112_filt <- md_vcs1112 %>% drop_na(D, Htot, Hcom)
str(md_vcs1112_filt) # 571 individuals in 6.25 ha
summary(md_vcs1112_filt)

wet_1566_filt <- wet_1566 %>% drop_na(D)
str(wet_1566_filt) # 886 individuals in 8 ha
summary(wet_1566_filt)

wet_985_filt <- wet_985 %>% drop_na(D)
str(wet_985_filt) # 886 individuals in 8 ha
summary(wet_985_filt)

# Filter out dbh sizes lower than 10 cm####

md_vcs1112_higher10 <- md_vcs1112_filt %>% filter (D > 9.999) %>% filter(plotID != '6.5')
str(md_vcs1112_higher10) #562 trees in 6 ha
summary(md_vcs1112_higher10) #Max DBH = 180 cm

vcs1566_higher10 <- wet_1566_filt %>% filter (D > 9.999)
str(vcs1566_higher10) #2407 individuals in 8 ha
summary(vcs1566_higher10) #Max DBH = 185 cm

vcs985_higher10 <- wet_985_filt %>% filter (D > 9.999)
str(vcs985_higher10) #2407 individuals in 8 ha

# Checking and correcting taxonomy####

Taxo_1112<-correctTaxo(genus = md_vcs1112_higher10$genus, 
                       species = md_vcs1112_higher10$species,
                       score = 0.5,
                       useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
md_vcs1112_higher10$genusCorr <- Taxo_1112$genusCorrected
md_vcs1112_higher10$speciesCorr <- Taxo_1112$speciesCorrected

Taxo_1566<-correctTaxo(genus = vcs1566_higher10$genus, 
                  species = vcs1566_higher10$species,
                  score = 0.5,
                  useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).

vcs1566_higher10$genusCorr <- Taxo_1566$genusCorrected
vcs1566_higher10$speciesCorr <- Taxo_1566$speciesCorrected

Taxo_985<-correctTaxo(genus = vcs985_higher10$genus, 
                       species = vcs985_higher10$species,
                       score = 0.5,
                       useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).

vcs985_higher10$genusCorr <- Taxo_985$genusCorrected
vcs985_higher10$speciesCorr <- Taxo_985$speciesCorrected

# Retrieving APG III Families and Orders from Genus names####

APG_1112 <- getTaxonomy(md_vcs1112_higher10$genusCorr, findOrder = T)
md_vcs1112_higher10$familyAPG <- APG_1112$family
md_vcs1112_higher10$orderAPG <- APG_1112$order

APG_1566 <- getTaxonomy(vcs1566_higher10$genusCorr, findOrder = T)
vcs1566_higher10$familyAPG <- APG_1566$family
vcs1566_higher10$orderAPG <- APG_1566$order

APG_985 <- getTaxonomy(vcs985_higher10$genusCorr, findOrder = T)
vcs985_higher10$familyAPG <- APG_985$family
vcs985_higher10$orderAPG <- APG_985$order

#Getting wood density data####
#The WD can either be attributed to an individual at a species, 
#genus, family or stand level.

# Compute the Wood Density up to the genus level 
# and then give the mean wood density per stand

WDdata_1112<-getWoodDensity(genus = Taxo_1112$genusCorrected, 
                            species = Taxo_1112$speciesCorrected, 
                            stand = md_vcs1112_higher10$plotId,
                            region = "SouthAmericaTrop")
# adding WD column to plot dataframe
md_vcs1112_higher10$WD <- WDdata_1112$meanWD
md_vcs1112_higher10$sdWD <- WDdata_1112$sdWD

WDdata_1566<-getWoodDensity(genus = Taxo_1566$genusCorrected, 
                       species = Taxo_1566$speciesCorrected, 
                       stand = vcs1566_higher10$plotId,
                       region = "SouthAmericaTrop")
str(WDdata_1566)# it works > 83 taxa in this subset
vcs1566_higher10$WD <- WDdata_1566$meanWD
vcs1566_higher10$sdWD <- WDdata_1566$sdWD

WDdata_985<-getWoodDensity(genus = Taxo_985$genusCorrected, 
                            species = Taxo_985$speciesCorrected, 
                            stand = vcs985_higher10$plotId,
                            region = "SouthAmericaTrop")
# adding WD column to plot dataframe
vcs985_higher10$WD <- WDdata_985$meanWD
vcs985_higher10$sdWD <- WDdata_985$sdWD

#Tree Height ####
#Retrieve height data####

# If the site misses tree height measurements, 
# it is used a continent or region specific H-D model
# or generic H-D model based on single bio-climatic predictor E
# from Chavel et al 2014, retrieved by combining lat and long data

# Retrieve height data from a Feldpaush et al. (2012) averaged model
dataHfeld_1112 <- retrieveH(D = md_vcs1112_higher10$D, region = "SAmerica")
md_vcs1112_higher10$Hfeld <- dataHfeld_1112$H

dataHfeld_1566 <- retrieveH(D = vcs1566_higher10$D, region = "SAmerica")
vcs1566_higher10$Hfeld <- dataHfeld_1566$H

dataHfeld_985 <- retrieveH(D = vcs985_higher10$D, region = "SAmerica")
vcs985_higher10$Hfeld <- dataHfeld_985$H

# Using coordinates to obtain height data
    # HchaveE<-retrieveH(D = we_plot65_higher10$D, 
             #      coord = cbind(NouraguesHD$long, NouraguesHD$lat))

# Retrieve height data from Chave et al. (2012) equation 6
md_vcs1112_higher10$lat <- rep(-9.87)
md_vcs1112_higher10$long <- rep(-67.28)
long_1112 <- -67.28
lat_1112 <- -9.87
coord_1112 <- c(long_1112, lat_1112)
coord_1112

dataHchave_1112 <- retrieveH(D = md_vcs1112_higher10$D,
                                  #coord = cbind(md_br_1_4_higher10$long, md_br_1_4_higher10$lat),
                             coord = coord_1112)
md_vcs1112_higher10$Hchave<- dataHchave_1112$H

long_1566 <- -48.47
lat_1566 <- -3.73
coord_1566 <- c(long_1566, lat_1566)
coord_1566

dataHchave_1566 <- retrieveH(D = vcs1566_higher10$D,
                        #coord = cbind(md_br_1_4_higher10$long, md_br_1_4_higher10$lat),
                        coord = coord_1566)
vcs1566_higher10$Hchave<- dataHchave_1566$H

long_985 <- -48.47
lat_985 <- -3.73
coord_985 <- c(long_985, lat_985)
coord_985

dataHchave_985 <- retrieveH(D = vcs985_higher10$D,
                             #coord = cbind(md_br_1_4_higher10$long, md_br_1_4_higher10$lat),
                            coord = coord_985)
vcs985_higher10$Hchave<- dataHchave_985$H

####
# Once diameter, wood density and height values ##
# have been retrieved for each tree ##
###

## Compute AGB per plot Chave 2014####

# computing AGB per plot

# 1112

AGBPlot_Hf_1112 <- by(md_vcs1112_higher10, md_vcs1112_higher10$plotID,
                      function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                      simplify = F)
AGBplot_D_Hf_WD_1112 <- sapply(AGBPlot_Hf_1112, sum)
AGBplot_D_Hf_WD_1112 # Mg per 1 ha

AGBplot_D_Hf_WD_1112_m <- sapply(AGBPlot_Hf_1112, mean)
AGBplot_D_Hf_WD_1112_m 

AGBplot_D_Hf_WD_1112_c <- sapply(AGBPlot_Hf_1112, length)
AGBplot_D_Hf_WD_1112_c

# 20% combined error
AGB_MgC_ha_alt1_1112 = (AGBplot_D_Hf_WD_1112) * 0.456
AGB_MgC_ha_alt1_1112
SDAGB_alt1_1112 = AGB_MgC_ha_alt1_1112 * 0.20
SDAGB_alt1_1112
SEAGB_alt1_1112 = SDAGB_alt1_1112/sqrt(AGBplot_D_Hf_WD_1112_c)
SEAGB_alt1_1112
HighCI_alt1_1112 = AGB_MgC_ha_alt1_1112 + (1.96*SEAGB_alt1_1112)
LowCI_alt1_1112 = AGB_MgC_ha_alt1_1112 - (1.96*SEAGB_alt1_1112)

# combining into data frame ##
MD_AGBPlot_Chave2014_Ht_1112 <-  data.frame (PlotID = unique(levels(md_vcs1112_higher10$plotID)), #PlotID  = c(1, 2, 2.5, 5, 6, 3, 4)
                                             AGB_MgC_ha = AGB_MgC_ha_alt1_1112,
                                             HighCI = HighCI_alt1_1112,
                                             LowCI = LowCI_alt1_1112,
                                             Project_ID = rep("VCS1112"),
                                             EquationType = rep("Chave (2014) [6]"))
MD_AGBPlot_Chave2014_Ht_1112

p1112_er <- data.frame(MD_AGBPlot_Chave2014_Ht_1112 %>% filter(PlotID!="6.5") %>% 
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1_1112),
                                   se = sd(AGB_MgC_ha_alt1_1112)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1112"),
                       EquationType = rep("Chave (2014) [6]"))
p1112_er

# 1566

AGBPlot_Hf_1566 <- by(vcs1566_higher10, vcs1566_higher10$plotID,
                      function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                      simplify = F)
AGBplot_D_Hf_WD_1566 <- sapply(AGBPlot_Hf_1566, sum)
AGBplot_D_Hf_WD_1566 # Mg per 1 ha

AGBplot_D_Hf_WD_1566_m <- sapply(AGBPlot_Hf_1566, mean)
AGBplot_D_Hf_WD_1566_m 

AGBplot_D_Hf_WD_1566_c <- sapply(AGBPlot_Hf_1566, length)
AGBplot_D_Hf_WD_1566_c

# 20% combined error
AGB_MgC_ha_alt1_1566 = (AGBplot_D_Hf_WD_1566) * 0.456
AGB_MgC_ha_alt1_1566
SDAGB_alt1_1566 = AGB_MgC_ha_alt1_1566 * 0.20
SDAGB_alt1_1566
SEAGB_alt1_1566 = SDAGB_alt1_1566/sqrt(AGBplot_D_Hf_WD_1566_c)
SEAGB_alt1_1566
HighCI_alt1_1566 = AGB_MgC_ha_alt1_1566 + (1.96*SEAGB_alt1_1566)
LowCI_alt1_1566 = AGB_MgC_ha_alt1_1566 - (1.96*SEAGB_alt1_1566)

AGBPlot_Chave2014_Ht_1566 <-  data.frame (PlotID = unique(levels(vcs1566_higher10$plotID)),
                                         AGB_MgC_ha = AGB_MgC_ha_alt1_1566,
                                         HighCI = HighCI_alt1_1566,
                                         LowCI = LowCI_alt1_1566,
                                         Project_ID = rep("VCS1566"),
                                         EquationType = rep("Chave (2014) [6]"))
AGBPlot_Chave2014_Ht_1566 

p1566_er <- data.frame(AGBPlot_Chave2014_Ht_1566 %>% filter(PlotID!="9") %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1_1566),
                                   se = sd(AGB_MgC_ha_alt1_1566)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1566"),
                       EquationType = rep("Chave (2014) [6]"))
p1566_er

# 985

AGBPlot_Hf_985 <- by(vcs985_higher10, vcs985_higher10$plotID,
                     function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                     simplify = F)
AGBplot_D_Hf_WD_985 <- sapply(AGBPlot_Hf_985, sum)
AGBplot_D_Hf_WD_985 # Mg per 1 ha

AGBplot_D_Hf_WD_985_m <- sapply(AGBPlot_Hf_985, mean)
AGBplot_D_Hf_WD_985_m 

AGBplot_D_Hf_WD_985_c <- sapply(AGBPlot_Hf_985, length)
AGBplot_D_Hf_WD_985_c

# 20% combined error
AGB_MgC_ha_alt1_985 = (AGBplot_D_Hf_WD_985) * 0.456
AGB_MgC_ha_alt1_1566
SDAGB_alt1_985 = AGB_MgC_ha_alt1_985 * 0.20
SDAGB_alt1_985
SEAGB_alt1_985= SDAGB_alt1_985/sqrt(AGBplot_D_Hf_WD_985_c)
SEAGB_alt1_985
HighCI_alt1_985 = AGB_MgC_ha_alt1_985 + (1.96*SEAGB_alt1_985)
LowCI_alt1_985 = AGB_MgC_ha_alt1_985 - (1.96*SEAGB_alt1_985)

# combining into data frame ##
AGBPlot_Chave2014_Ht_985 <-  data.frame (PlotID = unique(levels(vcs985_higher10$plotID)),
                                          AGB_MgC_ha = AGB_MgC_ha_alt1_985,
                                          HighCI = HighCI_alt1_985,
                                          LowCI = LowCI_alt1_985,
                                          Project_ID = rep("VCS985"),
                                          EquationType = rep("Chave (2014) [6]"))
AGBPlot_Chave2014_Ht_985

p985_er <- data.frame(AGBPlot_Chave2014_Ht_985 %>% filter(PlotID!="9") %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1_985),
                                   se = sd(AGB_MgC_ha_alt1_985)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS985"),
                       EquationType = rep("Chave (2014) [6]"))
p985_er

# Compute AGB(Mg) forest type equation from Chave (2005) ####

# 1112

# AGB = 0.0509 * (WD * D^2 * H)) Chave 2005 type moist forest

VCS1112_AGB_function_r5 <- function(WD, D, H) { # create a function with the name my_function
  AGB = 0.0509 * (WD * D^2 * H)  #* exp((0.459^2)/2) correction factor already embedded in equation
  print(AGB)
}
AGBPlot_r5_1112 <- by(md_vcs1112_higher10, md_vcs1112_higher10$plotID,
                      function (x) VCS1112_AGB_function_r5(WD = x$WD, D = x$D,H = x$Hfeld),
                      simplify = F)
AGBPlot_r5_1112
AGBplot_D_WD_1112 <- sapply(AGBPlot_r5_1112, sum)
AGBplot_D_WD_1112

AGBplot_D_WD_1112_m <- sapply(AGBPlot_r5_1112, mean)
AGBplot_D_WD_1112_m 

AGBplot_D_WD_1112_c <- sapply(AGBPlot_r5_1112, length)
AGBplot_D_WD_1112_c

# 20% Error
AGB_MgC_ha_chave_r5_1112 = (AGBplot_D_WD_1112/1000) * 0.456
AGB_MgC_ha_chave_r5_1112
SDAGB_r5_1112 = AGB_MgC_ha_chave_r5_1112 * 0.20
SEAGB_r5_1112 = SDAGB_r5_1112/sqrt(AGBplot_D_WD_1112_c)
SEAGB_r5_1112
HighCI_r5_1112 = AGB_MgC_ha_chave_r5_1112 + (1.96*SEAGB_r5_1112)
LowCI_r5_1112 = AGB_MgC_ha_chave_r5_1112 - (1.96*SEAGB_r5_1112)

MD_AGBPlot_r5_1112<-  data.frame (PlotID = unique(levels(md_vcs1112_higher10$plotID)),
                                   AGB_MgC_ha = AGB_MgC_ha_chave_r5_1112,
                                   HighCI = HighCI_r5_1112,
                                   LowCI = LowCI_r5_1112,
                                   Project_ID = rep("VCS1112"),
                                   EquationType = rep("Chave (2005) [5]"))
MD_AGBPlot_r5_1112

p1112_r5 <- data.frame(MD_AGBPlot_r5_1112 %>% filter(PlotID!="6.5") %>% 
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_chave_r5_1112),
                                   se = sd(AGB_MgC_ha_chave_r5_1112)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1112"),
                       EquationType = rep("Chave (2005) [5]"))
p1112_r5

 #1566
# AGB = 0.0776 * (WD * D^2 * H))^0.940 Chave 2005 type I wet

VCS1566_AGB_function_r5 <- function(WD, D, H) { # create a function with the name my_function
  AGB = 0.0776 * (WD * D^2 * H)^0.940 #* exp((0.459^2)/2) correction factor already embedded in equation
  print(AGB)
}
AGBPlot_r5_1566 <- by(vcs1566_higher10, vcs1566_higher10$plotID,
                      function (x) VCS1566_AGB_function_r5(WD = x$WD, D = x$D,H = x$Hfeld),
                      simplify = F)
AGBPlot_r5_1566
AGBplot_D_WD_1566 <- sapply(AGBPlot_r5_1566, sum)
AGBplot_D_WD_1566

AGBplot_D_WD_1566_m <- sapply(AGBPlot_r5_1566, mean)
AGBplot_D_WD_1566_m 

AGBplot_D_WD_1566_c <- sapply(AGBPlot_r5_1566, length)
AGBplot_D_WD_1566_c

# Error
AGB_MgC_ha_chave_r5_1566 = (AGBplot_D_WD_1566/1000) * 0.456
AGB_MgC_ha_chave_r5_1566
SDAGB_r5_1566 = AGB_MgC_ha_chave_r5_1566 * 0.20
SDAGB_r5_1566
SEAGB_r5_1566 = SDAGB_r5_1566/sqrt(AGBplot_D_WD_1566_c)
SEAGB_r5_1566
HighCI_r5_1566 = AGB_MgC_ha_chave_r5_1566 + (1.96*SEAGB_r5_1566)
LowCI_r5_1566 = AGB_MgC_ha_chave_r5_1566 - (1.96*SEAGB_r5_1566)

AGBPlot_r5_1566<-  data.frame (PlotID = unique(levels(vcs1566_higher10$plotID)),
                                   AGB_MgC_ha = AGB_MgC_ha_chave_r5_1566,
                                   HighCI = HighCI_r5_1566,
                                   LowCI = LowCI_r5_1566,
                                   Project_ID = rep("VCS1566"),
                                   EquationType = rep("Chave (2005) [5]"))
AGBPlot_r5_1566

p1566_r5 <- data.frame(AGBPlot_r5_1566 %>% filter(PlotID!="9") %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_chave_r5_1566),
                                   se = sd(AGB_MgC_ha_chave_r5_1566)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1566"),
                       EquationType = rep("Chave (2005) [5]"))
p1566_r5

# 985

VCS985_AGB_function_r5 <- function(WD, D, H) { # create a function with the name my_function
  AGB = 0.0776 * (WD * D^2 * H)^0.940 #* exp((0.459^2)/2) correction factor already embedded in equation
  print(AGB)
}
AGBPlot_r5_985 <- by(vcs985_higher10, vcs985_higher10$plotID,
                      function (x) VCS985_AGB_function_r5(WD = x$WD, D = x$D,H = x$Hfeld),
                      simplify = F)
AGBPlot_r5_985
AGBplot_D_WD_985 <- sapply(AGBPlot_r5_985, sum)
AGBplot_D_WD_985

AGBplot_D_WD_985_m <- sapply(AGBPlot_r5_985, mean)
AGBplot_D_WD_985_m 

AGBplot_D_WD_985_c <- sapply(AGBPlot_r5_985, length)
AGBplot_D_WD_985_c

# Error
AGB_MgC_ha_chave_r5_985 = (AGBplot_D_WD_985/1000) * 0.456
AGB_MgC_ha_chave_r5_985
SDAGB_r5_985 = AGB_MgC_ha_chave_r5_985 * 0.20
SEAGB_r5_985 = SDAGB_r5_985/sqrt(AGBplot_D_WD_985_c)
SEAGB_r5_985
HighCI_r5_985 = AGB_MgC_ha_chave_r5_985 + (1.96*SEAGB_r5_985)
LowCI_r5_985 = AGB_MgC_ha_chave_r5_985 - (1.96*SEAGB_r5_985)

AGBPlot_r5_985 <-  data.frame (PlotID = unique(levels(vcs985_higher10$plotID)),
                                AGB_MgC_ha = AGB_MgC_ha_chave_r5_985,
                                HighCI = HighCI_r5_985,
                                LowCI = LowCI_r5_985,
                                Project_ID = rep("VCS985"),
                                EquationType = rep("Chave (2005) [5]"))
AGBPlot_r5_985

p985_r5 <- data.frame(AGBPlot_r5_985 %>% filter(PlotID!="9") %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_chave_r5_985),
                                  se = sd(AGB_MgC_ha_chave_r5_985)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS985"),
                      EquationType = rep("Chave (2005) [5]"))
p985_r5

## Calculating AGB with Project equation ####

# Project VCS1112 (Brazil) ####
# Equation used AGB (kg) = ((42.69-12.8*(DBH)+1.242*(DBH)^2)) 

VCS1112_AGB_function <- function(D) { # create a function with the name my_function
  AGB = 42.69 - (12.8 * D) + (1.242 * (D^2)) * exp((0.349^2)/2)
  print(AGB) # this returns AGB in kg
}
AGBPlot_Proj1112 <- by(md_vcs1112_higher10, md_vcs1112_higher10$plotID,
                       function(x) VCS1112_AGB_function(D = x$D),
                       simplify = F)
AGBplot_ProjD1112 <- sapply(AGBPlot_Proj1112, sum)
AGBplot_ProjD1112 # kg per 1 ha

AGBplot_ProjD1112_c <- sapply(AGBPlot_Proj1112, length)
AGBplot_ProjD1112_c

# Error 
AGB_MgC_ha_Proj_1112 = (AGBplot_ProjD1112/1000) * 0.47 #fraction used by project
AGB_MgC_ha_Proj_1112
SDAGB_proj_1112 = AGB_MgC_ha_Proj_1112 * 0.20
SEAGB_proj_1112 = SDAGB_proj_1112/sqrt(AGBplot_ProjD1112_c)
HighCI_proj_1112 = AGB_MgC_ha_Proj_1112 + (1.96*SEAGB_proj_1112)
LowCI_proj_1112 = AGB_MgC_ha_Proj_1112 - (1.96*SEAGB_proj_1112)

MD_AGBPlot_proj1112<-  data.frame (PlotID = unique(levels(md_vcs1112_higher10$plotID)),
                               AGB_MgC_ha = AGB_MgC_ha_Proj_1112,
                               HighCI = HighCI_proj_1112,
                               LowCI = LowCI_proj_1112,
                               Project_ID = rep("VCS1112"),
                               EquationType = rep("Project [5]"))
MD_AGBPlot_proj1112

p1112_pr <- data.frame(MD_AGBPlot_proj1112 %>% filter(PlotID!="6.5") %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_Proj_1112),
                                   se = sd(AGB_MgC_ha_Proj_1112)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1112"),
                       EquationType = rep("Project [5]"))
p1112_pr

## Alternative equations for VCS1112 ####

# Romero 2020 equation MB2
VCS1112_AGB_alt1.1_function <- function(D,H,WD) { # create a function with the name my_function
  AGB = exp(-8.26306 + (0.87461 * log(D^2 * H)) + (0.97690 * log(WD))) * exp((0.349^2)/2)
  print(AGB)
}
## Compute AGB per plot ###
AGBPlot_alt1.1_1112 <- by(md_vcs1112_higher10, md_vcs1112_higher10$plotID,
                        function(x) VCS1112_AGB_alt1.1_function(D = x$D, H = x$Hfeld, WD=x$WD),
                        simplify = F)
AGBplot_Alt1.1_1112 <- sapply(AGBPlot_alt1.1_1112, sum)
AGBplot_Alt1.1_1112 # kg per 1 ha

AGBplot_Alt1.1_1112_c <- sapply(AGBPlot_alt1.1_1112, length)
AGBplot_Alt1.1_1112_c

# Error
AGB_MgC_ha_alt1.1_1112 = (AGBplot_Alt1.1_1112) * 0.456
AGB_MgC_ha_alt1.1_1112
SDAGB_alt1.1_1112 = AGB_MgC_ha_alt1.1_1112 * 0.20
SEAGB_alt1.1_1112 = SDAGB_alt1.1_1112/sqrt(AGBplot_Alt1.1_1112_c)
SEAGB_alt1.1_1112
HighCI_alt1.1_1112 = AGB_MgC_ha_alt1.1_1112 + (1.96*SEAGB_alt1.1_1112)
LowCI_alt1.1_1112 = AGB_MgC_ha_alt1.1_1112 - (1.96*SEAGB_alt1.1_1112)

MD_AGBPlot_alt1.1_1112<-  data.frame (PlotID = unique(levels(md_vcs1112_higher10$plotID)),
                                    AGB_MgC_ha = AGB_MgC_ha_alt1.1_1112,
                                    HighCI = HighCI_alt1.1_1112,
                                    LowCI = LowCI_alt1.1_1112,
                                    Project_ID = rep("VCS1112"),
                                    EquationType = rep("Romero (2020)-eq2 [3]"))
MD_AGBPlot_alt1.1_1112

p1112_alt1.1 <- data.frame(MD_AGBPlot_alt1.1_1112 %>% filter(PlotID!="6.5") %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1.1_1112),
                                   se = sd(AGB_MgC_ha_alt1.1_1112)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1112"),
                       EquationType = rep("Romero (2020)-eq2 [3]"))
p1112_alt1.1

## Brown et al 1997 moist forest equation Y = exp{-2.134+2.530*ln(D)}
VCS1112_AGB_alt1a_function <- function(D) { # create a function with the name my_function
  AGB = exp(-2.134 + 2.530 * log(D)) * exp((0.349^2)/2)
  print(AGB)
}
## Compute AGB per plot ###
AGBPlot_alt1a_1112 <- by(md_vcs1112_higher10, md_vcs1112_higher10$plotID,
                       function(x) VCS1112_AGB_alt1a_function(D = x$D),
                       simplify = F)
AGBplot_Alt1a_1112 <- sapply(AGBPlot_alt1a_1112, sum)
AGBplot_Alt1a_1112 # kg per 1 ha

AGBplot_Alt1a_1112_c <- sapply(AGBPlot_alt1a_1112, length)
AGBplot_Alt1a_1112_c

# Error propagation Per plot ##

AGB_MgC_ha_alt1a_1112 = (AGBplot_Alt1a_1112/1000) * 0.456
AGB_MgC_ha_alt1a_1112
SDAGB_alt1a_1112 = AGB_MgC_ha_alt1a_1112 * 0.20
SEAGB_alt1a_1112 = SDAGB_alt1a_1112/sqrt(AGBplot_Alt1a_1112_c)
SEAGB_alt1a_1112
HighCI_alt1a_1112 = AGB_MgC_ha_alt1a_1112 + (1.96*SEAGB_alt1a_1112)
LowCI_alt1a_1112 = AGB_MgC_ha_alt1a_1112 - (1.96*SEAGB_alt1a_1112)

MD_AGBPlot_alt1a_1112<-  data.frame (PlotID = unique(levels(md_vcs1112_higher10$plotID)),
                                   AGB_MgC_ha = AGB_MgC_ha_alt1a_1112,
                                   HighCI = HighCI_alt1a_1112,
                                   LowCI = LowCI_alt1a_1112,
                                   Project_ID = rep("VCS1112"),
                                   EquationType = rep("Brown (1997) [5]"))
MD_AGBPlot_alt1a_1112

p1112_alt1a <- data.frame(MD_AGBPlot_alt1a_1112 %>% filter(PlotID!="6.5") %>%
                             summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1a_1112),
                                       se = sd(AGB_MgC_ha_alt1a_1112)/sqrt(n()),
                                       HCI = mAGB_MgC_ha + (1.96*se), 
                                       LCI = mAGB_MgC_ha - (1.96*se),
                                       N = n()), 
                           Project_ID = rep("VCS1112"),
                           EquationType = rep("Brown (1997) [5]"))
p1112_alt1a

# VCS1112 alternative function 2####

#equation from Araujo et al 1999
 # M = a * (b*D^c) a = 0.6, b = 4.06, c = 1.76 Araujo et al 1999 from 
VCS1112_AGB_alt2_function <- function(D) { # create a function with the name my_function
  AGB = 0.6 *(4.06 * (D^1.76)) * exp((0.349^2)/2)
  print(AGB)
}
AGBPlot_alt2_1112 <- by(md_vcs1112_higher10, md_vcs1112_higher10$plotID,
                            function(x) VCS1112_AGB_alt2_function(D = x$D),
                            simplify = F)
AGBplot_Alt2_1112 <- sapply(AGBPlot_alt2_1112, sum)
AGBplot_Alt2_1112 # kg per 1 ha

AGBplot_Alt2_1112_c <- sapply(AGBPlot_alt2_1112, length)
AGBplot_Alt2_1112_c

# Error propagation Per plot ##

AGB_MgC_ha_alt2_1112 = (AGBplot_Alt2_1112/1000) * 0.456
AGB_MgC_ha_alt2_1112
SDAGB_alt2_1112 = AGB_MgC_ha_alt2_1112 * 0.20
SEAGB_alt2_1112 = SDAGB_alt2_1112/sqrt(AGBplot_Alt2_1112_c)
SEAGB_alt2_1112
HighCI_alt2_1112 = AGB_MgC_ha_alt2_1112 + (1.96*SEAGB_alt2_1112)
LowCI_alt2_1112 = AGB_MgC_ha_alt2_1112 - (1.96*SEAGB_alt2_1112)

MD_AGBPlot_alt2_1112<-  data.frame (PlotID = unique(levels(md_vcs1112_higher10$plotID)),
                                   AGB_MgC_ha = AGB_MgC_ha_alt2_1112,
                                   HighCI = HighCI_alt2_1112,
                                   LowCI = LowCI_alt2_1112,
                                   Project_ID = rep("VCS1112"),
                                   EquationType = rep("Araujo (1999) [4]"))
MD_AGBPlot_alt2_1112

p1112_alt2 <- data.frame(MD_AGBPlot_alt2_1112 %>% filter(PlotID!="6.5") %>%
                            summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt2_1112),
                                      se = sd(AGB_MgC_ha_alt2_1112)/sqrt(n()),
                                      HCI = mAGB_MgC_ha + (1.96*se), 
                                      LCI = mAGB_MgC_ha - (1.96*se),
                                      N = n()), 
                          Project_ID = rep("VCS1112"),
                          EquationType = rep("Araujo (1999) [4]"))
p1112_alt2

# VCS1112 alternative function 3####
# AGB = exp(−8.26077 + (1.73728 * log(D)) + (0.89154 * log(H)) + (0.96957 * log(WD))) from Romero et al 2020

VCS1112_AGB_alt3_function <- function(D,H,WD) {
  AGB = exp(-8.26077 + (1.73728 * log(D)) + (0.89154 * log(H)) + (0.96957 * log(WD))) * exp((0.349^2)/2)
  print(AGB)
}
AGBPlot_alt3_1112 <- by(md_vcs1112_higher10, md_vcs1112_higher10$plotID,
                        function(x) VCS1112_AGB_alt3_function(D = x$D, H = x$Hfeld, WD = x$WD),
                        simplify = F)
AGBplot_Alt3_1112 <- sapply(AGBPlot_alt3_1112, sum)
AGBplot_Alt3_1112 # kg per 1 ha

AGBplot_Alt3_1112_c <- sapply(AGBPlot_alt3_1112, length)
AGBplot_Alt3_1112_c

# Error propagation Per plot ##

AGB_MgC_ha_alt3_1112 = (AGBplot_Alt3_1112) * 0.456
AGB_MgC_ha_alt3_1112
SDAGB_alt3_1112 = AGB_MgC_ha_alt3_1112 * 0.20
SEAGB_alt3_1112 = SDAGB_alt3_1112/sqrt(AGBplot_Alt3_1112_c)
SEAGB_alt3_1112
HighCI_alt3_1112 = AGB_MgC_ha_alt3_1112 + (1.96*SEAGB_alt3_1112)
LowCI_alt3_1112 = AGB_MgC_ha_alt3_1112 - (1.96*SEAGB_alt3_1112)

MD_AGBPlot_alt3_1112<-  data.frame (PlotID = unique(levels(md_vcs1112_higher10$plotID)),
                                    AGB_MgC_ha = AGB_MgC_ha_alt3_1112,
                                    HighCI = HighCI_alt3_1112,
                                    LowCI = LowCI_alt3_1112,
                                    Project_ID = rep("VCS1112"),
                                    EquationType = rep("Romero (2020)-eq3 [3]"))
MD_AGBPlot_alt3_1112

p1112_alt3 <- data.frame(MD_AGBPlot_alt3_1112 %>% filter(PlotID!="6.5") %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt3_1112),
                                     se = sd(AGB_MgC_ha_alt3_1112)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1112"),
                         EquationType = rep("Romero (2020)-eq3 [3]"))
p1112_alt3

## VCS 1112 alt function if needed
# AGB = 1000 * 0.6*exp(3.323 + 2.546*log(D/100)) a = 0.6, b = 3.323, c = 2.546 from Carvalho et al 1998 in Keller et al 2001

# VCS1112 - alt function 4 ###
# ln(Dry weight) = a + b ln(Diameter)] from Nogueira et al 2008, a = -1.716, b = 2.413

VCS1112_AGB_alt4_function <- function(D) { # Nogueira 2008
  AGB = exp(-1.716 + (2.413 * log(D))) * exp((0.349^2)/2)
  print(AGB) # AGB in Mg
}
AGBPlot_alt4_1112 <- by(md_vcs1112_higher10, md_vcs1112_higher10$plotID,
                            function(x) VCS1112_AGB_alt4_function(D = x$D),
                            simplify = F)
AGBPlot_Alt4_1112 <- sapply(AGBPlot_alt4_1112, sum)
AGBPlot_Alt4_1112 # kg per 1 ha

AGBplot_Alt4_1112_c <- sapply(AGBPlot_alt4_1112, length)
AGBplot_Alt4_1112_c

# Error 

AGB_MgC_ha_alt4_1112 = (AGBPlot_Alt4_1112/1000) * 0.456
AGB_MgC_ha_alt4_1112
SDAGB_alt4_1112 = AGB_MgC_ha_alt4_1112 * 0.20
SEAGB_alt4_1112 = SDAGB_alt4_1112/sqrt(AGBplot_Alt4_1112_c)
SEAGB_alt4_1112
HighCI_alt4_1112 = AGB_MgC_ha_alt4_1112 + (1.96*SEAGB_alt4_1112)
LowCI_alt4_1112 = AGB_MgC_ha_alt4_1112 - (1.96*SEAGB_alt4_1112)

MD_AGBPlot_alt4_1112<-  data.frame (PlotID = unique(levels(md_vcs1112_higher10$plotID)),
                                    AGB_MgC_ha = AGB_MgC_ha_alt4_1112,
                                    HighCI = HighCI_alt4_1112,
                                    LowCI = LowCI_alt4_1112,
                                   Project_ID = rep("VCS1112"),
                                    EquationType = rep("Nogueira (2008) [4]"))# not used
MD_AGBPlot_alt4_1112

p1112_alt4 <- data.frame(MD_AGBPlot_alt4_1112 %>% filter(PlotID!="6.5") %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt4_1112),
                                     se = sd(AGB_MgC_ha_alt4_1112)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1112"),
                         EquationType = rep("Nogueira (2008) [4]"))
p1112_alt4

## Compiling data for plotting ####

#VCS1112_AGB_Data <- rbind(MD_AGBPlot_Chave2014_Ht_1112,MD_AGBPlot_r5_1112,
                        #  MD_AGBPlot_alt1.1_1112,MD_AGBPlot_alt2_1112,MD_AGBPlot_alt1_1112,
                        #  MD_AGBPlot_alt3_1112, MD_AGBPlot_alt4_1112)

VCS1112_AGB_Data2 <- rbind(p1112_r5,
  p1112_er,
  p1112_alt1.1,
  p1112_alt1a,
  p1112_alt2,
  p1112_alt3,
  p1112_alt4)
VCS1112_AGB_Data2

#Calcularing tCO2e/ha
vcs1112_toplot2 <- VCS1112_AGB_Data2
vcs1112_toplot2
vcs1112_toplot2$AGC_tCO2_ha = as.numeric(vcs1112_toplot2$mAGB_MgC_ha * (44/12))
vcs1112_toplot2$HCI_tCO2_ha = as.numeric(vcs1112_toplot2$HCI * (44/12))
vcs1112_toplot2$LCI_tCO2_ha = as.numeric(vcs1112_toplot2$LCI * (44/12))
vcs1112_toplot2$EquationType = as.factor(vcs1112_toplot2$EquationType)

## VCS1112 plot ####
vcs1112_toplot2
p_1112 <- ggplot(vcs1112_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha), 
                                   y = AGC_tCO2_ha, 
                                   #ymin = LCI_tCO2_ha, 
                                   #ymax = HCI_tCO2_ha, 
                                   color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,250)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS1112 (VM0007)", 
       subtitle = "6 1-ha moist forest plots")+
  theme_pubr()+ #scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0007] VCS1112 Brazil")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p_1112

#b1112<- list(geom_hline(yintercept = 231.06, color = '#003f5c',alpha=0.4),
 #             annotate("rect", ymin = 213.1644, ymax = 248.96,xmin = -Inf, xmax = Inf,alpha=0.1,linetype=2))

#p_1112_final <- p_1112 + geom_hline(aes(yintercept = 231.06), lty=2, color = "gray", cex=1.2, alpha = 0.6) + b1112
#p_1112_final

## 1112 Data Stats ###

VCS1112_AGB_Data3 <- rbind(#Wet_AGBPlot_proj_1396,
  p1112_r5,
  p1112_er,
  p1112_alt1.1,
  p1112_alt1a,
  p1112_alt2,
  p1112_alt3,
  p1112_alt4,
  p1112_pr)
VCS1112_AGB_Data3

#Calcularing tCO2e/ha
vcs1112_toplot3 <- VCS1112_AGB_Data3
vcs1112_toplot3
vcs1112_toplot3$AGC_tCO2_ha = as.numeric(vcs1112_toplot3$mAGB_MgC_ha * (44/12))
vcs1112_toplot3$HCI_tCO2_ha = as.numeric(vcs1112_toplot3$HCI * (44/12))
vcs1112_toplot3$LCI_tCO2_ha = as.numeric(vcs1112_toplot3$LCI * (44/12))
vcs1112_toplot3$EquationType = as.factor(vcs1112_toplot3$EquationType)
vcs1112_toplot3

## Project VCS1566 ####

## VCS1566 project equation ####
# ln(AGB in kg) = -1.544 + 2.37 ln(D)

VCS1566_AGB_function <- function(D) {
  AGB = exp(-1.544 + 2.37 * log(D)) * exp((0.349^2)/2)
  print(AGB) # this returns AGB in kg
}
AGBPlot_Proj1566 <- by(vcs1566_higher10, vcs1566_higher10$plotID,
                       function(x) VCS1566_AGB_function(D = x$D),
                       simplify = F)
AGBplot_ProjD1566 <- sapply(AGBPlot_Proj1566, sum)
AGBplot_ProjD1566 # kg per 1 ha

AGBplot_ProjD1566_c <- sapply(AGBPlot_Proj1566, length)
AGBplot_ProjD1566_c

# Error propagation Per plot #

AGB_MgC_ha_Proj_1566 = (AGBplot_ProjD1566/1000) * 0.47 # carbon fraction used by the project
SDAGB_proj_1566 = AGB_MgC_ha_Proj_1566 * 0.20
SEAGB_proj_1566 = SDAGB_proj_1566/sqrt(AGBplot_ProjD1566_c)
HighCI_proj_1566 = AGB_MgC_ha_Proj_1566 + (1.96*SEAGB_proj_1566)
LowCI_proj_1566 = AGB_MgC_ha_Proj_1566 - (1.96*SEAGB_proj_1566)

AGBPlot_proj1566 <-  data.frame (PlotID = unique(levels(vcs1566_higher10$plotID)),
                                   AGB_MgC_ha = AGB_MgC_ha_Proj_1566,
                                   HighCI = HighCI_proj_1566,
                                   LowCI = LowCI_proj_1566,
                                   Project_ID = rep("VCS1566"),
                                   EquationType = rep("Project [4]"))
AGBPlot_proj1566

p1566_pr <- data.frame(AGBPlot_proj1566 %>% filter(PlotID!="9") %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1566"),
                       EquationType = rep("Project [4]"))
p1566_pr

## Alternative equations for VCS1566 #### 

## Alvarez et al 2012 Tropical wet ln(AGB) = a + cln(D2 H q)
VCS1566_AGB_alt1_function <- function(D, H, WD) {
  AGB = exp(-2.289 + 0.937 * log(D^2 * H * WD)) * exp((0.349^2)/2)
  print(AGB)
}
AGBPlot_alt1_r3_1566 <- by(vcs1566_higher10, vcs1566_higher10$plotID,
                            function(x) VCS1566_AGB_alt1_function(D = x$D, H = x$Hfeld, WD = x$WD),
                            simplify = F)
AGBPlot_Alt1_r3_1566 <- sapply(AGBPlot_alt1_r3_1566, sum)
AGBPlot_Alt1_r3_1566 # kg per 1 ha

AGBPlot_Alt1_r3_1566_c <- sapply(AGBPlot_alt1_r3_1566, length)
AGBPlot_Alt1_r3_1566_c

# 20% combined error
AGB_MgC_ha_alt1_r3_1566 = (AGBPlot_Alt1_r3_1566/1000) * 0.456
AGB_MgC_ha_alt1_r3_1566
SDAGB_alt1_r3_1566 = AGB_MgC_ha_alt1_r3_1566 * 0.20
SDAGB_alt1_r3_1566
SEAGB_alt1_r3_1566 = SDAGB_alt1_r3_1566/sqrt(AGBPlot_Alt1_r3_1566_c)
SEAGB_alt1_r3_1566
HighCI_alt1_r3_1566 = AGB_MgC_ha_alt1_r3_1566 + (1.96*SEAGB_alt1_r3_1566)
LowCI_alt1_r3_1566 = AGB_MgC_ha_alt1_r3_1566 - (1.96*SEAGB_alt1_r3_1566)

Wet_AGBPlot_alt1_r3_1566<-  data.frame (PlotID = unique(levels(vcs1566_higher10$plotID)),
                                   AGB_MgC_ha = AGB_MgC_ha_alt1_r3_1566,
                                   HighCI = HighCI_alt1_r3_1566,
                                   LowCI = LowCI_alt1_r3_1566,
                                   Project_ID = rep("VCS1566"),
                                   EquationType = rep("Alvarez (2012)-I.2 [3]"))
Wet_AGBPlot_alt1_r3_1566

p1566_alt1 <- data.frame(Wet_AGBPlot_alt1_r3_1566 %>% filter(PlotID!="9") %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1_r3_1566),
                                   se = sd(AGB_MgC_ha_alt1_r3_1566)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1566"),
                       EquationType = rep("Alvarez (2012)-I.2 [3]"))
p1566_alt1

## Alternative equation 2.1 1566 ####

# Duque et al 2017

VCS1566_AGB_alt2.1_function <- function(D,H,WD) { 
  AGB = 0.089 * (D^2 * H * WD)^0.951 * exp((0.349^2)/2)
  print(AGB)
}
AGBPlot_alt2.1_1566 <- by(vcs1566_higher10, vcs1566_higher10$plotID,
                        function (x) VCS1566_AGB_alt2.1_function(D = x$D, H = x$Hfeld, WD = x$WD),
                        simplify = F)
AGBPlot_Alt2.1_1566 <- sapply(AGBPlot_alt2.1_1566, sum)
AGBPlot_Alt2.1_1566

AGBPlot_Alt2.1_1566_m <- sapply(AGBPlot_alt2.1_1566, mean)
AGBPlot_Alt2.1_1566_m 

AGBPlot_Alt2.1_1566_c <- sapply(AGBPlot_alt2.1_1566, length)
AGBPlot_Alt2.1_1566_c

# 20% combined error
AGB_MgC_ha_alt2.1_1566 = (AGBPlot_Alt2.1_1566/1000) * 0.456
AGB_MgC_ha_alt2.1_1566
SDAGB_alt2.1_1566 = AGB_MgC_ha_alt2.1_1566 * 0.20
SDAGB_alt2.1_1566
SEAGB_alt2.1_1566 = SDAGB_alt2.1_1566/sqrt(AGBPlot_Alt2.1_1566_c)
SEAGB_alt2.1_1566
HighCI_alt2.1_1566 = AGB_MgC_ha_alt2.1_1566 + (1.96*SEAGB_alt2.1_1566)
LowCI_alt2.1_1566 = AGB_MgC_ha_alt2.1_1566 - (1.96*SEAGB_alt2.1_1566)

# combining into project data frame ###
Wet_AGBPlot_alt2.1_1566 <-  data.frame (PlotID = unique(levels(vcs1566_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt2.1_1566,
                                      HighCI = HighCI_alt2.1_1566,
                                      LowCI = LowCI_alt2.1_1566,
                                      Project_ID = rep("VCS1566"),
                                      EquationType = rep("Duque (2017) [4]"))
Wet_AGBPlot_alt2.1_1566

p1566_alt2.1 <- data.frame(Wet_AGBPlot_alt2.1_1566 %>% filter(PlotID!="9") %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt2.1_1566),
                                     se = sd(AGB_MgC_ha_alt2.1_1566)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1566"),
                         EquationType = rep("Duque (2017) [4]"))
p1566_alt2.1

## Alt 3 1566####

VCS1566_AGB_alt_function3 <- function(D,WD) { # create a function with the name my_function
  AGB = exp(1.662-(1.114*log(D))+(1.169*log(D)^2)-(0.122*log(D)^3) +(0.331*log(WD))) * exp((0.336^2)/2)
  print(AGB)
}
VCS1566_AGB_alt3 <- by(vcs1566_higher10, vcs1566_higher10$plotID,
                       function (x) VCS1566_AGB_alt_function3(D = x$D, WD = x$WD),
                       simplify = F)
VCS1566_AGB_Alt3 <- sapply(VCS1566_AGB_alt3, sum)
VCS1566_AGB_Alt3 

VCS1566_AGB_Alt3_c <- sapply(VCS1566_AGB_alt3, length)
VCS1566_AGB_Alt3_c

AGB_MgC_ha_alt3_1566 = (VCS1566_AGB_Alt3/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt3_1566
SDAGB_alt3_1566 = AGB_MgC_ha_alt3_1566 * 0.20
SEAGB_alt3_1566 = SDAGB_alt3_1566/sqrt(VCS1566_AGB_Alt3_c)
SEAGB_alt3_1566
HighCI_alt3_1566 = AGB_MgC_ha_alt3_1566 + (1.96*SEAGB_alt3_1566)
LowCI_alt3_1566 = AGB_MgC_ha_alt3_1566 - (1.96*SEAGB_alt3_1566)

Wet_AGBPlot_alt3_1566 <-  data.frame (PlotID = unique(levels(vcs1566_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt3_1566,
                                      HighCI = HighCI_alt3_1566,
                                      LowCI = LowCI_alt3_1566,
                                      Project_ID = rep("VCS1566"),
                                      EquationType = rep("Alvarez (2012) [3]"))
Wet_AGBPlot_alt3_1566

p1566_alt3 <- data.frame(Wet_AGBPlot_alt3_1566 %>% filter(PlotID!="9") %>%
                             summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt3_1566),
                                       se = sd(AGB_MgC_ha_alt3_1566)/sqrt(n()),
                                       HCI = mAGB_MgC_ha + (1.96*se), 
                                       LCI = mAGB_MgC_ha - (1.96*se),
                                       N = n()), 
                           Project_ID = rep("VCS1566"),
                           EquationType = rep("Alvarez (2012) [3]"))
p1566_alt3

## Alt equation 4 15666 ####
# # Sierra et al. 2007 (Rank 4, DBH > 1) AGB = exp(-2.286 + 2.471 * log(D))

VCS1566_AGB_alt_function4 <- function(D) { 
  AGB = exp(-2.286 + (2.471 * log(D))) #* 0.091 # the latter is the correction factor and has been added to the equation based on Sierra et al 2007
  print(AGB)
}
VCS1566_AGB_alt4 <- by(vcs1566_higher10, vcs1566_higher10$plotID,
                       function (x) VCS1566_AGB_alt_function4(D = x$D),
                       simplify = F)
VCS1566_AGB_Alt4 <- sapply(VCS1566_AGB_alt4, sum)
VCS1566_AGB_Alt4 

VCS1566_AGB_Alt4_c <- sapply(VCS1566_AGB_alt4, length)
VCS1566_AGB_Alt4_c

AGB_MgC_ha_alt4_1566 = (VCS1566_AGB_Alt4/1000) * 0.456 # carbon fraction by Martin 2018
AGB_MgC_ha_alt4_1566
SDAGB_alt4_1566 = AGB_MgC_ha_alt4_1566 * 0.20
SEAGB_alt4_1566 = SDAGB_alt4_1566/sqrt(VCS1566_AGB_Alt4_c)
SEAGB_alt4_1566
HighCI_alt4_1566 = AGB_MgC_ha_alt4_1566 + (1.96*SEAGB_alt4_1566)
LowCI_alt4_1566 = AGB_MgC_ha_alt4_1566 - (1.96*SEAGB_alt4_1566)

Wet_AGBPlot_alt4_1566 <-  data.frame (PlotID = unique(levels(vcs1566_higher10$plotID)),
                                      AGB_MgC_ha = AGB_MgC_ha_alt4_1566,
                                      HighCI = HighCI_alt4_1566,
                                      LowCI = LowCI_alt4_1566,
                                      Project_ID = rep("VCS1566"),
                                      EquationType = rep("Sierra (2007) [4]"))
Wet_AGBPlot_alt4_1566

p1566_alt4 <- data.frame(Wet_AGBPlot_alt4_1566 %>% filter(PlotID!="9") %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt4_1566),
                                     se = sd(AGB_MgC_ha_alt4_1566)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1566"),
                         EquationType = rep("Sierra (2007) [4]"))
p1566_alt4

## Compiling data for plotting ####

VCS1566_AGB_Data2 <- rbind(#Wet_AGBPlot_proj_1396,
  p1566_er,
  p1566_r5,
  p1566_alt1,
  p1566_alt2.1,
  p1566_alt3,
  p1566_alt4)

VCS1566_AGB_Data2

# wrangling data

vcs1566_toplot2<-VCS1566_AGB_Data2 #%>% filter(PlotID!="9")#%>% filter(PlotID!="MD_3")
vcs1566_toplot2$EquationType = as.factor(vcs1566_toplot2$EquationType)
#vcs1566_toplot$Project_ID = as.factor(vcs1566_toplot$Project_ID)

#Calcularing tCO2e/ha
vcs1566_toplot2$AGC_tCO2_ha = as.numeric(vcs1566_toplot2$mAGB_MgC_ha * (44/12))
vcs1566_toplot2$HCI_tCO2_ha = as.numeric(vcs1566_toplot2$HCI * (44/12))
vcs1566_toplot2$LCI_tCO2_ha = as.numeric(vcs1566_toplot2$LCI * (44/12))

## Plotting VCS1566 ####
vcs1566_toplot2
p_1566 <- ggplot(vcs1566_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha), 
                                   y = AGC_tCO2_ha, 
                                   #ymin = mLCI, 
                                   #ymax = mHCI, 
                                   color = EquationType)) +
  geom_point(alpha=0.9, size=4,color = "black", shape = 21, fill = "darkgray")+ylim(0,320)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS1566 (VM0007)", 
       subtitle = "8 1-ha wet forest plots")+
  theme_pubr()+ #scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0007] VCS1566 Colombia")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p_1566

# 1566 Data Stats ##

VCS1566_AGB_Data3 <- rbind(#Wet_AGBPlot_proj_1396,
  p1566_er,
  p1566_r5,
  p1566_alt1,
  p1566_alt2.1,
  p1566_alt3,
  p1566_alt4,
  p1566_pr)
VCS1566_AGB_Data3

# wrangling data

vcs1566_toplot3 <-VCS1566_AGB_Data3 #%>% filter(PlotID!="9")#%>% filter(PlotID!="MD_3")
vcs1566_toplot3$EquationType = as.factor(vcs1566_toplot3$EquationType)
#vcs1566_toplot$Project_ID = as.factor(vcs1566_toplot$Project_ID)

#Calcularing tCO2e/ha
vcs1566_toplot3$AGC_tCO2_ha = as.numeric(vcs1566_toplot3$mAGB_MgC_ha * (44/12))
vcs1566_toplot3$HCI_tCO2_ha = as.numeric(vcs1566_toplot3$HCI * (44/12))
vcs1566_toplot3$LCI_tCO2_ha = as.numeric(vcs1566_toplot3$LCI * (44/12))

## Project 985 (Peru) ####

## Calculating AGB with Project equation ####
# Equation used AGB = WD * e^(-1.239+1.980*ln(DBH) + 0/207*ln(DBH)^2-0.0281*ln(DBH)^3) Chave et al 2005 wet forest

VCS985_AGB_function <- function(WD,D) { # create a function with the name my_function
  AGB = WD * exp(-1.239 + (1.980*log(D)) + (0.207 * log(D)^2) - (0.0281 * log(D)^3))
  print(AGB)
}
AGBPlot_Proj985 <- by(vcs985_higher10, vcs985_higher10$plotID,
                       function(x) VCS985_AGB_function(WD = x$WD, D = x$D),
                       simplify = F)
AGBplot_ProjD985 <- sapply(AGBPlot_Proj985, sum)
AGBplot_ProjD985 # kg per 1 ha

AGBplot_ProjD985_c <- sapply(AGBPlot_Proj985, length)
AGBplot_ProjD985_c

# Error 

AGB_MgC_ha_Proj_985 = (AGBplot_ProjD985/1000) * 0.47 # carbon fraction used by the project
SDAGB_proj_985 = AGB_MgC_ha_Proj_985 * 0.20
SEAGB_proj_985 = SDAGB_proj_985/sqrt(AGBplot_ProjD985_c)
HighCI_proj_985 = AGB_MgC_ha_Proj_985 + (1.96*SEAGB_proj_985)
LowCI_proj_985 = AGB_MgC_ha_Proj_985 - (1.96*SEAGB_proj_985)

AGBPlot_proj985 <-  data.frame (PlotID = unique(levels(vcs985_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_Proj_985,
                                 HighCI = HighCI_proj_985,
                                 LowCI = LowCI_proj_985,
                                 Project_ID = rep("VCS985"),
                                 EquationType = rep("Project [5]"))
AGBPlot_proj985

p985_pr <- data.frame(AGBPlot_proj985 %>% filter(PlotID!="9") %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_Proj_985),
                                  se = sd(AGB_MgC_ha_Proj_985)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS985"),
                      EquationType = rep("Project [5]"))
p985_pr

## Alternative equations for VCS985 ####

VCS985_AGB_alt_function1 <- function(D) { # Araujo et al 1999
  AGB = 0.6 *(4.06 * (D^1.76)) * exp((0.349^2)/2)
  print(AGB)
}
VCS985_AGB_alt1_r4 <- by(vcs985_higher10, vcs985_higher10$plotID,
                                      function (x) VCS985_AGB_alt_function1(D = x$D),
                                      simplify = F)
VCS985_AGB_Alt1_r4 <- sapply(VCS985_AGB_alt1_r4, sum)
VCS985_AGB_Alt1_r4

VCS985_AGB_Alt1_r4_m <- sapply(VCS985_AGB_alt1_r4, mean)
VCS985_AGB_Alt1_r4_m 

VCS985_AGB_Alt1_r4_c <- sapply(VCS985_AGB_alt1_r4, length)
VCS985_AGB_Alt1_r4_c

# 20% combined error

AGB_MgC_ha_alt1_r4_985 = (VCS985_AGB_Alt1_r4/1000) * 0.456 # carbon fraction used by the project
AGB_MgC_ha_alt1_r4_985
SDAGB_alt1_r4_985 = AGB_MgC_ha_alt1_r4_985 * 0.20
SEAGB_alt1_r4_985 = SDAGB_alt1_r4_985/sqrt(VCS985_AGB_Alt1_r4_c)
SEAGB_alt1_r4_985
HighCI_alt1_r4_985 = AGB_MgC_ha_alt1_r4_985 + (1.96*SEAGB_alt1_r4_985)
LowCI_alt1_r4_985 = AGB_MgC_ha_alt1_r4_985 - (1.96*SEAGB_alt1_r4_985)

AGBPlot_alt1_r4_1985 <-  data.frame (PlotID = unique(levels(vcs985_higher10$plotID)),
                                AGB_MgC_ha = AGB_MgC_ha_alt1_r4_985,
                                HighCI = HighCI_alt1_r4_985,
                                LowCI = LowCI_alt1_r4_985,
                                Project_ID = rep("VCS985"),
                                EquationType = rep("Araujo (1999) [4]"))
AGBPlot_alt1_r4_1985

p985_alt1 <- data.frame(AGBPlot_alt1_r4_1985 %>% filter(PlotID!="9") %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt1_r4_985),
                                  se = sd(AGB_MgC_ha_alt1_r4_985)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS985"),
                      EquationType = rep("Araujo (1999) [4]"))
p985_alt1

## VCS985 alternative equation 3 ####

VCS985_AGB_alt3_function <- function(D,H,WD) { 
  AGB = 0.089 * (D^2 * H * WD)^0.951 * exp((0.349^2)/2)# Duque et al 2017
  print(AGB)
}
VCS985_AGB_alt3_v1 <- by(vcs985_higher10, vcs985_higher10$plotID,
                         function (x) VCS985_AGB_alt3_function(D = x$D, H = x$Hfeld, WD = x$WD),
                         simplify = F)
VCS985_AGB_alt3 <- sapply(VCS985_AGB_alt3_v1, sum)
VCS985_AGB_alt3

VCS985_AGB_alt3_m <- sapply(VCS985_AGB_alt3_v1, mean)
VCS985_AGB_alt3_m 

VCS985_AGB_alt3_c <- sapply(VCS985_AGB_alt3_v1, length)
VCS985_AGB_alt3_c

# 20% combined error

AGB_MgC_ha_alt3_985 = (VCS985_AGB_alt3/1000) * 0.456 # carbon fraction used by the project
AGB_MgC_ha_alt3_985
SDAGB_alt3_985 = AGB_MgC_ha_alt3_985 * 0.20
SEAGB_alt3_985 = SDAGB_alt3_985/sqrt(VCS985_AGB_alt3_c)
SEAGB_alt3_985
HighCI_alt3_985 = AGB_MgC_ha_alt3_985 + (1.96*SEAGB_alt3_985)
LowCI_alt3_985 = AGB_MgC_ha_alt3_985 - (1.96*SEAGB_alt3_985)

AGBPlot_alt3_985 <-  data.frame (PlotID = unique(levels(vcs985_higher10$plotID)),
                                AGB_MgC_ha = AGB_MgC_ha_alt3_985,
                                HighCI = HighCI_alt3_985,
                                LowCI = LowCI_alt3_985,
                                Project_ID = rep("VCS985"),
                                EquationType = rep("Duque (2017) [4]"))
AGBPlot_alt3_985 

p985_alt3 <- data.frame(AGBPlot_alt3_985 %>% filter(PlotID!="9") %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt3_985),
                                    se = sd(AGB_MgC_ha_alt3_985)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS985"),
                        EquationType = rep("Duque (2017) [4]"))
p985_alt3

# Alt equation 4 ####
# Alvarez 2012 I.3 tropical wet

VCS985_AGB_alt4_function <- function(D, H, WD) {
  AGB = exp(-2.289 + 0.937 * log(D^2 * H * WD)) * exp((0.353^2)/2)
  print(AGB)
}
VCS985_AGB_alt4_v1 <- by(vcs985_higher10, vcs985_higher10$plotID,
                         function (x) VCS985_AGB_alt4_function(D = x$D, H = x$Hfeld, WD = x$WD),
                         simplify = F)
VCS985_AGB_alt4 <- sapply(VCS985_AGB_alt4_v1, sum)
VCS985_AGB_alt4

VCS985_AGB_alt4_m <- sapply(VCS985_AGB_alt4_v1, mean)
VCS985_AGB_alt4_m 

VCS985_AGB_alt4_c <- sapply(VCS985_AGB_alt4_v1, length)
VCS985_AGB_alt4_c

# 20% combined error

AGB_MgC_ha_alt4_985 = (VCS985_AGB_alt4/1000) * 0.456 # carbon fraction used by the project
AGB_MgC_ha_alt4_985
SDAGB_alt4_985 = AGB_MgC_ha_alt4_985 * 0.20
SEAGB_alt4_985 = SDAGB_alt4_985/sqrt(VCS985_AGB_alt4_c)
SEAGB_alt4_985
HighCI_alt4_985 = AGB_MgC_ha_alt4_985 + (1.96*SEAGB_alt4_985)
LowCI_alt4_985 = AGB_MgC_ha_alt4_985 - (1.96*SEAGB_alt4_985)

AGBPlot_alt4_985 <-  data.frame (PlotID = unique(levels(vcs985_higher10$plotID)),
                                AGB_MgC_ha = AGB_MgC_ha_alt4_985,
                                HighCI = HighCI_alt4_985,
                                LowCI = LowCI_alt4_985,
                                Project_ID = rep("VCS985"),
                                EquationType = rep("Alvarez (2012)-I.3 [4]"))
AGBPlot_alt4_985 

p985_alt4 <- data.frame(AGBPlot_alt4_985 %>% filter(PlotID!="9") %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha_alt4_985),
                                    se = sd(AGB_MgC_ha_alt4_985)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS985"),
                        EquationType = rep("Alvarez (2012)-I.3 [4]"))
p985_alt4

## Compiling data for plotting ####

VCS985_AGB_Data <- rbind(#AGBPlot_proj985,
                         AGBPlot_Chave2014_Ht_985,
                         AGBPlot_r5_985,
                         #AGBPlot_alt1_1985,
                         AGBPlot_alt1_r4_1985,
                          AGBPlot_alt3_985,
                         AGBPlot_alt4_985)
VCS985_AGB_Data 

VCS985_AGB_Data2 <- rbind(
  p985_r5,
  p985_er,
  p985_alt1,
  p985_alt3,
  p985_alt4)

VCS985_AGB_Data2

vcs985_toplot2 <-VCS985_AGB_Data2 #%>% filter(PlotID!="9")#%>% filter(PlotID!="MD_3")
vcs985_toplot2$EquationType = as.factor(vcs985_toplot2$EquationType)
#vcs985_toplot$Project_ID = as.factor(vcs985_toplot$Project_ID)

#Calcularing tCO2e/ha
vcs985_toplot2$AGC_tCO2_ha = as.numeric(vcs985_toplot2$mAGB_MgC_ha * (44/12))
vcs985_toplot2$HCI_tCO2_ha = as.numeric(vcs985_toplot2$HCI * (44/12))
vcs985_toplot2$LCI_tCO2_ha = as.numeric(vcs985_toplot2$LCI * (44/12))

## Plotting VCS985 ####
vcs985_toplot2
p_985 <- ggplot(vcs985_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha), 
                                  y = AGC_tCO2_ha, 
                                 # ymin = LCI, 
                                 # ymax = HCI, 
                                  color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,320)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS985 (VM0007)", 
       subtitle = "8 1-ha wet forest plots")+
  theme_pubr()+ #scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0007] VCS985 Peru")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p_985

# 985 Data Stats ###

VCS985_AGB_Data3 <- rbind(
  p985_r5,
  p985_er,
  p985_alt1,
  p985_alt3,
  p985_alt4,
  p985_pr)
VCS985_AGB_Data3

vcs985_toplot3 <-VCS985_AGB_Data3 #%>% filter(PlotID!="9")#%>% filter(PlotID!="MD_3")
vcs985_toplot3$EquationType = as.factor(vcs985_toplot3$EquationType)

#Calcularing tCO2e/ha
vcs985_toplot3$AGC_tCO2_ha = as.numeric(vcs985_toplot3$mAGB_MgC_ha * (44/12))
vcs985_toplot3$HCI_tCO2_ha = as.numeric(vcs985_toplot3$HCI * (44/12))
vcs985_toplot3$LCI_tCO2_ha = as.numeric(vcs985_toplot3$LCI * (44/12))

# VM0007 plot ####

#p_VM7 <- (p_985_final|p_1112_final)+p_1566_final+plot_layout(widths = c(2,1), ncol=2)
#p_VM7

p_VM7 <- (p_985|p_1112)+p_1566+plot_layout(widths = c(1,1,1), ncol=3)
p_VM7

# saving vm0007 plot ####
ggsave(filename = "FigVM0007_v8.png",
       plot = p_VM7, width = 14, height = 8, units = 'cm',
       scale = 2, dpi = 800)

p_fig2_pre <- p_VM6 /p_VM7 + plot_layout(ncol=1)  #+ p_VM9 + p_VM15 +plot_layout(ncol=1)
p_fig2_pre

# saving in high-resolution
ggsave(filename = "Fig2_SA_vm6_7_v2.png",
       plot = p_fig2_pre, width = 20, height = 16, units = 'cm',
       scale = 2, dpi = 800)

# Propagating errors####

#the AGBmonteCarlo function allows the user to propagate 
#different sources of error up to the finalAGB estimate

#In the Dpropag argument of the AGBmonteCarlo function,
#the user can set diameter measurement errors by providing
#either a residual standard error (RSE) or 
#a vector of errors (SD values) associated with diameters

#By default, the error propagation assumes that no errors were
#made on diameter measurements

KarnatakaForest$sdWD = dataWD$sdWD
KarnatakaForest$HfeldRSE = dataHfeld$RSE

# Per plot using the local HD model constructed above (modelHD)
resultMC <- by(KarnatakaForest, KarnatakaForest$plotId,
               function(x) AGBmonteCarlo(D = x$D, WD = x$WD, H = x$H, errWD = x$sdWD,
                                         HDmodel = HDmodel, Dpropag = "chave2004"),
               simplify = F)
meanAGBperplot <- unlist(sapply(resultMC, "[", 1))
credperplot <- sapply(resultMC, "[", 4)

# Per plot using the Feldpaush regional HD averaged model
resultMC <- by(KarnatakaForest, KarnatakaForest$plotId,
               function(x) AGBmonteCarlo(D = x$D, WD = x$WD, errWD = x$sdWD, H = x$Hfeld,
                                         errH = x$HfeldRSE, Dpropag = "chave2004"),
               simplify = F)
meanAGBperplotFeld <- unlist(sapply(resultMC, "[", 1))
credperplotFeld <- sapply(resultMC, "[", 4)

# Per plot using Chave et al. (2014) Equation 7
resultMC <- by(KarnatakaForest, KarnatakaForest$plotId,
               function(x)AGBmonteCarlo(D = x$D, WD = x$WD, errWD = x$sdWD,
                                        coord = cbind(x$long, x$lat),
                                        Dpropag = "chave2004"),
               simplify = F)
meanAGBperplotChave <- unlist(sapply(resultMC, "[", 1))
credperplotChave <- sapply(resultMC, "[", 4)

# Propagating errors with a standard error in wood density in all plots at once
KarnatakaForest$meanWD = KarnatakaWD$meanWD
KarnatakaForest$sdWD = KarnatakaWD$sdWD
## Not run:
resultMC <- by(KarnatakaForest, KarnatakaForest$plotId,
               function(x) AGBmonteCarlo(D = x$D, WD = x$meanWD, errWD = x$sdWD,
                                         HDmodel = HDmodel, Dpropag = "chave2004",
                                         Carbon = TRUE)) #this argument can be set to T or F
meanAGBperplot <- unlist(sapply(resultMC, "[", 1))
credperplot <- sapply(resultMC, "[", 4)
str(resultMC)












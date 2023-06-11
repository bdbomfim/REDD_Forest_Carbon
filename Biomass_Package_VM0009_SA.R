
# Using Biomass Package to compare ABG biomass estimates across REDD+ projects 
# with alternative allometric equations

#Installing and calling the package from the github repo (under development)###
install.packages("remotes")
remotes::install_github('umr-amap/BIOMASS')

#latest version released on CRAN
install.packages("BIOMASS")
createCache()

#run to use the package
library("BIOMASS")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gghighlight)
library(ggbreak)
library(patchwork)
library(ggpubr)

#To cite the package
citation("BIOMASS")

## VCS 1775 data ####

# Zambia woodlands - using Woolen et al 2017. Plots of 20 m radius (0.126 ha in size) were used within which all tree stems > 5 cm DBH were surveyed
tree_stem_data <- read_csv("data:/Mozambique_Woodlands/data/70b5cdda-72df-4007-b10e-d75b4046e603/tree_stem_data.csv")
mz_data <- data.frame(tree_stem_data)# tree_stem_data from Woodlands folder
str(mz_data)

# Data manipulation ####
# We will use survey id "5" (5 = Mabalane) and focus on trees that are alive and standing, and combine plots to total 1 ha each

mz_data_sv5 <- mz_data %>% filter(survey_id == 5) %>% filter (dead!= "TRUE") %>% filter(fallen!= "TRUE")
str(mz_data_sv5)# 15556 obs
mz_data_sv5$plot_id = as.factor(mz_data_sv5$plot_id)
unique(levels(mz_data_sv5$plot_id)) # need to gather 8 plots to obtain 1ha/plot

mz_data_sv5_up <- mz_data_sv5 %>%
  mutate(plotID = fct_collapse(plot_id,
                              "1" = c("HCN_CF10", "HCN_CF17", "HCN_CF22", "HCN_CF27", "HCN_CF41", "HCN_OF13", "HCN_OF18", "HCN_OF35"),
                              "2" = c("HCN_OF4", "HCN_OF5", "HCN_SS17", "HCN_SS27", "HCN_SS3", "HCN_SS32",  "HCN_SS33", "HCN_SS42"),
                              "3" = c("HCN_SS49", "HCN_SS61", "HCN_SS65", "HCN_SS77", "HCN_SS87", "HCN_SS89", "HCN_SS92", "MPS_CF248"),
                              "4" = c("MPS_CF249", "MPS_CF250", "MPS_CF251", "MPS_CF256", "MPS_OF605", "MPS_OF609", "MPS_OF615", "MPS_OF625"),
                              "5" = c("MPS_OF627", "MPS_OF631", "MPS_OF642", "MPS_OF643", "MPS_OF648", "MPS_OF649", "MPS_OF657", "MPS_OF662"),
                              "6" = c("MTB_CF123", "MTB_CF124", "MTB_CF125", "MTB_OF153", "MTB_OF185", "MTB_OF208", "MTB_OF212", "MTB_OF222"),
                              "7" = c("MTB_SS219", "MTB_SS220", "MTB_SS231", "MTB_SS232", "MTB_SS236", "MTB_SS244", "MTB_SS246", "MTB_SS249"),
                              "8" = c("MTC_OF135", "MTC_OF137", "MTC_OF62", "MTC_OF64", "MTC_OF73", "MTC_OF74", "MTC_OF79", "MTC_OF81")))
str(mz_data_sv5_up)
unique(levels(mz_data_sv5_up$plotID)) 

mz_data_sv5_up_filt <- mz_data_sv5_up %>%  filter(plotID == "1"|plotID == "2" | plotID == "3" | plotID == "4" | plotID == "5" | plotID == "6" | plotID == "7" | plotID == "8")
str(mz_data_sv5_up_filt)
head(mz_data_sv5_up_filt)

# Data wrangling to adjust species names
mz_data_sv5_up_filt <- mz_data_sv5_up_filt %>% separate(species_name, c("genus", "species")) 
names(mz_data_sv5_up_filt)
#names(md_brduc01)[7] <- 'family'
names(mz_data_sv5_up_filt)[8] <- 'D'

wd_1775 <- mz_data_sv5_up_filt #renaming for project 1775
str(wd_1775)

wd_1775 %>%
  group_by(plotID) %>%
  summarise(n = n())

## VCS 902 data ####

# Zimbabwe woodlands - using Woolen et al 2017. Plots of 20 m radius (0.126 ha in size) were used within which all tree stems > 5 cm DBH were surveyed
str(mz_data) #30986 individuals of  11 variables - total of 32 0.25-ha plots in a total of 8 ha total area

# Data manipulation ####
# We will use survey id "7" (7 = Gurue) and focus on trees that are alive and standing, and combine plots to total 1 ha each

mz_data_sv7 <- mz_data %>% filter(survey_id == 7) %>% filter (dead!= "TRUE") %>% filter(fallen!= "TRUE")
str(mz_data_sv7)# 15556 obs
mz_data_sv7$plot_id = as.factor(mz_data_sv7$plot_id)
unique(levels(mz_data_sv7$plot_id)) # need to gather 8 plots to obtain 1ha/plot
summary(mz_data_sv7)

mz_data_sv7_up <- mz_data_sv7 %>%
  mutate(plotID = fct_collapse(plot_id,
                               "1" = c("MHO_0b", "MHO_10b", "MHO_12a", "MHO_12b", "MHO_14a", "MHO_14b", "MHO_15a", "MHO_15b"),
                               "2" = c("MHO_19b", "MHR_19a", "MHR_19b", "MHR_22b", "MHR_4b", "MHR_8b",  "MHO_19a", "MQT_16"),
                               "3" = c("MQT_24a", "MQT_27", "MQT_2b", "MQT_32a", "MQT_40b", "MQT_42b", "MQT_43a", "NCN_17a"),
                               "4" = c("NCN_18a", "NCN_18b", "NCN_1a", "NCN_1b", "NCN_23a", "NCN_2a", "NCN_3a", "NCN_3b"),
                               "5" = c("NCP_11a", "NCP_20a", "NCP_20b", "NCP_3a", "NCP_3b", "NCP_8a", "NCP_8b", "NMP_10b"),
                               "6" = c("NMP_14a", "NMP_19b", "NMP_20a", "NMP_20b", "NMP_22b", "NMP_23a", "NMP_23b", "NMP_4a"),
                               "7" = c("NSE_1a", "NSE_20a", "NSE_22a", "NSE_22b", "NSE_5a", "NSE_5b", "NTX_0a", "NTX_14a"),
                               "8" = c("NTX_14b", "NTX_18b", "NTX_20a", "NTX_20b", "NTX_23b", "NTX_6a", "NTX_6b", "NTX_7a"),
                               "9" = c("PLK_14a", "PLK_14b", "PLK_15a", "PLK_15b", "PLK_23a", "PLK_4a", "PLK_4b", "PLK_5a"),
                               "10" = c("SRA_11a", "SRA_11b", "SRA_12a", "SRA_12b", "SRA_13a", "SRA_16a", "SRA_4a", "SRA_4b")))
str(mz_data_sv7_up)
unique(levels(mz_data_sv7_up$plotID)) 

mz_data_sv7_up_filt <- mz_data_sv7_up %>%  filter(plotID == "1"|plotID == "2" | plotID == "3" | 
                                                    plotID == "4" | plotID == "5" | plotID == "6" | 
                                                    plotID == "7" | plotID == "8"| plotID == "9"| plotID == "10")
str(mz_data_sv7_up_filt) # 1489 obs
head(mz_data_sv7_up_filt)

# Data wrangling to adjust species names
mz_data_sv7_up_filt <- mz_data_sv7_up_filt %>% separate(species_name, c("genus", "species")) 
names(mz_data_sv7_up_filt)
#names(md_brduc01)[7] <- 'family'
names(mz_data_sv7_up_filt)[8] <- 'D'

wd_902 <- mz_data_sv7_up_filt #renaming for project 1775
str(wd_902) #10 1-ha plots

## VCS 934 data ####
#map = 180 cm/ ano ## 

AND_A01_2013_2018_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/AND_A01_2013_2018_Inventory.csv")
and_a01 <- data.frame(AND_A01_2013_2018_Inventory)# AND_A01_2013_Inventory Para data
str(and_a01) # 789 individuals of  11 variables - total of 20 0.25-ha plots in a total of 5 ha total area

md_and_a01 <- data.frame(and_a01) #renaming to keep original data frame as uploaded
str(md_and_a01)
md_and_a01$dead.2013
unique(levels(as.factor(md_and_a01$dead.2013)))
md_and_a01$plot.ID

md_and_a01$plot <- md_and_a01$plot.ID
md_and_a01$plot

md_and_a01 %>%
  group_by(plot) %>%
  summarise(mean = mean(DBH.2013), n = n())

md_and_a01$plot <- as.numeric(md_and_a01$plot)

md_and_a01$plotID <- as.factor(ifelse(md_and_a01$plot<5, '1',
                                      ifelse(md_and_a01$plot>5 & md_and_a01$plot<9, '2',
                                             ifelse(md_and_a01$plot>8 & md_and_a01$plot<13, '3', 
                                                    ifelse(md_and_a01$plot>12 & md_and_a01$plot<17,'4','5')))))

# print data frame
str(md_and_a01)

# Data wrangling to adjust species names
md_and_a01 <- md_and_a01 %>% separate(scientific.name, c("genus", "species"))
names(md_and_a01)
str(md_and_a01) # 836
names(md_and_a01)[8] <- 'family'
names(md_and_a01)[10] <- 'D'

md_and_a01$type = as.factor(md_and_a01$type)

md_and_a01_f <- md_and_a01 %>% filter(dead.2013!= "TRUE") %>% filter(type== "O")
str(md_and_a01_f) # 335 individuals

md_934 <- md_and_a01_f #renaming for project 934

## Inspecting and Manipulating plot data####

# Filter out NA's in D column ###

wd_1775_filt <- wd_1775 %>% drop_na(D)
str(wd_1775_filt) # 7597 individuals in 8 ha
summary(wd_1775_filt) # max D = 81.4 cm

wd_902_filt <- wd_902 %>% drop_na(D)
str(wd_902_filt) # 1489 individuals in 10 ha
summary(wd_902_filt) # max D = 110.50 cm

md_934_filt <- md_934 %>% drop_na(D)
str(md_934_filt) # 3355 individuals in 5 ha
summary(md_934_filt) # max D = 142 cm

# Filter out dbh sizes lower than 10 cm##

wd_1775_higher10 <- wd_1775_filt %>% filter (D > 9.999)
str(wd_1775_higher10) # 2658 trees
summary(wd_1775_higher10) #Max DBH = 81.40 cm

wd_902_higher10 <- wd_902_filt %>% filter (D > 9.999)
str(wd_902_higher10) # 799 trees
summary(wd_902_higher10) #Max DBH = 110.5 cm

md_934_higher10 <- md_934_filt %>% filter (D > 9.999)
str(md_934_higher10) # 335 trees
summary(md_934_higher10) # Max D = 142

# Inspecting data by plot

wd_1775_higher10 %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n()) #8 1-ha plots

wd_902_higher10 %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n()) #10 1-ha plots

md_934_higher10 %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n()) #5 1-ha plots

# Checking and correcting taxonomy####

Taxowd_1775<-correctTaxo(genus = wd_1775_higher10$genus, 
                        species = wd_1775_higher10$species,
                        score = 0.5,
                        useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
wd_1775_higher10$genusCorr <- Taxowd_1775$genusCorrected
wd_1775_higher10$speciesCorr <- Taxowd_1775$speciesCorrected

Taxowd_902<-correctTaxo(genus = wd_902_higher10$genus, 
                         species = wd_902_higher10$species,
                         score = 0.5,
                         useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
wd_902_higher10$genusCorr <- Taxowd_902$genusCorrected
wd_902_higher10$speciesCorr <- Taxowd_902$speciesCorrected

Taxomd_934<-correctTaxo(genus = md_934_higher10$genus, 
                        species = md_934_higher10$species,
                        score = 0.5,
                        useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
md_934_higher10$genusCorr <- Taxomd_934$genusCorrected
md_934_higher10$speciesCorr <- Taxomd_934$speciesCorrected

# Retrieving APG III Families and Orders from Genus names####

APG_1775 <- getTaxonomy(wd_1775_higher10$genusCorr, findOrder = T)
wd_1775_higher10$familyAPG <- APG_1775$family
wd_1775_higher10$orderAPG <- APG_1775$order

APGmd_934 <- getTaxonomy(md_934_higher10$genusCorr, findOrder = T)
md_934_higher10$familyAPG <- APGmd_934$family
md_934_higher10$orderAPG <- APGmd_934$order

APGmd_902 <- getTaxonomy(wd_902_higher10$genusCorr, findOrder = T)
wd_902_higher10$familyAPG <- APGmd_902$family
wd_902_higher10$orderAPG <- APGmd_902$order

# Retrieving wood density data####

#The WD can either be attributed to an individual at a species, 
#genus, family or stand level.

# Compute the Wood Density up to the genus level 
# and then give the mean wood density per stand (or not)

WDdata_1775<-getWoodDensity(genus = Taxowd_1775$genusCorrected, 
                             species = Taxowd_1775$speciesCorrected, 
                             stand = wd_1775_higher10$plotID,
                             region = "AfricaTrop")
# adding WD column to plot dataframe
wd_1775_higher10$WD <- WDdata_1775$meanWD
wd_1775_higher10$sdWD <- WDdata_1775$sdWD

WDdatamd_934<-getWoodDensity(genus = Taxomd_934$genusCorrected, 
                             species = Taxomd_934$speciesCorrected, 
                             stand = md_934_higher10$plotID,
                             region = "AfricaTrop")
# adding WD column to plot dataframe
md_934_higher10$WD <- WDdatamd_934$meanWD
md_934_higher10$sdWD <- WDdatamd_934$sdWD

WDdata_902<-getWoodDensity(genus = Taxowd_902$genusCorrected, 
                             species = Taxowd_902$speciesCorrected, 
                             stand = wd_902_higher10$plotID,
                             region = "SouthAmericaTrop")
# adding WD column to plot dataframe
wd_902_higher10$WD <- WDdata_902$meanWD
wd_902_higher10$sdWD <- WDdata_902$sdWD

#Retrieve Tree height data####

# Retrieve height data from a Feldpaush et al. (2012) averaged model

dataHfeld_1775 <- retrieveH(D = wd_1775_higher10$D, region = "EAfrica")
wd_1775_higher10$Hfeld <- dataHfeld_1775$H
wd_1775_higher10$HRSE <- dataHfeld_1775$RSE

dataHfeld_934 <- retrieveH(D = md_934_higher10$D, region = "CAfrica")
md_934_higher10$Hfeld <- dataHfeld_934$H
md_934_higher10$HRSE <- dataHfeld_934$RSE

dataHfeld_902 <- retrieveH(D = wd_902_higher10$D, region = "EAfrica")
wd_902_higher10$Hfeld <- dataHfeld_902$H
wd_902_higher10$HRSE <- dataHfeld_902$RSE

# Using coordinates to obtain height data
   #HchaveE<-retrieveH(D = we_plot65_higher10$D, coord = cbind(NouraguesHD$long, NouraguesHD$lat))

# Retrieve height data from Chave et al. (2012) equation 6
long_1775 <- 32.668431
lat_1775 <- -23.853458
coord_1775 <- c(long_1775, lat_1775)
coord_1775
wd_1775_higher10$lat <- rep(-23.853458)
wd_1775_higher10$long <- rep(32.668431)

dataHchave_1775 <- retrieveH(D = wd_1775_higher10$D,
                            coord = coord_1775)
#region = "SAmerica")
dataHchave_1775
wd_1775_higher10$Hchave<- dataHchave_1775$H
#str(wd_1775_higher10)

md_934_higher10$lat <- rep(-2.53)
md_934_higher10$long <- rep(-46.83)
long_934 <- -46.83
lat_934 <- -2.53
coord_934 <- c(long_934, lat_934)
coord_934

dataHchave_934 <- retrieveH(D = md_934_higher10$D,
                            coord = cbind(md_934_higher10$long, md_934_higher10$lat))#, #cbind(md_brana01_higher10$lat, md_brana01_higher10$long))#,
#region = "SAmerica")
dataHchave_934
md_934_higher10$Hchave<- dataHchave_934$H
#str(md_934_higher10)

long_902 <- 37.005643
lat_902 <- -15.468194
wd_902_higher10$lat <- rep(-15.468194)
wd_902_higher10$long <- rep(37.005643)

coord_902 <- c(long_902, lat_902)
coord_902

dataHchave_902 <- retrieveH(D = wd_902_higher10$D,
                            coord = cbind(wd_902_higher10$long, wd_902_higher10$lat))#, #cbind(md_brana01_higher10$lat, md_brana01_higher10$long))#,
#region = "SAmerica")
dataHchave_902
wd_902_higher10$Hchave<- dataHchave_902$H

####
# Once diameter, wood density and height values ##
# have been retrieved for each tree ##
###

## Compute AGB Chave 2014 equation ####

# 1775

AGBPlot_ChHf_1775 <- by(wd_1775_higher10, wd_1775_higher10$plotID,
                   function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                   simplify = F)
AGBplot_D_H_WD_1775 <- sapply(AGBPlot_ChHf_1775, sum)
AGBplot_D_H_WD_1775 # Mg per 1 ha

AGBplot_D_H_WD_1775_c <- sapply(AGBPlot_ChHf_1775, length)
AGBplot_D_H_WD_1775_c

# Error 
AGB_MgC_ha_ChHf_1775 = (AGBplot_D_H_WD_1775) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_ChHf_1775
SDAGB_ChHf_1775= AGB_MgC_ha_ChHf_1775 * 0.20
SEAGB_ChHf_1775 = SDAGB_ChHf_1775/sqrt(AGBplot_D_H_WD_1775_c)
SEAGB_ChHf_1775
HighCI_ChHf_1775 = AGB_MgC_ha_ChHf_1775 + (1.96*SEAGB_ChHf_1775)
LowCI_ChHf_1775 = AGB_MgC_ha_ChHf_1775 - (1.96*SEAGB_ChHf_1775)

AGBPlot_ChHf_1775 <-  data.frame (PlotID = unique(levels(wd_1775_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_ChHf_1775,
                                  HighCI = HighCI_ChHf_1775,
                                  LowCI = LowCI_ChHf_1775,
                                  Project_ID = rep("VCS1775"),
                                  EquationType = rep("Chave (2014) [6]"))

AGBPlot_ChHf_1775_f <- AGBPlot_ChHf_1775 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_ChHf_1775_f

p1775_er <- data.frame(AGBPlot_ChHf_1775_f %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1775"),
                       EquationType = rep("Chave (2014) [6]"))
p1775_er

 ## 1775 DBH <= 65 cm
# If running equations against the project's choice, then use the 
# filtered data frame to include only trees with DBH lower than 65 cm

wd_1775_higher10_low65 <- wd_1775_higher10 %>% filter (D < 66) # to be used when using the project's equation

AGBPlot_ChHf_1775_l65 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                        function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                        simplify = F)
AGBplot_D_H_WD_1775_l65 <- sapply(AGBPlot_ChHf_1775_l65, sum)
AGBplot_D_H_WD_1775_l65 # Mg per 1 ha

AGBplot_D_H_WD_1775_c_l65 <- sapply(AGBPlot_ChHf_1775_l65, length)
AGBplot_D_H_WD_1775_c_l65

# Error 
AGB_MgC_ha_ChHf_1775_l65 = (AGBplot_D_H_WD_1775_l65) * 0.456 # carbon fraction by Martin et al 2018
SDAGB_ChHf_1775_l65= AGB_MgC_ha_ChHf_1775_l65 * 0.20
SEAGB_ChHf_1775_l65 = SDAGB_ChHf_1775_l65/sqrt(AGBplot_D_H_WD_1775_c_l65)
HighCI_ChHf_1775_l65 = AGB_MgC_ha_ChHf_1775_l65 + (1.96*SEAGB_ChHf_1775_l65)
LowCI_ChHf_1775_l65 = AGB_MgC_ha_ChHf_1775_l65 - (1.96*SEAGB_ChHf_1775_l65)

AGBPlot_ChHf_1775_l65 <-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_ChHf_1775_l65,
                                  HighCI = HighCI_ChHf_1775_l65,
                                  LowCI = LowCI_ChHf_1775_l65,
                                  Project_ID = rep("VCS1775"),
                                  EquationType = rep("Chave (2014) [6]"))

AGBPlot_ChHf_1775_f_l65 <- AGBPlot_ChHf_1775_l65 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_ChHf_1775_f_l65

p1775_er_l65 <- data.frame(AGBPlot_ChHf_1775_f_l65 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1775"),
                       EquationType = rep("Chave (2014) [6]"))
p1775_er_l65

# 902

AGBPlot_ChHf_902 <- by(wd_902_higher10, wd_902_higher10$plotID,
                        function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                        simplify = F)
AGBplot_D_H_WD_902 <- sapply(AGBPlot_ChHf_902, sum)
AGBplot_D_H_WD_902 # Mg per 1 ha

AGBplot_D_H_WD_902_c <- sapply(AGBPlot_ChHf_902, length)
AGBplot_D_H_WD_902_c

# Error
AGB_MgC_ha_ChHf_902 = (AGBplot_D_H_WD_902) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_ChHf_902
SDAGB_ChHf_902 = AGB_MgC_ha_ChHf_902 * 0.20
SEAGB_ChHf_902 = SDAGB_ChHf_902/sqrt(AGBplot_D_H_WD_902_c)
HighCI_ChHf_902 = AGB_MgC_ha_ChHf_902 + (1.96*SEAGB_ChHf_902)
LowCI_ChHf_902 = AGB_MgC_ha_ChHf_902 - (1.96*SEAGB_ChHf_902)

AGBPlot_ChHf_902 <-  data.frame (PlotID = unique(levels(wd_902_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_ChHf_902,
                                  HighCI = HighCI_ChHf_902,
                                  LowCI = LowCI_ChHf_902,
                                  Project_ID = rep("VCS902"),
                                  EquationType = rep("Chave (2014) [6]"))
AGBPlot_ChHf_902_f <- AGBPlot_ChHf_902 %>% filter(AGB_MgC_ha > 0)
AGBPlot_ChHf_902_f

p902_er <- data.frame(AGBPlot_ChHf_902_f %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS902"),
                       EquationType = rep("Chave (2014) [6]"))
p902_er

# 934

AGBPlot_ChHf_934 <- by(md_934_higher10, md_934_higher10$plotID,
                       function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                       simplify = F)
AGBPlot_ChHf_934
AGBplot_D_H_WD_934 <- sapply(AGBPlot_ChHf_934, sum)
AGBplot_D_H_WD_934 

AGBplot_D_H_WD_934_c <- sapply(AGBPlot_ChHf_934, length)
AGBplot_D_H_WD_934_c

# 20% error
AGB_MgC_ha_ChHf_934 = (AGBplot_D_H_WD_934) * 0.456 # carbon fraction by Martin et al 2018
SDAGB_ChHf_934 = AGB_MgC_ha_ChHf_934 * 0.20
SEAGB_ChHf_934 = SDAGB_ChHf_934/sqrt(AGBplot_D_H_WD_934_c)
HighCI_ChHf_934 = AGB_MgC_ha_ChHf_934 + (1.96*SEAGB_ChHf_934)
LowCI_ChHf_934 = AGB_MgC_ha_ChHf_934 - (1.96*SEAGB_ChHf_934)

AGBPlot_ChHf_934 <-  data.frame (PlotID = unique(levels(md_934_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_ChHf_934,
                                 HighCI = HighCI_ChHf_934,
                                 LowCI = LowCI_ChHf_934,
                                 Project_ID = rep("VCS934"),
                                 EquationType = rep("Chave (2014) [6]"))
AGBPlot_ChHf_934 

p934_er <- data.frame(AGBPlot_ChHf_934 %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                  se = sd(AGB_MgC_ha)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS934"),
                      EquationType = rep("Chave (2014) [6]"))
p934_er

# AGB(Mg) Chave 2005 forest specific ####
## VCS 1775  - Chave2005 dry forest II.5

VCS1775_AGB_r5 <- function(D, WD) { # create a function with the name my_function
    AGB = exp(-1.083 + (2.266 * log(D)) + log(WD))
    print(AGB) # this returns AGB in kg
} 
AGBPlot_r5_1775_v1 <- by(wd_1775_higher10, wd_1775_higher10$plotID,
                        function(x) VCS1775_AGB_r5(D = x$D, WD = x$WD),
                        simplify = F)
AGBplot_r5_1775 <- sapply(AGBPlot_r5_1775_v1, sum)
AGBplot_r5_1775 # kg per 1 ha

AGBplot_r5_1775_c <- sapply(AGBPlot_r5_1775_v1, length)
AGBplot_r5_1775_c

# Error 

AGB_MgC_ha_r5_1775 = (AGBplot_r5_1775/1000) * 0.456
SDAGB_r5_1775 = AGB_MgC_ha_r5_1775 * 0.20
SEAGB_r5_1775 = SDAGB_r5_1775/sqrt(AGBplot_r5_1775_c)
SEAGB_r5_1775
HighCI_r5_1775 = AGB_MgC_ha_r5_1775 + (1.96*SEAGB_r5_1775)
LowCI_r5_1775 = AGB_MgC_ha_r5_1775 - (1.96*SEAGB_r5_1775)

AGBPlot_r5_1775<-  data.frame (PlotID = unique(levels(wd_1775_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_r5_1775,
                                 HighCI = HighCI_r5_1775,
                                 LowCI = LowCI_r5_1775,
                                 Project_ID = rep("VCS1775"),
                                 EquationType = rep("Chave (2005) [5]")) #extra
AGBPlot_r5_1775# 1775

AGBPlot_r5_1775_f <- AGBPlot_r5_1775 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_r5_1775_f

p1775_r5 <- data.frame(AGBPlot_r5_1775_f %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1775"),
                       EquationType = rep("Chave (2005) [5]"))
p1775_r5

 # 1775 DBH <= 65 cm

VCS1775_AGB_r5 <- function(D, WD) { # create a function with the name my_function
  AGB = exp(-1.083 + (2.266 * log(D)) + log(WD))
  print(AGB) # this returns AGB in kg
} 
AGBPlot_r5_1775_v1_l65 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                         function(x) VCS1775_AGB_r5(D = x$D, WD = x$WD),
                         simplify = F)
AGBplot_r5_1775_l65 <- sapply(AGBPlot_r5_1775_v1_l65, sum)
AGBplot_r5_1775_l65 # kg per 1 ha

AGBplot_r5_1775_c_l65 <- sapply(AGBPlot_r5_1775_v1_l65, length)
AGBplot_r5_1775_c_l65

# Error 

AGB_MgC_ha_r5_1775_l65 = (AGBplot_r5_1775_l65/1000) * 0.456
SDAGB_r5_1775_l65 = AGB_MgC_ha_r5_1775_l65 * 0.20
SEAGB_r5_1775_l65 = SDAGB_r5_1775_l65/sqrt(AGBplot_r5_1775_c_l65)
HighCI_r5_1775_l65 = AGB_MgC_ha_r5_1775_l65 + (1.96*SEAGB_r5_1775_l65)
LowCI_r5_1775_l65 = AGB_MgC_ha_r5_1775_l65 - (1.96*SEAGB_r5_1775_l65)

AGBPlot_r5_1775_l65<-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                               AGB_MgC_ha = AGB_MgC_ha_r5_1775_l65,
                               HighCI = HighCI_r5_1775_l65,
                               LowCI = LowCI_r5_1775_l65,
                               Project_ID = rep("VCS1775"),
                               EquationType = rep("Chave (2005) [5]"))
AGBPlot_r5_1775_l65# 1775

AGBPlot_r5_1775_f_l65 <- AGBPlot_r5_1775_l65 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_r5_1775_f_l65

p1775_r5_l65 <- data.frame(AGBPlot_r5_1775_f_l65 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1775"),
                       EquationType = rep("Chave (2005) [5]"))
p1775_r5_l65

# 902 Chave 2005 dry tropical forest II.5

VCS902_AGB_r5 <- function(D, WD) { # create a function with the name my_function
  AGB = exp(-1.083 + (2.266 * log(D)) + log(WD))
  print(AGB) # this returns AGB in kg
} 
AGBPlot_r5_902 <- by(wd_902_higher10, wd_902_higher10$plotID,
                         function(x) VCS902_AGB_r5(D = x$D, WD = x$WD),
                         simplify = F)
AGBplot_D_WD_902 <- sapply(AGBPlot_r5_902, sum)
AGBplot_D_WD_902 # Mg per 1 ha

AGBplot_r5_902_c <- sapply(AGBPlot_r5_902, length)
AGBplot_r5_902_c

# Error 

AGB_MgC_ha_r5_902 = (AGBplot_D_WD_902/1000) * 0.456
AGB_MgC_ha_r5_902
SDAGB_r5_902 = AGB_MgC_ha_r5_902 * 0.20
SEAGB_r5_902 = SDAGB_r5_902/sqrt(AGBplot_r5_902_c)
SEAGB_r5_902
HighCI_r5_902 = AGB_MgC_ha_r5_902 + (1.96*SEAGB_r5_902)
LowCI_r5_902 = AGB_MgC_ha_r5_902 - (1.96*SEAGB_r5_902)

AGBPlot_r5_902 <-  data.frame (PlotID = unique(levels(wd_902_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_r5_902,
                                 HighCI = HighCI_r5_902,
                                 LowCI = LowCI_r5_902,
                                 Project_ID = rep("VCS902"),
                                 EquationType = rep("Chave (2005) [5]"))
AGBPlot_r5_902 

AGBPlot_r5_902_f <- AGBPlot_r5_902 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_r5_902_f

p902_r5 <- data.frame(AGBPlot_r5_902_f %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS902"),
                       EquationType = rep("Chave (2005) [5]"))
p902_r5

# 934
# Chave et al 2005 II.5 dry forest
VCS934_AGB_r5 <- function(D, WD) { # create a function with the name my_function
  AGB = exp(-1.083 + (2.266 * log(D)) + log(WD))
  print(AGB) # this returns AGB in kg
} 
AGBPlot_r5_934_v1 <- by(md_934_higher10, md_934_higher10$plotID,
                     function(x) VCS934_AGB_r5(D = x$D, WD = x$WD),
                     simplify = F)
AGBplot_r5_934 <- sapply(AGBPlot_r5_934_v1, sum)
AGBplot_r5_934 # kg per 1 ha

AGBplot_r5_934_c <- sapply(AGBPlot_r5_934_v1, length)
AGBplot_r5_934_c

# 20% error
AGB_MgC_ha_r5_934 = (AGBplot_r5_934/1000) * 0.456 # carbon fraction by Martin et al 2018
SDAGB_r5_934 = AGB_MgC_ha_r5_934 * 0.20
SEAGB_r5_934 = SDAGB_r5_934/sqrt(AGBplot_r5_934_c)
HighCI_r5_934 = AGB_MgC_ha_r5_934 + (1.96*SEAGB_r5_934)
LowCI_r5_934 = AGB_MgC_ha_r5_934 - (1.96*SEAGB_r5_934)

AGBPlot_934_r5 <-  data.frame (PlotID  = unique(levels(md_934_higher10$plotID)),
                                AGB_MgC_ha = AGB_MgC_ha_r5_934,
                                HighCI = HighCI_r5_934,
                                LowCI = LowCI_r5_934,
                                Project_ID = rep("VCS934"),
                                EquationType = rep("Chave (2005) [5]"))
AGBPlot_934_r5

p934_r5 <- data.frame(AGBPlot_934_r5 %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                  se = sd(AGB_MgC_ha)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS934"),
                      EquationType = rep("Chave (2005) [5]"))
p934_r5

# Project VCS1775 (Zambia) ####

# by Chidumayo (2013) Wood Biomass (kg) = 0.0446 * DBH^2.765
#by Mugasha et al 2013 Wood Biomass (kg) = 0.1027 * DBH^2.4798
wd_1775_higher10_low65 <- wd_1775_higher10 %>% filter (D < 66)

VCS1775_AGB_low65_function <- function(D) { # function for trees with dbh < 65 cm
  AGB = 0.0446 * D^2.765 * exp((0.4^2)/2)
  print(AGB) # this returns AGB in kg
}

AGBPlot_Projlo65_1775 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                        function(x) VCS1775_AGB_low65_function(D = x$D),
                        simplify = F)
AGBPlot_Projlo65_1775
AGBplot_ProjD_lo65_1775 <- sapply(AGBPlot_Projlo65_1775, sum)
AGBplot_ProjD_lo65_1775 # kg per 1 ha

AGBplot_ProjD_lo65_1775_c <- sapply(AGBPlot_Projlo65_1775, sum)
AGBplot_ProjD_lo65_1775_c 

# Error 
AGB_MgC_ha_proj_lo65_1775 = (AGBplot_ProjD_lo65_1775/1000) * 0.50 # carbon fraction used by the project
SDAGB_projlo65_1775 = AGB_MgC_ha_proj_lo65_1775 * 0.20
SEAGB_proj_1775 = SDAGB_projlo65_1775/sqrt(AGBplot_ProjD_lo65_1775_c)
HighCI_proj_1775 = AGB_MgC_ha_proj_lo65_1775 + (1.96*SEAGB_proj_1775)
LowCI_proj_1775 = AGB_MgC_ha_proj_lo65_1775 - (1.96*SEAGB_proj_1775)

AGBPlot_proj_lo65_1775 <-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_proj_lo65_1775,
                                  HighCI = HighCI_proj_1775,
                                  LowCI = LowCI_proj_1775,
                                  Project_ID = rep("VCS1775"),
                                  EquationType = rep("Project [4]"))
AGBPlot_proj_lo65_1775 

p1775_pr <- data.frame(AGBPlot_proj_lo65_1775 %>% filter(AGB_MgC_ha > 0) %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1775"),
                       EquationType = rep("Project [4]"))
p1775_pr

## Alternative equations for VCS1775 ####

## Chamshama et al 2004 Biomass = 0.0625 * D^2.553)
VCS1775_AGB_alt_function1 <- function(D) {
  AGB = 0.0625 * D^2.553 * exp((0.4^2)/2)
  print(AGB)
}
AGBPlot_alt1_1775 <- by(wd_1775_higher10, wd_1775_higher10$plotID,
                   function(x) VCS1775_AGB_alt_function1(D = x$D),
                   simplify = F)
AGBplot_Alt1_1775 <- sapply(AGBPlot_alt1_1775, sum)
AGBplot_Alt1_1775 # kg per 1 ha

AGBplot_Alt1_1775_c <- sapply(AGBPlot_alt1_1775, length)
AGBplot_Alt1_1775_c

# Error
AGB_MgC_ha_alt1_1775 = (AGBplot_Alt1_1775/1000) * 0.456
AGB_MgC_ha_alt1_1775
SDAGB_alt1_1775 = AGB_MgC_ha_alt1_1775 * 0.20
SEAGB_alt1_1775 = SDAGB_alt1_1775/sqrt(AGBplot_Alt1_1775_c)
SEAGB_alt1_1775
HighCI_alt1_1775 = AGB_MgC_ha_alt1_1775 + (1.96*SEAGB_alt1_1775)
LowCI_alt1_1775 = AGB_MgC_ha_alt1_1775 - (1.96*SEAGB_alt1_1775)

AGBPlot_alt1_1775 <-  data.frame (PlotID = unique(levels(wd_1775_higher10$plotID)),
                               AGB_MgC_ha = AGB_MgC_ha_alt1_1775,
                               HighCI = HighCI_alt1_1775,
                               LowCI = LowCI_alt1_1775,
                               Project_ID = rep("VCS1775"),
                               EquationType = rep("Chamshama (2004) [4]"))
AGBPlot_alt1_1775

AGBPlot_alt1_1775_f <- AGBPlot_alt1_1775 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_alt1_1775_f

p1775_alt1 <- data.frame(AGBPlot_alt1_1775_f %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1775"),
                       EquationType = rep("Chamshama (2004) [4]"))
p1775_alt1

 # Alt 1 DBH <= 65 cm

VCS1775_AGB_alt_function1 <- function(D) {
  AGB = 0.0625 * D^2.553 * exp((0.4^2)/2)
  print(AGB)
}
AGBPlot_alt1_1775_l65 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                        function(x) VCS1775_AGB_alt_function1(D = x$D),
                        simplify = F)
AGBplot_Alt1_1775_l65 <- sapply(AGBPlot_alt1_1775_l65, sum)

AGBplot_Alt1_1775_c_l65 <- sapply(AGBPlot_alt1_1775_l65, length)

# Error
AGB_MgC_ha_alt1_1775_l65 = (AGBplot_Alt1_1775_l65/1000) * 0.456
SDAGB_alt1_1775_l65 = AGB_MgC_ha_alt1_1775_l65 * 0.20
SEAGB_alt1_1775_l65 = SDAGB_alt1_1775_l65/sqrt(AGBplot_Alt1_1775_c_l65)
HighCI_alt1_1775_l65 = AGB_MgC_ha_alt1_1775_l65 + (1.96*SEAGB_alt1_1775_l65)
LowCI_alt1_1775_l65 = AGB_MgC_ha_alt1_1775_l65 - (1.96*SEAGB_alt1_1775_l65)

AGBPlot_alt1_1775_l65 <-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_alt1_1775_l65,
                                  HighCI = HighCI_alt1_1775_l65,
                                  LowCI = LowCI_alt1_1775_l65,
                                  Project_ID = rep("VCS1775"),
                                  EquationType = rep("Chamshama (2004) [4]"))
AGBPlot_alt1_1775_l65

AGBPlot_alt1_1775_f_l65 <- AGBPlot_alt1_1775_l65 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_alt1_1775_f_l65

p1775_alt1_l65 <- data.frame(AGBPlot_alt1_1775_f_l65 %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1775"),
                         EquationType = rep("Chamshama (2004) [4]"))
p1775_alt1_l65

# VCS1775 alternative function 2####

#equation from Ryan et al 2011
# AGC = exp (2.601 * log(dbh) - 3.629))
VCS1775_AGB_alt_function2 <- function(D) { # already calculate in carbon figures
  AGB = exp (2.601 * log(D) - 3.629) * exp((0.52^2)/2)
  print(AGB)
}
AGBPlot_alt2_1775 <- by(wd_1775_higher10, wd_1775_higher10$plotID,
                   function(x) VCS1775_AGB_alt_function2(D = x$D),
                   simplify = F)
AGBplot_Alt2_1775 <- sapply(AGBPlot_alt2_1775, sum)
AGBplot_Alt2_1775 # kg per 1 ha

AGBplot_Alt2_1775_c <- sapply(AGBPlot_alt2_1775, length)
AGBplot_Alt2_1775_c

# Error 
AGB_MgC_ha_alt2_1775 = (AGBplot_Alt2_1775/1000) #* 0.456 removing as equation provide carbon figures already
SDAGB_alt2_1775 = AGB_MgC_ha_alt2_1775 * 0.20
SEAGB_alt2_1775 = SDAGB_alt2_1775/sqrt(AGBplot_Alt2_1775_c)
SEAGB_alt2_1775
HighCI_alt2_1775 = AGB_MgC_ha_alt2_1775 + (1.96*SEAGB_alt2_1775)
LowCI_alt2_1775 = AGB_MgC_ha_alt2_1775 - (1.96*SEAGB_alt2_1775)

AGBPlot_alt2_1775<-  data.frame (PlotID = unique(levels(wd_1775_higher10$plotID)),
                               AGB_MgC_ha = AGB_MgC_ha_alt2_1775,
                               HighCI = HighCI_alt2_1775,
                               LowCI = LowCI_alt2_1775,
                               Project_ID = rep("VCS1775"),
                               EquationType = rep("Ryan (2011) [4]"))
AGBPlot_alt2_1775

AGBPlot_alt2_1775_f <- AGBPlot_alt2_1775 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_alt2_1775_f

p1775_alt2 <- data.frame(AGBPlot_alt2_1775_f %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1775"),
                         EquationType = rep("Ryan (2011) [4]"))
p1775_alt2

  # Alt 2 DBH <= 65 cm

VCS1775_AGB_alt_function2 <- function(D) { # already calculate in carbon figures
  AGB = exp (2.601 * log(D) - 3.629) * exp((0.52^2)/2)
  print(AGB)
}
AGBPlot_alt2_1775_l65 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                        function(x) VCS1775_AGB_alt_function2(D = x$D),
                        simplify = F)
AGBplot_Alt2_1775_l65 <- sapply(AGBPlot_alt2_1775_l65, sum)

AGBplot_Alt2_1775_c_l65 <- sapply(AGBPlot_alt2_1775_l65, length)

# Error 
AGB_MgC_ha_alt2_1775_l65 = (AGBplot_Alt2_1775_l65/1000) #* 0.456 removing as equation provide carbon figures already
SDAGB_alt2_1775_l65 = AGB_MgC_ha_alt2_1775_l65 * 0.20
SEAGB_alt2_1775_l65 = SDAGB_alt2_1775_l65/sqrt(AGBplot_Alt2_1775_c_l65)
HighCI_alt2_1775_l65 = AGB_MgC_ha_alt2_1775_l65 + (1.96*SEAGB_alt2_1775_l65)
LowCI_alt2_1775_l65 = AGB_MgC_ha_alt2_1775_l65 - (1.96*SEAGB_alt2_1775_l65)

AGBPlot_alt2_1775_l65<-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt2_1775_l65,
                                 HighCI = HighCI_alt2_1775_l65,
                                 LowCI = LowCI_alt2_1775_l65,
                                 Project_ID = rep("VCS1775"),
                                 EquationType = rep("Ryan (2011) [4]"))
AGBPlot_alt2_1775_l65

AGBPlot_alt2_1775_f_l65 <- AGBPlot_alt2_1775_l65 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_alt2_1775_f_l65

p1775_alt2_l65 <- data.frame(AGBPlot_alt2_1775_f_l65 %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1775"),
                         EquationType = rep("Ryan (2011) [4]"))
p1775_alt2_l65

## VCS 1775 alt function3.1 - equation fit by Tomo for Miombo at stand-level
 # ABGTotal = 0.056 × D^2549 R2= 93.7

VCS1775_AGB_alt_function3.1 <- function(D) { # create a function with the name my_function
  AGB = 0.056 * D^2.549 * exp((0.52^2)/2)
  print(AGB) # this returns AGB in kg
} 
AGBPlot_alt3.1_1775 <- by(wd_1775_higher10, wd_1775_higher10$plotID,
                        function(x) VCS1775_AGB_alt_function3.1(D = x$D),
                        simplify = F)
AGBplot_Alt3.1_1775 <- sapply(AGBPlot_alt3.1_1775, sum)
AGBplot_Alt3.1_1775 # kg per 1 ha

AGBplot_Alt3.1_1775_c <- sapply(AGBPlot_alt3.1_1775, length)
AGBplot_Alt3.1_1775_c

# Error 
AGB_MgC_ha_alt3.1_1775 = (AGBplot_Alt3.1_1775/1000) * 0.456
SDAGB_alt3.1_1775 = AGB_MgC_ha_alt3.1_1775 * 0.20
SEAGB_alt3.1_1775 = SDAGB_alt3.1_1775/sqrt(AGBplot_Alt3.1_1775_c)
SEAGB_alt3.1_1775
HighCI_alt3.1_1775 = AGB_MgC_ha_alt3.1_1775 + (1.96*SEAGB_alt3.1_1775)
LowCI_alt3.1_1775 = AGB_MgC_ha_alt3.1_1775 - (1.96*SEAGB_alt3.1_1775)

AGBPlot_alt3.1_1775<-  data.frame (PlotID = unique(levels(wd_1775_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt3.1_1775,
                                 HighCI = HighCI_alt3.1_1775,
                                 LowCI = LowCI_alt3.1_1775,
                                 Project_ID = rep("VCS1775"),
                                 EquationType = rep("Tomo (2012) [4]"))
AGBPlot_alt3.1_1775

AGBPlot_alt3.1_1775_f <- AGBPlot_alt3.1_1775 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_alt3.1_1775_f

p1775_alt3.1 <- data.frame(AGBPlot_alt3.1_1775_f %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1775"),
                         EquationType = rep("Tomo (2012) [4]"))
p1775_alt3.1

  # Alt 3 DBH <= 65 cm

VCS1775_AGB_alt_function3.1 <- function(D) { # create a function with the name my_function
  AGB = 0.056 * D^2.549 * exp((0.52^2)/2)
  print(AGB) # this returns AGB in kg
} 
AGBPlot_alt3.1_1775_l65 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                          function(x) VCS1775_AGB_alt_function3.1(D = x$D),
                          simplify = F)
AGBplot_Alt3.1_1775_l65 <- sapply(AGBPlot_alt3.1_1775_l65, sum)

AGBplot_Alt3.1_1775_c_l65 <- sapply(AGBPlot_alt3.1_1775_l65, length)

# Error 
AGB_MgC_ha_alt3.1_1775_l65 = (AGBplot_Alt3.1_1775_l65/1000) * 0.456
SDAGB_alt3.1_1775_l65 = AGB_MgC_ha_alt3.1_1775_l65 * 0.20
SEAGB_alt3.1_1775_l65 = SDAGB_alt3.1_1775_l65/sqrt(AGBplot_Alt3.1_1775_c_l65)
HighCI_alt3.1_1775_l65 = AGB_MgC_ha_alt3.1_1775_l65 + (1.96*SEAGB_alt3.1_1775_l65)
LowCI_alt3.1_1775_l65 = AGB_MgC_ha_alt3.1_1775_l65 - (1.96*SEAGB_alt3.1_1775_l65)

AGBPlot_alt3.1_1775_l65<-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                                   AGB_MgC_ha = AGB_MgC_ha_alt3.1_1775_l65,
                                   HighCI = HighCI_alt3.1_1775_l65,
                                   LowCI = LowCI_alt3.1_1775_l65,
                                   Project_ID = rep("VCS1775"),
                                   EquationType = rep("Tomo (2012) [4]"))
AGBPlot_alt3.1_1775_l65

AGBPlot_alt3.1_1775_f_l65 <- AGBPlot_alt3.1_1775_l65 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_alt3.1_1775_f_l65

p1775_alt3.1_l65 <- data.frame(AGBPlot_alt3.1_1775_f_l65 %>%
                             summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                       se = sd(AGB_MgC_ha)/sqrt(n()),
                                       HCI = mAGB_MgC_ha + (1.96*se), 
                                       LCI = mAGB_MgC_ha - (1.96*se),
                                       N = n()), 
                           Project_ID = rep("VCS1775"),
                           EquationType = rep("Tomo (2012) [4]"))
p1775_alt3.1_l65

# VCS1775 - alt function 4 ####
# Mugasha et al 2013 model form 2 General 

VCS1775_AGB_alt4_function <- function(D) { # create a function with the name my_function
  AGB = (0.1027 * D^2.4798) * exp((0.411^2)/2)
  print(AGB) # AGB in Mg
}
AGBPlot_alt4_1775 <- by(wd_1775_higher10, wd_1775_higher10$plotID,
                   function(x) VCS1775_AGB_alt4_function(D = x$D),
                   simplify = F)
AGBplot_Alt4_1775 <- sapply(AGBPlot_alt4_1775, sum)
AGBplot_Alt4_1775 # kg per 1 ha

AGBplot_Alt4_1775_c <- sapply(AGBPlot_alt4_1775, length)
AGBplot_Alt4_1775_c

# Error 
AGB_MgC_ha_alt4_1775 = (AGBplot_Alt4_1775/1000) * 0.456
AGB_MgC_ha_alt4_1775
SDAGB_alt4_1775 = AGB_MgC_ha_alt4_1775 * 0.20
SEAGB_alt4_1775 = SDAGB_alt4_1775/sqrt(AGBplot_Alt4_1775_c)
SEAGB_alt4_1775
HighCI_alt4_1775 = AGB_MgC_ha_alt4_1775 + (1.96*SEAGB_alt4_1775)
LowCI_alt4_1775 = AGB_MgC_ha_alt4_1775 - (1.96*SEAGB_alt4_1775)

AGBPlot_alt4_1775<-  data.frame (PlotID = unique(levels(wd_1775_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt4_1775,
                                 HighCI = HighCI_alt4_1775,
                                 LowCI = LowCI_alt4_1775,
                                 Project_ID = rep("VCS1775"),
                                 EquationType = rep("Mugasha (2013) [4]")) #extra
AGBPlot_alt4_1775

AGBPlot_alt4_1775_f <- AGBPlot_alt4_1775 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_alt4_1775_f

p1775_alt4 <- data.frame(AGBPlot_alt4_1775_f %>%
                             summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                       se = sd(AGB_MgC_ha)/sqrt(n()),
                                       HCI = mAGB_MgC_ha + (1.96*se), 
                                       LCI = mAGB_MgC_ha - (1.96*se),
                                       N = n()), 
                           Project_ID = rep("VCS1775"),
                           EquationType = rep("Mugasha (2013) [4]"))
p1775_alt4

  # Alt 4 DBH <= 65 cm

VCS1775_AGB_alt4_function <- function(D) { # create a function with the name my_function
  AGB = (0.1027 * D^2.4798) * exp((0.411^2)/2)
  print(AGB) # AGB in Mg
}
AGBPlot_alt4_1775_l65 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                        function(x) VCS1775_AGB_alt4_function(D = x$D),
                        simplify = F)
AGBplot_Alt4_1775_l65 <- sapply(AGBPlot_alt4_1775_l65, sum)

AGBplot_Alt4_1775_c_l65 <- sapply(AGBPlot_alt4_1775_l65, length)

# Error 
AGB_MgC_ha_alt4_1775_l65 = (AGBplot_Alt4_1775_l65/1000) * 0.456
SDAGB_alt4_1775_l65 = AGB_MgC_ha_alt4_1775_l65 * 0.20
SEAGB_alt4_1775_l65 = SDAGB_alt4_1775_l65/sqrt(AGBplot_Alt4_1775_c_l65)
HighCI_alt4_1775_l65 = AGB_MgC_ha_alt4_1775_l65 + (1.96*SEAGB_alt4_1775_l65)
LowCI_alt4_1775_l65 = AGB_MgC_ha_alt4_1775_l65 - (1.96*SEAGB_alt4_1775_l65)

AGBPlot_alt4_1775_l65<-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt4_1775_l65,
                                 HighCI = HighCI_alt4_1775_l65,
                                 LowCI = LowCI_alt4_1775_l65,
                                 Project_ID = rep("VCS1775"),
                                 EquationType = rep("Mugasha (2013) [4]"))

AGBPlot_alt4_1775_f_l65 <- AGBPlot_alt4_1775_l65 %>% filter(AGB_MgC_ha > 0) 

p1775_alt4_l65 <- data.frame(AGBPlot_alt4_1775_f_l65 %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1775"),
                         EquationType = rep("Mugasha (2013) [4]"))
p1775_alt4_l65

# Alt 5 VCS1775 ####
#AGB = exp(2.5553 * log(D) - 2.5265) #Chidumayo 2013 [4]

VCS1775_AGB_alt5_function <- function(D) { # create a function with the name my_function
  AGB = exp(2.5553 * log(D) - 2.5265) * exp((0.41^2)/2)
  print(AGB) # AGB in Mg
}
AGBPlot_alt5_1775 <- by(wd_1775_higher10, wd_1775_higher10$plotID,
                        function(x) VCS1775_AGB_alt5_function(D = x$D),
                        simplify = F)
AGBplot_Alt5_1775 <- sapply(AGBPlot_alt5_1775, sum)
AGBplot_Alt5_1775 # kg per 1 ha

AGBplot_Alt5_1775_c <- sapply(AGBPlot_alt5_1775, length)
AGBplot_Alt5_1775_c

# Error 
AGB_MgC_ha_alt5_1775 = (AGBplot_Alt5_1775/1000) * 0.47
AGB_MgC_ha_alt5_1775
SDAGB_alt5_1775 = AGB_MgC_ha_alt5_1775 * 0.20
SEAGB_alt5_1775 = SDAGB_alt5_1775/sqrt(AGBplot_Alt5_1775_c)
SEAGB_alt5_1775
HighCI_alt5_1775 = AGB_MgC_ha_alt5_1775 + (1.96*SEAGB_alt5_1775)
LowCI_alt5_1775 = AGB_MgC_ha_alt5_1775 - (1.96*SEAGB_alt5_1775)

AGBPlot_alt5_1775<-  data.frame (PlotID = unique(levels(wd_1775_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt4_1775,
                                 HighCI = HighCI_alt5_1775,
                                 LowCI = LowCI_alt5_1775,
                                 Project_ID = rep("VCS1775"),
                                 EquationType = rep("Chidumayo (2013) [4]")) #extra
AGBPlot_alt5_1775

AGBPlot_alt5_1775_f <- AGBPlot_alt5_1775 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_alt5_1775_f

p1775_alt5 <- data.frame(AGBPlot_alt5_1775_f %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1775"),
                         EquationType = rep("Chidumayo (2013) [4]"))
p1775_alt5

 # Alt 5 DBH <= 65 cm

VCS1775_AGB_alt5_function <- function(D) { # create a function with the name my_function
  AGB = exp(2.5553 * log(D) - 2.5265) * exp((0.41^2)/2)
  print(AGB) # AGB in Mg
}
AGBPlot_alt5_1775_l65 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                        function(x) VCS1775_AGB_alt5_function(D = x$D),
                        simplify = F)
AGBplot_Alt5_1775_l65 <- sapply(AGBPlot_alt5_1775_l65, sum)
AGBplot_Alt5_1775 # kg per 1 ha

AGBplot_Alt5_1775_c_l65 <- sapply(AGBPlot_alt5_1775_l65, length)
AGBplot_Alt5_1775_c

# Error 
AGB_MgC_ha_alt5_1775_l65 = (AGBplot_Alt5_1775_l65/1000) * 0.47
SDAGB_alt5_1775_l65 = AGB_MgC_ha_alt5_1775_l65 * 0.20
SEAGB_alt5_1775_l65 = SDAGB_alt5_1775_l65/sqrt(AGBplot_Alt5_1775_c_l65)
HighCI_alt5_1775_l65 = AGB_MgC_ha_alt5_1775_l65 + (1.96*SEAGB_alt5_1775_l65)
LowCI_alt5_1775_l65 = AGB_MgC_ha_alt5_1775_l65 - (1.96*SEAGB_alt5_1775_l65)

AGBPlot_alt5_1775_l65 <-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt4_1775_l65,
                                 HighCI = HighCI_alt5_1775_l65,
                                 LowCI = LowCI_alt5_1775_l65,
                                 Project_ID = rep("VCS1775"),
                                 EquationType = rep("Chidumayo (2013) [4]")) #extra
AGBPlot_alt5_1775_l65

AGBPlot_alt5_1775_f_l65<- AGBPlot_alt5_1775_l65 %>% filter(AGB_MgC_ha > 0) 
AGBPlot_alt5_1775_f

p1775_alt5_l65 <- data.frame(AGBPlot_alt5_1775_f_l65 %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1775"),
                         EquationType = rep("Chidumayo (2013) [4]"))
p1775_alt5_l65

# Other Miombo equations
# Mate et al 2014 both below:
# AGB = (1.0030 - 1.0567 * WD) * D^2.0390, R2= 0.95, RMSE = 93.06
# AGB = 0.2201 * D^2.1574
# other equations Abbot, P., Lowore, J. and Werren, M. 1997. Models for the estimation of single tree volume in four Miombo woodland types. Forest Ecology and Management 97: 25-37.

## 1775 Plotting ####

# combining in data frame####
AGBPlot_proj_lo65_1775

VCS1775_AGB_Data2 <- rbind(p1775_er,
                          p1775_r5,
                          p1775_alt1,
                          p1775_alt2,
                          p1775_alt3.1,
                          p1775_alt4,
                          p1775_alt5)
VCS1775_AGB_Data2

#Calcularing tCO2e/ha
vcs1775_toplot2 <-VCS1775_AGB_Data2
vcs1775_toplot2 
vcs1775_toplot2$AGC_tCO2_ha = as.numeric(vcs1775_toplot2$mAGB_MgC_ha * (44/12))
vcs1775_toplot2$HCI_tCO2_ha = as.numeric(vcs1775_toplot2$HCI * (44/12))
vcs1775_toplot2$LCI_tCO2_ha = as.numeric(vcs1775_toplot2$LCI * (44/12))
vcs1775_toplot2$EquationType = as.factor(vcs1775_toplot2$EquationType)

## Plot  VCS1775 #######

p1775_2 <- ggplot(vcs1775_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha, decreasing=FALSE), 
                            y = AGC_tCO2_ha, 
                            #ymin = LCI_tCO2_ha, 
                            #ymax = HCI_tCO2_ha, 
                            color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,115)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS1775 Zambia | VM0009", 
       subtitle = "8 1-ha woodland plots")+
  theme_bw()+
  #coord_flip()+#scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0009] VCS1775 Zambia")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p1775_2

#b1775<- list(geom_hline(yintercept = 57.43, color = '#003f5c',alpha=0.4),
 #             annotate("rect", ymin = 48.88, ymax = 65.98,xmin = -Inf, xmax = Inf,alpha=0.1,linetype=2))

#p_1775_final <- p_1775 + geom_hline(aes(yintercept=633.86), lty=2, color = "gray", cex=1.2, alpha = 0.6) + b1775
#p_1775_final

# 1775 Data Stats ####

# without project, full range of DBH

VCS1775_AGB_Data3 <- rbind(p1775_er,
                           p1775_r5,
                           p1775_alt1,
                           p1775_alt2,
                           p1775_alt3.1,
                           p1775_alt4,
                           p1775_alt5)
VCS1775_AGB_Data3

#Calcularing tCO2e/ha
vcs1775_toplot3 <-VCS1775_AGB_Data3

vcs1775_toplot3$AGC_tCO2_ha = as.numeric(vcs1775_toplot3$mAGB_MgC_ha * (44/12))
vcs1775_toplot3$HCI_tCO2_ha = as.numeric(vcs1775_toplot3$HCI * (44/12))
vcs1775_toplot3$LCI_tCO2_ha = as.numeric(vcs1775_toplot3$LCI * (44/12))
vcs1775_toplot3$EquationType = as.factor(vcs1775_toplot3$EquationType)

# with project, DBH <= 65 cm

VCS1775_AGB_Data4 <- rbind(p1775_er_l65,
                           p1775_pr,
                           p1775_r5_l65,
                           p1775_alt1_l65,
                           p1775_alt2_l65,
                           p1775_alt3.1_l65,
                           p1775_alt4_l65,
                           p1775_alt5_l65)
VCS1775_AGB_Data4

#Calcularing tCO2e/ha
vcs1775_toplot4 <-VCS1775_AGB_Data4

vcs1775_toplot4$AGC_tCO2_ha = as.numeric(vcs1775_toplot4$mAGB_MgC_ha * (44/12))
vcs1775_toplot4$HCI_tCO2_ha = as.numeric(vcs1775_toplot4$HCI * (44/12))
vcs1775_toplot4$LCI_tCO2_ha = as.numeric(vcs1775_toplot4$LCI * (44/12))
vcs1775_toplot4$EquationType = as.factor(vcs1775_toplot4$EquationType)

# Project VCS902 (Zimbabwe) ####
# log(AGB): a * log(D) + b in Ryan et al 2011
 #log(AGB) = 2.601 * log(D) - 3.629

# by Chidumayo (2013) Wood Biomass (kg) = 0.0446 * DBH^2.765
#by Mugasha et al 2013 Wood Biomass (kg) = 0.1027 * DBH^2.4798

VCS902_AGB_Ryan2011_function <- function(D) {
  AGB = exp(2.601 * log(D) - 3.629) * exp((0.52^2)/2)#
  print(AGB) # this returns AGB in kg
}
AGBPlot_Proj_902 <- by(wd_902_higher10, wd_902_higher10$plotID,
                            function(x) VCS902_AGB_Ryan2011_function(D = x$D),
                            simplify = F)
AGBPlot_Proj_902
AGBplot_ProjD_902 <- sapply(AGBPlot_Proj_902, sum)
AGBplot_ProjD_902

AGBplot_ProjD_902_c <- sapply(AGBPlot_Proj_902, length)
AGBplot_ProjD_902_c

# Error 

AGB_MgC_ha_proj_902 = (AGBplot_ProjD_902/1000) * 0.47 # carbon fraction as measured in Ryan et al 2011
AGB_MgC_ha_proj_902
SDAGB_proj_902 = AGB_MgC_ha_proj_902 * 0.20
SEAGB_proj_902 = SDAGB_proj_902/sqrt(AGBplot_ProjD_902_c)
HighCI_proj_902 = AGB_MgC_ha_proj_902 + (1.96*SEAGB_proj_902)
LowCI_proj_902 = AGB_MgC_ha_proj_902 - (1.96*SEAGB_proj_902)

AGBPlot_proj_902 <-  data.frame (PlotID = unique(levels(wd_902_higher10$plotID)),
                                       AGB_MgC_ha = AGB_MgC_ha_proj_902,
                                       HighCI = HighCI_proj_902,
                                       LowCI = LowCI_proj_902,
                                       Project_ID = rep("VCS902"),
                                       EquationType = rep("Ryan (2011) [4]"))
AGBPlot_proj_902 

AGBPlot_proj_902_f <- AGBPlot_proj_902 %>% filter(AGB_MgC_ha > 0)
AGBPlot_proj_902_f

p902_pr <- data.frame(AGBPlot_proj_902_f %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                  se = sd(AGB_MgC_ha)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS902"),
                      EquationType = rep("Ryan (2011) [4]"))
p902_pr

## Alternative equation 1 for VCS902 ####
# AGB = -0.089 + 0.000634 *BA (in cm2) in Henry et al. 2011, where BA = 0.78539 * D²

VCS902_AGB_alt_function1 <- function(D) { # create a function with the name my_function
  AGB = -0.089 + 0.000634 * (0.78539 * D^2) * exp((0.5^2)/2)
  print(AGB)
}
AGBPlot_alt1_902 <- by(wd_902_higher10, wd_902_higher10$plotID,
                        function(x) VCS902_AGB_alt_function1(D = x$D),
                        simplify = F)
AGBplot_Alt1_902 <- sapply(AGBPlot_alt1_902, sum)
AGBplot_Alt1_902 # kg per 1 ha

AGBplot_Alt1_902_c <- sapply(AGBPlot_alt1_902, length)
AGBplot_Alt1_902_c

# Error

AGB_MgC_ha_alt1_902 = (AGBplot_Alt1_902) * 0.47
SDAGB_alt1_902 = AGB_MgC_ha_alt1_902 * 0.20
SEAGB_alt1_902 = SDAGB_alt1_902/sqrt(AGBplot_Alt1_902_c)
HighCI_alt1_902 = AGB_MgC_ha_alt1_902 + (1.96*SEAGB_alt1_902)
LowCI_alt1_902 = AGB_MgC_ha_alt1_902 - (1.96*SEAGB_alt1_902)

AGBPlot_alt1_902 <-  data.frame (PlotID = unique(levels(wd_902_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_alt1_902,
                                  HighCI = HighCI_alt1_902,
                                  LowCI = LowCI_alt1_902,
                                  Project_ID = rep("VCS902"),
                                  EquationType = rep("Henry (2011)-eq1 [4]"))
AGBPlot_alt1_902

AGBPlot_alt1_902_f <- AGBPlot_alt1_902 %>% filter(AGB_MgC_ha > 0)
AGBPlot_alt1_902_f

p902_alt1 <- data.frame(AGBPlot_alt1_902_f %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                  se = sd(AGB_MgC_ha)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS902"),
                      EquationType = rep("Henry (2011)-eq1 [4]"))
p902_alt1

# VCS902 alternative function 2####

#equation from Chidumayo et al 1997 for Zambia B = 20.02 *D - 203.37

VCS902_AGB_alt_function2 <- function(D) { 
  AGB = 20.02 * D - 203.37 * exp((0.5^2)/2)
  print(AGB)
}
AGBPlot_alt2_902 <- by(wd_902_higher10, wd_902_higher10$plotID,
                        function(x) VCS902_AGB_alt_function2(D = x$D),
                        simplify = F)
AGBplot_Alt2_902 <- sapply(AGBPlot_alt2_902, sum)
AGBplot_Alt2_902 # kg per 1 ha

AGBplot_Alt2_902_c <- sapply(AGBPlot_alt2_902, length)
AGBplot_Alt2_902_c

# Error 

AGB_MgC_ha_alt2_902 = (AGBplot_Alt2_902/1000) * 0.47 
SDAGB_alt2_902 = AGB_MgC_ha_alt2_902 * 0.10
SEAGB_alt2_902 = SDAGB_alt2_902/sqrt(AGBplot_Alt2_902_c)
HighCI_alt2_902 = AGB_MgC_ha_alt2_902 + (1.96*SEAGB_alt2_902)
LowCI_alt2_902 = AGB_MgC_ha_alt2_902 - (1.96*SEAGB_alt2_902)

AGBPlot_alt2_902<-  data.frame (PlotID = unique(levels(wd_902_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt2_902,
                                 HighCI = HighCI_alt2_902,
                                 LowCI = LowCI_alt2_902,
                                 Project_ID = rep("VCS902"),
                                 EquationType = rep("Chidumayo (1997) [4]"))
AGBPlot_alt2_902

AGBPlot_alt2_902_f <- AGBPlot_alt2_902 %>% filter(AGB_MgC_ha > 0)
AGBPlot_alt2_902_f

p902_alt2 <- data.frame(AGBPlot_alt2_902_f %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                    se = sd(AGB_MgC_ha)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS902"),
                        EquationType = rep("Chidumayo (1997) [4]"))
p902_alt2

## VCS 902 alt function3 ####
 # Y= -41.077 + 2.816554 * D + 0.35657 *(D^2) - tropical dry forest in Mozambique Henry et al 2011

VCS902_AGB_alt_function3 <- function(D) { # create a function with the name my_function
  AGB = -41.077 + 2.816554 * D + 0.35657 * (D^2) * exp((0.5^2)/2)
  print(AGB) # this returns AGB in kg
} 
AGBPlot_alt3_902 <- by(wd_902_higher10, wd_902_higher10$plotID,
                        function(x) VCS902_AGB_alt_function3(D = x$D),
                        simplify = F)
AGBplot_Alt3_902 <- sapply(AGBPlot_alt3_902, sum)
AGBplot_Alt3_902 # kg per 1 ha

AGBplot_Alt3_902_c <- sapply(AGBPlot_alt3_902, length)
AGBplot_Alt3_902_c

# Error 
AGB_MgC_ha_alt3_902 = (AGBplot_Alt3_902/1000) * 0.456
SDAGB_alt3_902 = AGB_MgC_ha_alt3_902 * 0.20
SEAGB_alt3_902 = SDAGB_alt3_902/sqrt(AGBplot_Alt3_902_c)
HighCI_alt3_902 = AGB_MgC_ha_alt3_902 + (1.96*SEAGB_alt3_902)
LowCI_alt3_902 = AGB_MgC_ha_alt3_902 - (1.96*SEAGB_alt3_902)

AGBPlot_alt3_902<-  data.frame (PlotID = unique(levels(wd_902_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt3_902,
                                 HighCI = HighCI_alt3_902,
                                 LowCI = LowCI_alt3_902,
                                 Project_ID = rep("VCS902"),
                                 EquationType = rep("Henry (2011) [4]"))
AGBPlot_alt3_902

AGBPlot_alt3_902_f <- AGBPlot_alt3_902 %>% filter(AGB_MgC_ha > 0)
AGBPlot_alt3_902_f

p902_alt3 <- data.frame(AGBPlot_alt3_902_f %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                    se = sd(AGB_MgC_ha)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS902"),
                        EquationType = rep("Henry (2011) [4]"))
p902_alt3

# VCS902 - alt function 4 #
# Guy 1981 fit in Zimbabwe  Tobal biomass of a tree = 0.0549 * (D)^2.5101

VCS902_AGB_alt_function4 <- function(D) { 
  AGB = 0.0549 * (D)^2.5101 * exp((0.5^2)/2) 
  print(AGB) # AGB in Mg
}
AGBPlot_alt4_902 <- by(wd_902_higher10, wd_902_higher10$plotID,
                        function(x) VCS902_AGB_alt_function4(D = x$D),
                        simplify = F)
AGBplot_Alt4_902 <- sapply(AGBPlot_alt4_902, sum)
AGBplot_Alt4_902 # kg per 1 ha

AGBplot_Alt4_902_c <- sapply(AGBPlot_alt4_902, length)
AGBplot_Alt4_902_c

# Error 
AGB_MgC_ha_alt4_902 = (AGBplot_Alt4_902/1000) * 0.47
SDAGB_alt4_902 = AGB_MgC_ha_alt4_902 * 0.20
SEAGB_alt4_902 = SDAGB_alt4_902/sqrt(AGBplot_Alt4_902_c)
HighCI_alt4_902 = AGB_MgC_ha_alt4_902 + (1.96*SEAGB_alt4_902)
LowCI_alt4_902 = AGB_MgC_ha_alt4_902 - (1.96*SEAGB_alt4_902)

AGBPlot_alt4_902<-  data.frame (PlotID = unique(levels(wd_902_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt4_902,
                                 HighCI = HighCI_alt4_902,
                                 LowCI = LowCI_alt4_902,
                                 Project_ID = rep("VCS902"),
                                 EquationType = rep("Guy (1981) [4]"))
AGBPlot_alt4_902

AGBPlot_alt4_902_f <- AGBPlot_alt4_902 %>% filter(AGB_MgC_ha > 0)
AGBPlot_alt4_902_f

p902_alt4 <- data.frame(AGBPlot_alt4_902_f %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                    se = sd(AGB_MgC_ha)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS902"),
                        EquationType = rep("Guy (1981) [4]"))
p902_alt4

# Brown et al dry tropics B = 34.47 - 8.067*D + 0.659*D^2

## 902 Plotting ####

# combining in data frame#

VCS902_AGB_Data2 <- rbind(p902_pr, 
                         p902_er,
                         p902_r5,
                         p902_alt1,
                         p902_alt2,
                         p902_alt3,
                         p902_alt4)
VCS902_AGB_Data2

vcs902_toplot2 <-VCS902_AGB_Data2 #%>% filter(AGB_MgC_ha > 0)
#vcs902_toplot$LowCI = as.numeric(vcs902_toplot$LowCI)
#vcs902_toplot$HighCI = as.numeric(vcs902_toplot$HighCI)
#vcs902_toplot$Project_ID = as.factor(vcs902_toplot$Project_ID)

vcs902_toplot2

#Calcularing tCO2e/ha
vcs902_toplot2$EquationType = as.factor(vcs902_toplot2$EquationType)
vcs902_toplot2$AGC_tCO2_ha = as.numeric(vcs902_toplot2$mAGB_MgC_ha * (44/12))
vcs902_toplot2$HCI_tCO2_ha = as.numeric(vcs902_toplot2$HCI * (44/12))
vcs902_toplot2$LCI_tCO2_ha = as.numeric(vcs902_toplot2$LCI * (44/12))

vcs902_toplot2
## Plot  VCS902 #######

p902_2 <- ggplot(vcs902_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha, decreasing=FALSE), 
                                     y = AGC_tCO2_ha, 
                                     #ymin = LCI_tCO2_ha, 
                                     #ymax = HCI_tCO2_ha, 
                                     color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,50)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS902 Zimbabwe | VM0009", 
       subtitle = "10 1-ha woodland plots")+
  theme_pubr()+
  #coord_flip()+#scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0009] VCS902 Zimbabwe")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p902_2

# Project VCS934 (DRC) ####
# Equation used  AGB = exp(−1.602 +(2.266 ln(DBH))+(0.136 ln(DBH)^2)+(−0.0206 ln(DBH)^3)+(0.0206 ln(WD)) # Chave et al (2005) general model II.2 (All types)

VCS934_AGB_function <- function(D, WD) { # create a function with the name my_function
  AGB = exp(-1.602 + (2.266 * log(D)) + (0.136 * (log(D)^2)) + (-0.0206 * (log(D)^3)) + (0.0206 * log(WD)))
  print(AGB) # this returns AGB in kg
}
AGBPlot_Proj_934 <- by(md_934_higher10, md_934_higher10$plotID,
                       function(x) VCS934_AGB_function(D = x$D, WD = x$WD),
                       simplify = F)
AGBplot_ProjD_WD_934 <- sapply(AGBPlot_Proj_934, sum)
AGBplot_ProjD_WD_934 # AGB in kg

AGBplot_ProjD_WD_934_c <- sapply(AGBPlot_Proj_934, length)

# 10% error

AGB_MgC_ha_proj934 = (AGBplot_ProjD_WD_934/1000) * 0.50 # carbon fraction used as the project did not specify and methodology is vague
SDAGB_proj_934 = AGB_MgC_ha_proj934 * 0.20
SEAGB_proj_934 = SDAGB_proj_934/sqrt(AGBplot_ProjD_WD_934_c)
HighCI_proj_934 = AGB_MgC_ha_proj934 + (1.96*SEAGB_proj_934)
LowCI_proj_934 = AGB_MgC_ha_proj934 - (1.96*SEAGB_proj_934)

AGBPlot_proj_934 <-  data.frame (PlotID = unique(levels(md_934_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_proj934,
                                 HighCI = HighCI_proj_934,
                                 LowCI = LowCI_proj_934,
                                 Project_ID = rep("VCS934"),
                                 EquationType = rep("Project [6]"))
AGBPlot_proj_934

p934_pr <- data.frame(AGBPlot_proj_934 %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                  se = sd(AGB_MgC_ha)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS934"),
                      EquationType = rep("Project [6]"))
p934_pr

## Alt equation 1 934 ####

# Djomo et al 2010 eq 
VCS934_AGB_alt_function1 <- function(D,WD) { 
  AGB = exp(-1.9644 + (2.3382 * log(D)) + (0.3579 * log(WD))) * exp((0.325^2)/2)
  print(AGB)
}
AGBPlot_alt1_v1_934 <- by(md_934_higher10, md_934_higher10$plotID,
                          function(x) VCS934_AGB_alt_function1(D = x$D, WD = x$WD),
                          simplify = F)
AGBplot_alt1_934 <- sapply(AGBPlot_alt1_v1_934, sum)
AGBplot_alt1_934 # kg per 1 ha

AGBplot_alt1_934_c <- sapply(AGBPlot_alt1_v1_934, length)
AGBplot_alt1_934_c

# 20% error
AGB_MgC_ha_alt1_934 = (AGBplot_alt1_934/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_alt1_934
SDAGB_alt1_934 = AGB_MgC_ha_alt1_934 * 0.20
SEAGB_alt1_934 = SDAGB_alt1_934/sqrt(AGBplot_alt1_934_c)
HighCI_alt1_934 = AGB_MgC_ha_alt1_934 + (1.96*SEAGB_alt1_934)
LowCI_alt1_934 = AGB_MgC_ha_alt1_934 - (1.96*SEAGB_alt1_934)

AGBPlot_alt1_934 <-  data.frame (PlotID  = unique(levels(md_934_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt1_934,
                                 HighCI = HighCI_alt1_934,
                                 LowCI = LowCI_alt1_934,
                                 Project_ID = rep("VCS934"),
                                 EquationType = rep("Djomo (2010)-2 [4]"))
AGBPlot_alt1_934

p934_alt1 <- data.frame(AGBPlot_alt1_934 %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                  se = sd(AGB_MgC_ha)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS934"),
                      EquationType = rep("Djomo (2010)-2 [4]"))
p934_alt1

## Alt equation 2 934 ####

# Djomo et al 2010
VCS934_AGB_alt_function2 <- function(D) { # create a function with the name my_function
  AGB = exp(-2.2057 + 2.5841*log(D)) * exp((0.483^2)/2) #Djomo et al 2010
  print(AGB)
}
AGBPlot_alt2_v1_934 <- by(md_934_higher10, md_934_higher10$plotID,
                          function(x) VCS934_AGB_alt_function2(D = x$D),
                          simplify = F)
AGBplot_alt2_934 <- sapply(AGBPlot_alt2_v1_934, sum)
AGBplot_alt2_934 # kg per 1 ha

AGBplot_alt2_934_c <- sapply(AGBPlot_alt2_v1_934, length)
AGBplot_alt2_934_c

# 20% error
AGB_MgC_ha_alt2_934 = (AGBplot_alt2_934/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_alt2_934
SDAGB_alt2_934 = AGB_MgC_ha_alt2_934 * 0.20
SEAGB_alt2_934 = SDAGB_alt2_934/sqrt(AGBplot_alt2_934_c)
HighCI_alt2_934 = AGB_MgC_ha_alt2_934 + (1.96*SEAGB_alt2_934)
LowCI_alt2_934 = AGB_MgC_ha_alt2_934 - (1.96*SEAGB_alt2_934)

AGBPlot_alt2_934 <-  data.frame (PlotID  = unique(levels(md_934_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt2_934,
                                 HighCI = HighCI_alt2_934,
                                 LowCI = LowCI_alt2_934,
                                 Project_ID = rep("VCS934"),
                                 EquationType = rep("Djomo (2010)-1 [4]"))
AGBPlot_alt2_934

p934_alt2 <- data.frame(AGBPlot_alt2_934 %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                    se = sd(AGB_MgC_ha)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS934"),
                        EquationType = rep("Djomo (2010)-1 [4]"))
p934_alt2

## Alt equation 3 934 ####
# logY= 0.000328+(2.163×log(X)) Henry et al 2011 eq 420
#Y= 0.11068–1.7905×X+13.08245×(X^2) Henry et al 2011 eq 400
#log10Y= –4.20+2.69×log10(X) Henry et al 2011 eq 394
VCS934_AGB_alt_function3 <- function(D) { # Djomo et al 2010 v3
  AGB = 10^(-4.2 + 2.69 * log10(D)) * exp((0.483^2)/2)
  print(AGB)
}
AGBPlot_alt3_v1_934 <- by(md_934_higher10, md_934_higher10$plotID,
                          function(x) VCS934_AGB_alt_function3(D = x$D),
                          simplify = F)
AGBplot_alt3_934 <- sapply(AGBPlot_alt3_v1_934, sum)
AGBplot_alt3_934 # kg per 1 ha

AGBplot_alt3_934_c <- sapply(AGBPlot_alt3_v1_934, length)
AGBplot_alt3_934_c

# 10% error

AGB_MgC_ha_alt3_934 = (AGBplot_alt3_934) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_alt3_934
SDAGB_alt3_934 = AGB_MgC_ha_alt3_934 * 0.20
SEAGB_alt3_934 = SDAGB_alt3_934/sqrt(AGBplot_alt3_934_c)
HighCI_alt3_934 = AGB_MgC_ha_alt3_934 + (1.96*SEAGB_alt3_934)
LowCI_alt3_934 = AGB_MgC_ha_alt3_934 - (1.96*SEAGB_alt3_934)

AGBPlot_alt3_934 <-  data.frame (PlotID  = unique(levels(md_934_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt3_934,
                                 HighCI = HighCI_alt3_934,
                                 LowCI = LowCI_alt3_934,
                                 Project_ID = rep("VCS934"),
                                 EquationType = rep("Henry (2011)-eq394 [4]"))
AGBPlot_alt3_934

p934_alt3 <- data.frame(AGBPlot_alt3_934 %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                    se = sd(AGB_MgC_ha)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS934"),
                        EquationType = rep("Henry (2011)-eq394 [4]"))
p934_alt3

## Alt equation 4 934####

VCS934_AGB_alt_function4 <- function(WD,D) { # Djomo et al 2010 v3
  AGB = WD * exp(-1.499 + 2.148 * log(D) + 0.207 * log(D)^2 - 0.0281 * log(D)^3) * exp((0.483^2)/2)
  print(AGB)
}
AGBPlot_alt4_v1_934 <- by(md_934_higher10, md_934_higher10$plotID,
                          function(x) VCS934_AGB_alt_function4(WD = x$WD, D = x$D),
                          simplify = F)
AGBplot_alt4_934 <- sapply(AGBPlot_alt4_v1_934, sum)
AGBplot_alt4_934 # kg per 1 ha

AGBplot_alt4_934_c <- sapply(AGBPlot_alt4_v1_934, length)
AGBplot_alt4_934_c

# 20% error

AGB_MgC_ha_alt4_934 = (AGBplot_alt4_934/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_alt4_934
SDAGB_alt4_934 = AGB_MgC_ha_alt4_934 * 0.20
SEAGB_alt4_934 = SDAGB_alt4_934/sqrt(AGBplot_alt4_934_c)
HighCI_alt4_934 = AGB_MgC_ha_alt4_934 + (1.96*SEAGB_alt4_934)
LowCI_alt4_934 = AGB_MgC_ha_alt4_934 - (1.96*SEAGB_alt4_934)

AGBPlot_alt4_934 <-  data.frame (PlotID  = unique(levels(md_934_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt4_934,
                                 HighCI = HighCI_alt4_934,
                                LowCI = LowCI_alt4_934,
                                 Project_ID = rep("VCS934"),
                                 EquationType = rep("Djomo (2010)-4 [4]")) 
AGBPlot_alt4_934

p934_alt4 <- data.frame(AGBPlot_alt4_934 %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                    se = sd(AGB_MgC_ha)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS934"),
                        EquationType = rep("Djomo (2010)-4 [4]"))
p934_alt4

## 934 plotting ####

VCS934_AGB_Data2 <- rbind(p934_er,
                         p934_r5,
                         p934_alt1,
                         p934_alt2,
                         p934_alt3,
                         p934_alt4)
VCS934_AGB_Data2 

vcs934_toplot2 <-VCS934_AGB_Data2 #%>% filter(PlotID<5)

# Calculating tCO2e/ha
vcs934_toplot2$EquationType = as.factor(vcs934_toplot2$EquationType)
vcs934_toplot2$AGC_tCO2_ha = as.numeric(vcs934_toplot2$mAGB_MgC_ha * (44/12))
vcs934_toplot2$HCI_tCO2_ha = as.numeric(vcs934_toplot2$HCI * (44/12))
vcs934_toplot2$LCI_tCO2_ha = as.numeric(vcs934_toplot2$LCI * (44/12))
vcs934_toplot2

p934_2 <- ggplot(vcs934_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha, decreasing=FALSE), 
                                     y = AGC_tCO2_ha, 
                                     #ymin = LCI_tCO2_ha, 
                                     #ymax = HCI_tCO2_ha, 
                                     color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,250)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS934 DRC | VM0009", 
       subtitle = "5 1-ha dry forest plots")+
  theme_pubr()+
  #coord_flip()+#scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0009] VCS934 DRC")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p934_2

# 934 Data Stats ####

VCS934_AGB_Data3 <- rbind(p934_er,
                          p934_r5,
                          p934_alt1,
                          p934_alt2,
                          p934_alt3,
                          p934_alt4,
                          p934_pr)
VCS934_AGB_Data3 

vcs934_toplot3 <-VCS934_AGB_Data3 #%>% filter(PlotID<5)

# Calculating tCO2e/ha
vcs934_toplot3$EquationType = as.factor(vcs934_toplot3$EquationType)
vcs934_toplot3$AGC_tCO2_ha = as.numeric(vcs934_toplot3$mAGB_MgC_ha * (44/12))
vcs934_toplot3$HCI_tCO2_ha = as.numeric(vcs934_toplot3$HCI * (44/12))
vcs934_toplot3$LCI_tCO2_ha = as.numeric(vcs934_toplot3$LCI * (44/12))
vcs934_toplot3

# Plot VM0009 ####
#p_VM9 <- p_934+p_1775+p_902+plot_layout(ncol=3)

#p_VM9 <- (p_902_final|p_934_final)+p_1775_final+plot_layout(widths = c(2,1), ncol=2)
#p_VM9

p_VM9 <- (p902_2|p934_2)+p1775_2+plot_layout(widths = c(1,1,1), ncol=3)
p_VM9

ggsave(filename = "FigVM0009_v5.png",
       plot = p_VM9, width = 18, height = 8, units = 'cm',
       scale = 2, dpi = 800)

p_fig2_pre2 <- p_VM6 /p_VM7 /p_VM9 + plot_layout(ncol=1)  #+ p_VM9 + p_VM15 +plot_layout(ncol=1)
p_fig2_pre2

ggsave(filename = "Fig2_SA_vm6_7_9_v2.png",
       plot = p_fig2_pre, width = 20, height = 16, units = 'cm',
       scale = 2, dpi = 800)

## END ####

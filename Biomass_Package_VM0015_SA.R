
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
library(patchwork)
library(ggpubr)

#To cite the package
citation("BIOMASS")

## Uploading Brazil Forest Inventory data ####

## VCS 1094 data ####
ANA_A01_2015_2018_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/ANA_A01_2015_2018_Inventory.csv")
br_ana01 <- data.frame(ANA_A01_2015_2018_Inventory)# ANA_A01_2015_2018 from Pará
str(br_ana01) # 3967 individuals of  11 variables - total of 32 0.25-ha plots in a total of 8 ha total area

br_ana01_ed <- br_ana01 %>% filter(scientific.name!="NA") %>% separate(scientific.name, c("genus", "species"))
str(br_ana01_ed) # 3679 individuals

br_ana01_ed$plotID <- as.factor(ifelse(br_ana01_ed$plot<5, '1',
                             ifelse(br_ana01_ed$plot == 5, '2',
                                    ifelse(br_ana01_ed$plot>5 & br_ana01_ed$plot<9, '2',
                                           ifelse(br_ana01_ed$plot>8 & br_ana01_ed$plot<13, '3', 
                                                  ifelse(br_ana01_ed$plot>12 & br_ana01_ed$plot<17, '4',
                                                         ifelse(br_ana01_ed$plot>16 & br_ana01_ed$plot<21, '5',
                                                                ifelse(br_ana01_ed$plot>20 & br_ana01_ed$plot<25, '6',
                                                                       ifelse(br_ana01_ed$plot>24 & br_ana01_ed$plot<29, '7','8')))))))))

# print data frame
str(br_ana01_ed$plotID)
names(br_ana01_ed)

names(br_ana01_ed)[7] <- 'family'
names(br_ana01_ed)[9] <- 'D'

br_ana01_ed$type
br_ana01_ed_f <- br_ana01_ed %>% filter(Dead.2015!= "TRUE") %>% filter(type == "O")
str(br_ana01_ed_f) # 3508 OBST

md_brana01_1094 <- data.frame(br_ana01_ed_f) #renaming to keep original data frame as uploaded
str(md_brana01_1094)

## VCS 1541 data ####

load("~/Dropbox/R/Allometric_Equations/Allometric_Equations/data:/doi_10.15146_5xcp-0d46__v2 (1)/bci.tree/bci.tree1.rdata")
BCI_data <- data.frame(bci.tree1)
load("~/Dropbox/R/Allometric_Equations/Allometric_Equations/data:/doi_10.15146_5xcp-0d46__v2 (1)/bci.spptable.rdata")
BCI_sp_data <- data.frame(bci.spptable)
names(BCI_sp_data)
names(BCI_data) # 423617 individuals of  16 variables 

BCI_data_full <- BCI_data %>% inner_join(BCI_sp_data, 
                              by=c('sp'='sp'))
str(BCI_data_full)

BCI_data_full$quadrat2 = as.factor(BCI_data_full$quadrat) #- total of 1251 0.04-ha plots in a total of 50 ha total area
unique(levels(BCI_data_full$quadrat2))

BCI_data_ed <- BCI_data_full %>% drop_na(dbh)
str(BCI_data_ed)

BCI_data_ed <- BCI_data_ed %>%
  mutate(plotID = fct_collapse(quadrat2,
                               "1" = c("0000" ,"0001", "0002", "0003","0004", "0005" ,"0006" ,"0007" ,"0008", "0009",
                                       "0010", "0011", "0012", "0013","0014", "0015", "0016" ,"0017", "0018", "0019",
                                       "0020" ,"0021", "0022", "0023", "0024"),
                               "2" = c("0104", "0105", "0106" ,"0107" ,"0108", "0109", "0110", "0111" ,"0112", "0113",
                                       "0114", "0115", "0116", "0117", "0118","0119" ,"0120", "0121", "0122", "0123",
                                       "0124", "0200", "0201","0202", "0203"),
                               "3" = c("0100", "0101", "0102", "0103","0204", "0205", "0206" ,"0207", "0208","0209",
                                       "0210" ,"0211", "0212" ,"0213", "0214" ,"0215", "0216", "0217", "0218", "0219" ,
                                       "0220", "0221", "0222", "0223","0224"),
                               "4" = c("0300", "0301", "0302", "0303", "0304", "0305" ,"0306", "0307", "0308" ,"0309" ,
                                       "0310", "0311" ,"0312" ,"0313","0314" ,"0315", "0316", "0317" ,"0318" ,"0319" ,
                                       "0320" ,"0321","0322", "0323", "0324"),
                               "5" = c("0324", "0400", "0401" ,"0402", "0403","0404" ,"0405" ,"0406", "0407", "0408" ,
                                       "0409","0410" ,"0411", "0412", "0413", "0414" ,"0415", "0416", "0417", "0418", 
                                       "0419","0420","0421", "0422","0423"),
                               "6" = c("0424", "0500","0501", "0502" ,"0503" ,"0504", "0505" ,"0506", "0507", "0508",
                                       "0509", "0510", "0511", "0512", "0513", "0514", "0515", "0516", "0517", "0518",
                                       "0519", "0520" ,"0521" ,"0522", "0523"),
                               "7" = c("0524", "0600", "0601" ,"0602", "0603" ,"0604", "0605", "0606", "0607" ,"0608",
                                       "0609", "0610", "0611" ,"0612", "0613","0614", "0615" ,"0616", "0617", "0618",
                                       "0619", "0620", "0621", "0622" ,"0623"),
                               "8" = c("0704", "0705", "0706", "0707", "0708", "0709", "0710", "0711", "0712", "0713",
                                       "0714", "0715", "0716", "0717", "0718","0719", "0720", "0721", "0722" ,"0723",
                                       "0724", "0800" ,"0801", "0802", "0803"),
                               "9" = c("2804", "2805", "2806", "2807", "2808", "2809", "2810", "2811" ,"2812" ,"2813",
                                       "2814" ,"2815", "2816", "2817", "2818","2819", "2820", "2821", "2822", "2823",
                                       "2824", "2900", "2901" ,"2902", "2903"),
                               "10" = c("3524", "3600", "3601", "3602", "3603", "3604" ,"3605", "3606", "3607", "3608",
                                        "3609", "3610", "3611", "3612", "3613","3614", "3615", "3616", "3617", "3618",
                                        "3619", "3620", "3621" ,"3622" ,"3623")))
str(BCI_data_ed)
unique(levels(BCI_data_ed$plotID))

vcs1541 <- data.frame(BCI_data_ed) #renaming to keep original data frame as uploaded
str(vcs1541) # 235338 obs
vcs1541$status = as.factor(vcs1541$status)
vcs1541$status

# Data wrangling to adjust species names
vcs1541_filt <- vcs1541 %>% filter(status == "A")#%>% separate(scientific.name, c("genus", "species"))
names(vcs1541_filt) #235338
summary(vcs1541_filt)
vcs1541_filt$Species

vcs1541_filt$D = vcs1541_filt$dbh / 10
vcs1541_filt$D

names(vcs1541_filt)[18] <- 'genus'
names(vcs1541_filt)[19] <- 'species'
names(vcs1541_filt)[20] <- 'family'

vcs1541_filt %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n(), agb_sum = sum(agb))

vcs1541_filt$plotID = as.numeric(vcs1541_filt$plotID)
summary(vcs1541_filt)

new_vcs1541_filt <- subset(vcs1541_filt, plotID < 11)
str(new_vcs1541_filt)
new_vcs1541_filt$plotID = as.factor(new_vcs1541_filt$plotID)
unique(levels(new_vcs1541_filt$plotID))

new_vcs1541_filt %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n(), agb_sum = sum(agb))

## VCS944 data####

# VCS 944 Data - moist forests in Brazil
ANA_A01_2015_2018_Inventory <- read_csv("data:/Forest_Inventory_Brazil_2007/data/ANA_A01_2015_2018_Inventory.csv")
br_ana01 <- data.frame(ANA_A01_2015_2018_Inventory)# upload ANA_BR_FI_2015 from Pará
str(br_ana01) #3839 individuals of  11 variables - total of 32 0.25-ha plots in a total of 8 ha total area

br_ana01$plotID <- as.factor(ifelse(br_ana01$plot<5, '1',
                                    ifelse(br_ana01$plot == 5, '2',
                                           ifelse(br_ana01$plot>5 & br_ana01$plot<9, '2',
                                                  ifelse(br_ana01$plot>8 & br_ana01$plot<13, '3', 
                                                         ifelse(br_ana01$plot>12 & br_ana01$plot<17, '4',
                                                                ifelse(br_ana01$plot>16 & br_ana01$plot<21, '5',
                                                                       ifelse(br_ana01$plot>20 & br_ana01$plot<25, '6',
                                                                              ifelse(br_ana01$plot>24 & br_ana01$plot<29, '7','8')))))))))

# print data frame
str(br_ana01$plotID)

md_brana01_944 <- data.frame(br_ana01) #renaming to keep original data frame as uploaded
str(md_brana01_944) # 3967 obs
md_brana01_944$Dead.2015

md_brana01_944 <- md_brana01_944 %>% separate(scientific.name, c("genus", "species"))
names(md_brana01_944) #235338

names(md_brana01_944)[7] <- 'family'
names(md_brana01_944)[9] <- 'D'

md_brana01_944_f <- md_brana01_944 %>% filter(Dead.2015== "FALSE") %>% filter(type == "O")
str(md_brana01_944_f) #3605

md_brana01_944_f %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n())

## Moist deciduous forests in Para, Brazil to VCS1094 Ecomapua####

## Inspecting and Manipulating plot data####

# Filter out NA's in D column
md_brana01_1094_filt <- md_brana01_944_f %>% drop_na(D)
str(md_brana01_1094_filt) # 3605 individuals in 8 ha
summary(md_brana01_1094_filt)

md_and01_1541_filt <- new_vcs1541_filt %>% drop_na(D)
str(md_and01_1541_filt) # 34161 individuals in 8 ha
summary(md_and01_1541_filt) # max D =142.8

md_and01_1541_filt %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n()) # 9 plots

md_944_filt <- md_brana01_944_f %>% drop_na(D)
str(md_944_filt) # 3635 individuals in 8 ha
summary(md_944_filt)

# Filter out dbh sizes lower than 10 cm####
md_brana01_1094_higher10 <- md_brana01_1094_filt %>% filter (D > 9.999)
str(md_brana01_1094_higher10) #3605 trees
summary(md_brana01_1094_higher10) #Max DBH = 142.80 cm

md_and01_1541_higher10 <- md_and01_1541_filt %>% filter (D > 9.999 & D < 150)
str(md_and01_1541_higher10) # 3232 trees in 9 ha
summary(md_and01_1541_higher10) #Max DBH = 134 cm

md_944_higher10 <- md_944_filt %>% filter (D > 9.999)
str(md_944_higher10) # 3635 trees
summary(md_944_higher10) # Max D = 142.80

# inspecting data by plot
md_944_higher10 %>%
  group_by(plotID) %>%
  summarise(mean = mean(D), n = n()) #8 1-ha plots

# Checking and correcting taxonomy####
length(md_brana01_1094_higher10$genus)
length(md_brana01_1094_higher10$species)

Taxomd_1094<-correctTaxo(genus = md_brana01_1094_higher10$genus, 
                  species = md_brana01_1094_higher10$species,
                  score = 0.5,
                  useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
md_brana01_1094_higher10$genusCorr <- Taxomd_1094$genusCorrected
md_brana01_1094_higher10$speciesCorr <- Taxomd_1094$speciesCorrected

Taxomd_1541 <- correctTaxo(genus = md_and01_1541_higher10$genus, 
                    species = md_and01_1541_higher10$species,
                    score = 0.5,
                    useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
md_and01_1541_higher10$genusCorr <- Taxomd_1541$genusCorrected
md_and01_1541_higher10$speciesCorr <- Taxomd_1541$speciesCorrected

Taxomd_944<-correctTaxo(genus = md_944_higher10$genus, 
                        species = md_944_higher10$species,
                        score = 0.5,
                        useCache = TRUE) # score of the matching Score of the matching (see http://tnrs.iplantcollaborative.org/instructions.html#match).
md_944_higher10$genusCorr <- Taxomd_944$genusCorrected
md_944_higher10$speciesCorr <- Taxomd_944$speciesCorrected

# Retrieving APG III Families and Orders from Genus names####
APGmd_1094 <- getTaxonomy(md_brana01_1094_higher10$genusCorr, findOrder = T)
md_brana01_1094_higher10$familyAPG <- APGmd_1094$family
md_brana01_1094_higher10$orderAPG <- APGmd_1094$order

APGmd_1541 <- getTaxonomy(md_and01_1541_higher10$genusCorr, findOrder = T)
md_and01_1541_higher10$familyAPG <- APGmd_1541$family
md_and01_1541_higher10$orderAPG <- APGmd_1541$order

APGmd_944 <- getTaxonomy(md_944_higher10$genusCorr, findOrder = T)
md_944_higher10$familyAPG <- APGmd_944$family
md_944_higher10$orderAPG <- APGmd_944$order

# Retrieving wood density data####

#The WD can either be attributed to an individual at a species, 
#genus, family or stand level.

# Compute the Wood Density up to the genus level 
# and then give the mean wood density per stand (or not)

WDdatamd_1094<-getWoodDensity(genus = Taxomd_1094$genusCorrected, 
                       species = Taxomd_1094$speciesCorrected, 
                       stand = md_brana01_1094_higher10$plotID,
                       region = "SouthAmericaTrop")
# adding WD column to plot dataframe
md_brana01_1094_higher10$WD <- WDdatamd_1094$meanWD
md_brana01_1094_higher10$sdWD <- WDdatamd_1094$sdWD

WDdatamd_1541 <- getWoodDensity(genus = Taxomd_1541$genusCorrected, 
                         species = Taxomd_1541$speciesCorrected, 
                         stand = md_and01_1541_higher10$plotID,
                         region = "World") #changed to world instead of central america as the DRYAD data only stored 420 wood density values in this region of interest
# adding WD column to plot dataframe
md_and01_1541_higher10$WD <- WDdatamd_1541$meanWD
md_and01_1541_higher10$sdWD <- WDdatamd_1541$sdWD

WDdatamd_944 <- getWoodDensity(genus = Taxomd_944$genusCorrected, 
                             species = Taxomd_944$speciesCorrected, 
                             stand = md_944_higher10$plotID,
                             region = "SouthAmericaTrop")
# adding WD column to plot dataframe
md_944_higher10$WD <- WDdatamd_944$meanWD
md_944_higher10$sdWD <- WDdatamd_944$sdWD

# Tree height data####

# Retrieve height data from a Feldpaush et al. (2012) averaged model
dataHfeld_1094 <- retrieveH(D = md_brana01_1094_higher10$D, region = "SAmerica")
md_brana01_1094_higher10$Hfeld <- dataHfeld_1094$H
md_brana01_1094_higher10$HRSE <- dataHfeld_1094$RSE
str(md_brana01_1094_higher10)

dataHfeld_1541 <- retrieveH(D = md_and01_1541_higher10$D, region = "SAmerica") #as there is no Central America option
md_and01_1541_higher10$Hfeld <- dataHfeld_1541$H
md_and01_1541_higher10$HRSE <- dataHfeld_1541$RSE

dataHfeld_944 <- retrieveH(D = md_944_higher10$D, region = "SAmerica")
md_944_higher10$Hfeld <- dataHfeld_944$H
md_944_higher10$HRSE <- dataHfeld_944$RSE

# Retrieve height data from Chave et al. (2012) equation 6

  #lat and long from data source
long <- -54.99
lat <- -3.36
coord_ana01 <- c(long, lat)
coord_ana01
md_brana01_1094_higher10$lat <- rep(-3.36)
md_brana01_1094_higher10$long <- rep(-54.99)

dataHchave_1094 <- retrieveH(D = md_brana01_1094_higher10$D,
                        coord = coord_ana01)#, #cbind(md_brana01_higher10$lat, md_brana01_higher10$long))#,
                        #region = "SAmerica")
md_brana01_1094_higher10$Hchave<- dataHchave_1094$H

md_and01_1541_higher10$lat <- rep(9.152096)
md_and01_1541_higher10$long <- rep(-79.854984)
long_1541 <- -79.854984
lat_1541 <- 9.152096
coord_1541 <- c(long_1541, lat_1541)
coord_1541

dataHchave_1541 <- retrieveH(D = md_and01_1541_higher10$D,
                            coord = cbind(md_and01_1541_higher10$long, md_and01_1541_higher10$lat))#, #cbind(md_brana01_higher10$lat, md_brana01_higher10$long))#,
#region = "SAmerica")
md_and01_1541_higher10$Hchave<- dataHchave_1541$H

long_944 <- -54.99
lat_944 <- -3.36
coord_944<- c(md_944_higher10$long, md_944_higher10$lat)
coord_944
md_944_higher10$long <- rep(-54.99)
md_944_higher10$lat <- rep(-3.36)

dataHchave_944 <- retrieveH(D = md_944_higher10$D,
                            coord = cbind(md_944_higher10$long, md_944_higher10$lat))#, #cbind(md_brana01_higher10$lat, md_brana01_higher10$long))#,
#region = "SAmerica")
dataHchave_944
md_944_higher10$Hchave<- dataHchave_944$H

####
# Once diameter, wood density and height values ##
# have been retrieved for each tree ##
###

## Compute Chave 2014 AGB ####

# 1094
AGBPlot_ChHf_1094 <- by(md_brana01_1094_higher10, md_brana01_1094_higher10$plotID,
              function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
               simplify = F)
AGBplot_D_H_WD_1094 <- sapply(AGBPlot_ChHf_1094, sum)
AGBplot_D_H_WD_1094 # Mg per 1 ha

AGBplot_D_H_WD_1094_c <- sapply(AGBPlot_ChHf_1094, length)
AGBplot_D_H_WD_1094_c

# Error 
AGB_MgC_ha_ChHf_1094 = (AGBplot_D_H_WD_1094) * 0.456 # carbon fraction by Martin et al 2018
SDAGB_ChHf_1094 = AGB_MgC_ha_ChHf_1094 * 0.20
SEAGB_ChHf_1094 = SDAGB_ChHf_1094/sqrt(AGBplot_D_H_WD_1094_c)
HighCI_ChHf_1094 = AGB_MgC_ha_ChHf_1094 + (1.96*SEAGB_ChHf_1094)
LowCI_ChHf_1094 = AGB_MgC_ha_ChHf_1094 - (1.96*SEAGB_ChHf_1094)

AGBPlot_ChHf_1094 <-  data.frame (PlotID = unique(levels(md_brana01_1094_higher10$plotID)),
                                AGB_MgC_ha = AGB_MgC_ha_ChHf_1094,
                                HighCI = HighCI_ChHf_1094,
                                LowCI = LowCI_ChHf_1094,
                                Project_ID = rep("VCS1094"),
                                EquationType = rep("Chave (2014) [6]"))
AGBPlot_ChHf_1094 

p1094_er <- data.frame(AGBPlot_ChHf_1094 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1094"),
                       EquationType = rep("Chave (2014) [6]"))
p1094_er

# 1541

AGBPlot_ChHf_1541 <- by(md_and01_1541_higher10, md_and01_1541_higher10$plotID,
                   function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                   simplify = F)
AGBplot_D_H_WD_1541 <- sapply(AGBPlot_ChHf_1541, sum)
AGBplot_D_H_WD_1541 # Mg per 1 ha

AGBplot_D_H_WD_1541_c <- sapply(AGBPlot_ChHf_1541, length)
AGBplot_D_H_WD_1541_c

# Adding CIs to data frame

AGB_MgC_ha_ChHf_1541 = (AGBplot_D_H_WD_1541) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_ChHf_1541
SDAGB_ChHf_1541 = AGB_MgC_ha_ChHf_1541 * 0.20
SEAGB_ChHf_1541 = SDAGB_ChHf_1541/sqrt(AGBplot_D_H_WD_1541_c)
HighCI_ChHf_1541 = AGB_MgC_ha_ChHf_1541 + (1.96*SEAGB_ChHf_1541)
LowCI_ChHf_1541 = AGB_MgC_ha_ChHf_1541 - (1.96*SEAGB_ChHf_1541)

AGBPlot_ChHf_1541 <-  data.frame (PlotID = unique(levels(md_and01_1541_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_ChHf_1541,
                                  HighCI = HighCI_ChHf_1541,
                                  LowCI = LowCI_ChHf_1541,
                                  Project_ID = rep("VCS1541"),
                                  EquationType = rep("Chave (2014) [6]"))
AGBPlot_ChHf_1541 

md_and01_1541_higher10 %>%
  group_by(plotID) %>%
  summarise(sum = sum(agb), n = n())

AGBPlot_ChHf_1541_f <- AGBPlot_ChHf_1541 %>% filter(AGB_MgC_ha > 80) 
AGBPlot_ChHf_1541_f

p1541_er <- data.frame(AGBPlot_ChHf_1541_f %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1541"),
                       EquationType = rep("Chave (2014) [6]"))
p1541_er

# 944
str(md_944_higher10)

AGBPlot_ChHf_944 <- by(md_944_higher10, md_944_higher10$plotID,
                   function(x) computeAGB(D = x$D, WD = x$WD, H = x$Hfeld),
                   simplify = F)
AGBplot_D_H_WD_944 <- sapply(AGBPlot_ChHf_944, sum)
AGBplot_D_H_WD_944 

AGBplot_D_H_WD_944_c <- sapply(AGBplot_D_H_WD_944, length)
AGBplot_D_H_WD_944_c

# 20% error
AGB_MgC_ha_ChHf_944 = (AGBplot_D_H_WD_944) * 0.456 # carbon fraction by Martin et al 2018
SDAGB_ChHf_944 = AGB_MgC_ha_ChHf_944 * 0.20
SEAGB_ChHf_944 = SDAGB_ChHf_944/sqrt(AGBplot_D_H_WD_944_c)
HighCI_ChHf_944 = AGB_MgC_ha_ChHf_944 + (1.96*SEAGB_ChHf_944)
LowCI_ChHf_944 = AGB_MgC_ha_ChHf_944 - (1.96*SEAGB_ChHf_944)

AGBPlot_ChHf_944 <-  data.frame (PlotID = unique(levels(md_944_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_ChHf_944,
                                  HighCI = HighCI_ChHf_944,
                                  LowCI = LowCI_ChHf_944,
                                  Project_ID = rep("VCS944"),
                                  EquationType = rep("Chave (2014) [6]"))
AGBPlot_ChHf_944

p944_er <- data.frame(AGBPlot_ChHf_944 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS944"),
                       EquationType = rep("Chave (2014) [6]"))
p944_er

# Chave 2005 forest type specific ####
# AGB = 0.0509 * (WD * D^2 * H)) Chave 2005 type moist forest

VCS1094_AGB_function_r5 <- function(WD, D, H) { # create a function with the name my_function
  AGB = 0.0509 * (WD * D^2 * H)  #* exp((0.459^2)/2) correction factor already embedded in equation
  print(AGB)
}
AGBPlot_r5_1094 <- by(md_brana01_1094_higher10, md_brana01_1094_higher10$plotID,
                      function (x) VCS1094_AGB_function_r5(WD = x$WD, D = x$D,H = x$Hfeld),
                      simplify = F)
AGBPlot_r5_1094
AGBplot_1094_r5 <- sapply(AGBPlot_r5_1094, sum)
AGBplot_1094_r5

AGBplot_1094_r5_c <- sapply(AGBPlot_r5_1094, length)
AGBplot_1094_r5_c

# Error
AGB_MgC_ha_r5_1094 = (AGBplot_1094_r5/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_r5_1094
SDAGB_r5_1094 = AGB_MgC_ha_r5_1094 * 0.20
SEAGB_r5_1094 = SDAGB_r5_1094/sqrt(AGBplot_1094_r5_c)
HighCI_r5_1094 = AGB_MgC_ha_r5_1094 + (1.96*SEAGB_r5_1094)
LowCI_r5_1094 = AGB_MgC_ha_r5_1094 - (1.96*SEAGB_r5_1094)

AGBPlot_r5_1094 <-  data.frame (PlotID = unique(levels(md_brana01_1094_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_r5_1094,
                                  HighCI = HighCI_r5_1094,
                                  LowCI = LowCI_r5_1094,
                                  Project_ID = rep("VCS1094"),
                                  EquationType = rep("Chave (2005) [5]"))
AGBPlot_r5_1094 

p1094_r5 <- data.frame(AGBPlot_r5_1094 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1094"),
                       EquationType = rep("Chave (2005) [5]"))
p1094_r5

# 1541
# AGB = 0.0509 * (WD * D^2 * H)) Chave 2005 type moist forest

VCS1541_AGB_function_r5 <- function(WD, D, H) { # create a function with the name my_function
  AGB = 0.0509 * (WD * D^2 * H)  #* exp((0.459^2)/2) correction factor already embedded in equation
  print(AGB)
}
AGBPlot_r5_1541 <- by(md_and01_1541_higher10, md_and01_1541_higher10$plotID,
                      function (x) VCS1541_AGB_function_r5(WD = x$WD, D = x$D,H = x$Hfeld),
                      simplify = F)
AGBPlot_r5_1541
AGBplot_1541_r5 <- sapply(AGBPlot_r5_1541, sum)
AGBplot_1541_r5

AGBplot_1541_r5_c <- sapply(AGBPlot_r5_1541, length)
AGBplot_1541_r5_c

# Adding CIs to data frame

AGB_MgC_ha_r5_1541 = (AGBplot_1541_r5/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_r5_1541
SDAGB_r5_1541 = AGB_MgC_ha_r5_1541 * 0.20
SEAGB_r5_1541 = SDAGB_r5_1541/sqrt(AGBplot_1541_r5_c)
HighCI_r5_1541 = AGB_MgC_ha_r5_1541 + (1.96*SEAGB_r5_1541)
LowCI_r5_1541 = AGB_MgC_ha_r5_1541 - (1.96*SEAGB_r5_1541)

AGBPlot_r5_1541 <-  data.frame (PlotID = unique(levels(md_and01_1541_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_r5_1541,
                                 HighCI = HighCI_r5_1541,
                                 LowCI = LowCI_r5_1541,
                                 Project_ID = rep("VCS1541"),
                                 EquationType = rep("Chave (2005) [5]"))
AGBPlot_r5_1541 

AGBPlot_r5_1541_f <- AGBPlot_r5_1541 %>% filter(AGB_MgC_ha > 80) 
AGBPlot_r5_1541_f

p1541_r5 <- data.frame(AGBPlot_r5_1541_f %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1541"),
                       EquationType = rep("Chave (2005) [5]"))
p1541_r5

# 944

# AGB = 0.0509 * (WD * D^2 * H)) Chave 2005 type moist forest
VCS944_AGB_function_r5 <- function(WD, D, H) { # create a function with the name my_function
  AGB = 0.0509 * (WD * D^2 * H)  #* exp((0.459^2)/2) correction factor already embedded in equation
  print(AGB)
}
AGBPlot_r5_944 <- by(md_944_higher10, md_944_higher10$plotID,
                      function (x) VCS944_AGB_function_r5(WD = x$WD, D = x$D,H = x$Hfeld),
                      simplify = F)
AGBPlot_r5_944
AGBplot_944_r5 <- sapply(AGBPlot_r5_944, sum)
AGBplot_944_r5

AGBplot_944_r5_c <- sapply(AGBPlot_r5_944, length)
AGBplot_944_r5_c

# Error
AGB_MgC_ha_r5_944 = (AGBplot_944_r5/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_r5_944
SDAGB_r5_944 = AGB_MgC_ha_r5_944 * 0.20
SEAGB_r5_944 = SDAGB_r5_944/sqrt(AGBplot_944_r5_c)
HighCI_r5_944 = AGB_MgC_ha_r5_944 + (1.96*SEAGB_r5_944)
LowCI_r5_944 = AGB_MgC_ha_r5_944 - (1.96*SEAGB_r5_944)

AGBPlot_r5_944 <-  data.frame (PlotID = unique(levels(md_944_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_r5_944,
                                 HighCI = HighCI_r5_944,
                                 LowCI = LowCI_r5_944,
                                 Project_ID = rep("VCS944"),
                                 EquationType = rep("Chave (2005) [5]"))
AGBPlot_r5_944

p944_r5 <- data.frame(AGBPlot_r5_944 %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                  se = sd(AGB_MgC_ha)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS944"),
                      EquationType = rep("Chave (2005) [5]"))
p944_r5

# Project VCS1094 (Para, Brazil) ####
# Equation used AGB = WD/0.67 *{exp[0.33(lnDBH) + 0.933(lnDBH)^2) - 0.122 (lnDBH)^3) - 0.37]}

VCS1094_AGB_function <- function(D, WD) { # create a function with the name my_function
  AGB = WD/0.67 * (exp(0.33 * log(D) + 0.933 * (log(D)^2) - 0.122 * (log(D)^3) - 0.37)) * exp((0.4^2)/2)
  print(AGB) # this returns AGB in kg
}
AGBPlot_Proj_1094 <- by(md_brana01_1094_higher10, md_brana01_1094_higher10$plotID,
                  function(x) VCS1094_AGB_function(D = x$D, WD = x$WD),
                  simplify = F)
AGBplot_ProjD_WD_1094 <- sapply(AGBPlot_Proj_1094, sum)
AGBplot_ProjD_WD_1094 # Mg per 1 ha

AGBplot_ProjD_WD_1094_c <- sapply(AGBPlot_Proj_1094, length)

# Error

AGB_MgC_ha_proj_1094 = (AGBplot_ProjD_WD_1094/1000) * 0.50 # carbon fraction used by the project
SDAGB_proj_1094 = AGB_MgC_ha_proj_1094 * 0.20
SEAGB_proj_1094 = SDAGB_proj_1094/sqrt(AGBplot_ProjD_WD_1094_c)
HighCI_proj_1094 = AGB_MgC_ha_proj_1094 + (1.96*SEAGB_proj_1094)
LowCI_proj_1094 = AGB_MgC_ha_proj_1094 - (1.96*SEAGB_proj_1094)

AGBPlot_proj_1094 <-  data.frame (PlotID = unique(levels(md_brana01_1094_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_proj_1094,
                                  HighCI = HighCI_proj_1094,
                                  LowCI = LowCI_proj_1094,
                                  Project_ID = rep("VCS1094"),
                                  EquationType = rep("Project [4]"))
AGBPlot_proj_1094 

p1094_pr <- data.frame(AGBPlot_proj_1094 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1094"),
                       EquationType = rep("Project [4]"))
p1094_pr

## Alternative equation 1 VCS1094 ####
## Brown et al 1997 moist forest equation Y = exp{-2.134+2.530*ln(D)}

VCS1094_AGB_alt1_function <- function(D) { #extra
  AGB = exp(-2.134 + 2.530 * log(D)) * exp((0.4^2)/2)
  print(AGB)
}
AGBPlot_alt1_1094 <- by(md_brana01_1094_higher10, md_brana01_1094_higher10$plotID,
                   function(x) VCS1094_AGB_alt1_function(D = x$D),
                   simplify = F)
AGBplot_Alt1_1094 <- sapply(AGBPlot_alt1_1094, sum)
AGBplot_Alt1_1094 # kg per 1 ha

AGBplot_Alt1_1094_c <- sapply(AGBPlot_alt1_1094, length)
AGBplot_Alt1_1094_c

# Error
AGB_MgC_ha_alt1_1094 = (AGBplot_Alt1_1094/1000) * 0.456
SDAGB_alt1_1094 = AGB_MgC_ha_alt1_1094 * 0.20
SEAGB_alt1_1094 = SDAGB_alt1_1094/sqrt(AGBplot_Alt1_1094_c)
HighCI_alt1_1094 = AGB_MgC_ha_alt1_1094 + (1.96*SEAGB_alt1_1094)
LowCI_alt1_1094 = AGB_MgC_ha_alt1_1094 - (1.96*SEAGB_alt1_1094)

MD_AGBPlot_alt1_1094<-  data.frame (PlotID = unique(levels(md_brana01_1094_higher10$plotID)),
                               AGB_MgC_ha = AGB_MgC_ha_alt1_1094,
                               HighCI = HighCI_alt1_1094,
                               LowCI = LowCI_alt1_1094,
                               Project_ID = rep("VCS1094"),
                               EquationType = rep("Brown (1997) [5]"))
MD_AGBPlot_alt1_1094

p1094_alt1 <- data.frame(MD_AGBPlot_alt1_1094 %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1094"),
                       EquationType = rep("Brown (1997) [5]"))
p1094_alt1

# VCS1094 alternative function 2####

#equation from Araujo et al 1999
 # M = a * (b*D^c) a = 0.6, b = 4.06, c = 1.76 Araujo et al 1999 from 
VCS1094_AGB_alt2_function <- function(D) { 
  AGB = 0.6 *(4.06 * (D^1.76)) * exp((0.4^2)/2)
  print(AGB)
}
AGBPlot_alt2_1094 <- by(md_brana01_1094_higher10, md_brana01_1094_higher10$plotID,
                   function(x) VCS1094_AGB_alt2_function(D = x$D),
                   simplify = F)
AGBplot_Alt2_1094 <- sapply(AGBPlot_alt2_1094, sum)
AGBplot_Alt2_1094 # kg per 1 ha

AGBplot_Alt2_1094_c <- sapply(AGBPlot_alt2_1094, length)
AGBplot_Alt2_1094_c

# Error 
AGB_MgC_ha2_1094 = (AGBplot_Alt2_1094/1000) * 0.456
AGB_MgC_ha2_1094
SDAGB2_1094 = AGB_MgC_ha2_1094 * 0.20
SEAGB2_1094 = SDAGB2_1094/sqrt(AGBplot_Alt2_1094_c)
HighCI2_1094 = AGB_MgC_ha2_1094 + (1.96*SEAGB2_1094)
LowCI2_1094 = AGB_MgC_ha2_1094 - (1.96*SEAGB2_1094)

MD_AGBPlot_alt2_1094<-  data.frame (PlotID = unique(levels(md_brana01_1094_higher10$plotID)),
                               AGB_MgC_ha = AGB_MgC_ha2_1094,
                               HighCI = HighCI2_1094,
                               LowCI = LowCI2_1094,
                               Project_ID = rep("VCS1094"),
                               EquationType = rep("Araujo (1999) [4]"))
MD_AGBPlot_alt2_1094

p1094_alt2 <- data.frame(MD_AGBPlot_alt2_1094 %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1094"),
                         EquationType = rep("Araujo (1999) [4]"))
p1094_alt2

## VCS 1094 alt function3 - Baker et al 2004 eq 1

VCS1094_AGB_alt3_function <- function(D) { # create a function with the name my_function
  AGB = exp(0.33 * log(D) + 0.933 * (log(D)^2) - 0.122 * (log(D)^3) - 0.37) * exp((0.4^2)/2)
  print(AGB) # this returns AGB in kg
} 
AGBPlot_alt3_1094 <- by(md_brana01_1094_higher10, md_brana01_1094_higher10$plotID,
                   function(x) VCS1094_AGB_alt3_function(D = x$D),
                   simplify = F)
AGBplot_Alt3_1094 <- sapply(AGBPlot_alt3_1094, sum)
AGBplot_Alt3_1094 # kg per 1 ha

AGBplot_Alt3_1094_c <- sapply(AGBPlot_alt3_1094, length)
AGBplot_Alt3_1094_c

# Error
AGB_MgC_ha3_1094 = (AGBplot_Alt3_1094/1000) * 0.456
SDAGB3_1094 = AGB_MgC_ha3_1094 * 0.20
SEAGB3_1094 = SDAGB3_1094/sqrt(AGBplot_Alt3_1094_c)
HighCI3_1094 = AGB_MgC_ha3_1094 + (1.96*SEAGB3_1094)
LowCI3_1094 = AGB_MgC_ha3_1094 - (1.96*SEAGB3_1094)

MD_AGBPlot_alt3_1094 <-  data.frame (PlotID = unique(levels(md_brana01_1094_higher10$plotID)),
                               AGB_MgC_ha = AGB_MgC_ha3_1094,
                               HighCI = HighCI3_1094,
                               LowCI = LowCI3_1094,
                               Project_ID = rep("VCS1094"),
                               EquationType = rep("Baker (2004) [4]"))
MD_AGBPlot_alt3_1094

p1094_alt3 <- data.frame(MD_AGBPlot_alt3_1094 %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1094"),
                         EquationType = rep("Baker (2004) [4]"))
p1094_alt3

# VCS1094 - alt function 4 ####
# ln(Dry weight) = a + b ln(Diameter)] from Nogueira et al 2008, a = -1.716, b = 2.413

VCS1094_AGB_alt4_function <- function(D) { # create a function with the name my_function
  AGB = exp(-1.716 + (2.413 * log(D))) * exp((0.306^2)/2)
  print(AGB) # AGB in Mg
}
AGBPlot_alt4_1094 <- by(md_brana01_1094_higher10, md_brana01_1094_higher10$plotID,
                   function(x) VCS1094_AGB_alt4_function(D = x$D),
                   simplify = F)
AGBplot_Alt4_1094 <- sapply(AGBPlot_alt4_1094, sum)
AGBplot_Alt4_1094 # kg per 1 ha

AGBplot_Alt4_1094_c <- sapply(AGBPlot_alt4_1094, length)
AGBplot_Alt4_1094_c

# Adding CIs to data frame
AGB_MgC_ha4_1094 = (AGBplot_Alt4_1094/1000) * 0.456
AGB_MgC_ha4_1094
SDAGB4_1094 = AGB_MgC_ha4_1094 * 0.20
SEAGB4_1094 = SDAGB4_1094/sqrt(AGBplot_Alt4_1094_c)
HighCI4_1094 = AGB_MgC_ha4_1094 + (1.96*SEAGB4_1094)
LowCI4_1094 = AGB_MgC_ha4_1094 - (1.96*SEAGB4_1094)

MD_AGBPlot_alt4_1094 <-  data.frame (PlotID = unique(levels(md_brana01_1094_higher10$plotID)),
                               AGB_MgC_ha = AGB_MgC_ha4_1094,
                               HighCI = HighCI4_1094,
                               LowCI = LowCI4_1094,
                               Project_ID = rep("VCS1094"),
                               EquationType = rep("Nogueira (2008) [4]"))
MD_AGBPlot_alt4_1094

p1094_alt4 <- data.frame(MD_AGBPlot_alt4_1094 %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1094"),
                         EquationType = rep("Nogueira (2008) [4]"))
p1094_alt4

## 1094 Plotting ####

# combining in data frame####

VCS1094_AGB_Data2 <- rbind(p1094_er, 
                          p1094_r5,
                          p1094_alt1,
                          p1094_alt2,
                          p1094_alt3,
                           p1094_alt4) # Brown 1997
VCS1094_AGB_Data2  

vcs1094_toplot2 <-VCS1094_AGB_Data2

#Calcularing tCO2e/ha
vcs1094_toplot2$EquationType = as.factor(vcs1094_toplot2$EquationType)
vcs1094_toplot2$AGC_tCO2_ha = as.numeric(vcs1094_toplot2$mAGB_MgC_ha * (44/12))
vcs1094_toplot2$HCI_tCO2_ha = as.numeric(vcs1094_toplot2$HCI * (44/12))
vcs1094_toplot2$LCI_tCO2_ha = as.numeric(vcs1094_toplot2$LCI * (44/12))

## Plotting VCS1094 #######

p_1094 <- ggplot(vcs1094_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha), 
                            y = AGC_tCO2_ha, 
                            # ymin = LCI_tCO2_ha, 
                            # ymax = HCI_tCO2_ha, 
                            color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,650)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS1094 (VM0015)", 
       subtitle = "8 1-ha moist forest plots")+
  theme_pubr()+ #scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0015] VCS1094 Brazil")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project",  unhighlighted_params = list(colour = "#302828"))
p_1094


# 1094 Data Stats ####

VCS1094_AGB_Data3 <- rbind(p1094_er, 
                           p1094_r5,
                           p1094_alt1,
                           p1094_alt2,
                           p1094_alt3,
                           p1094_alt4,
                           p1094_pr) # Brown 1997
VCS1094_AGB_Data3  

vcs1094_toplot3 <-VCS1094_AGB_Data3

#Calcularing tCO2e/ha
vcs1094_toplot3$EquationType = as.factor(vcs1094_toplot3$EquationType)
vcs1094_toplot3$AGC_tCO2_ha = as.numeric(vcs1094_toplot3$mAGB_MgC_ha * (44/12))
vcs1094_toplot3$HCI_tCO2_ha = as.numeric(vcs1094_toplot3$HCI * (44/12))
vcs1094_toplot3$LCI_tCO2_ha = as.numeric(vcs1094_toplot3$LCI * (44/12))

## VCS1541 Guatemala ####

# 1541 Project equation ####
#Log10(AGB) = -4.09992 + 2.57782*Log10(DBH) 

VCS1541_AGB_function <- function(D) { # 1541 project equation
  AGB = 10^(-4.09992 + 2.57782 * log10(D)) * exp((0.3^2)/2)
  print(AGB) # this returns AGB in kg
}
AGBPlot_Proj_1541 <- by(md_and01_1541_higher10, md_and01_1541_higher10$plotID,
                        function(x) VCS1541_AGB_function(D = x$D),
                        simplify = F)
AGBplot_ProjD_1541 <- sapply(AGBPlot_Proj_1541, sum)
AGBplot_ProjD_1541

AGBplot_ProjD_1541_c <- sapply(AGBPlot_Proj_1541, length)

# 10% error

AGB_MgC_ha_proj1541 = (AGBplot_ProjD_1541) * 0.50 # carbon fraction used by the project
SDAGB_proj_1541 = AGB_MgC_ha_proj1541 * 0.20
SEAGB_proj_1541 = SDAGB_proj_1541/sqrt(AGBplot_ProjD_1541_c)
HighCI_proj_1541 = AGB_MgC_ha_proj1541 + (1.96*SEAGB_proj_1541)
LowCI_proj_1541 = AGB_MgC_ha_proj1541 - (1.96*SEAGB_proj_1541)

AGBPlot_proj_1541 <-  data.frame (PlotID = unique(levels(md_and01_1541_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_proj1541,
                                  HighCI = HighCI_proj_1541,
                                  LowCI = LowCI_proj_1541,
                                  Project_ID = rep("VCS1541"),
                                  EquationType = rep("Project [4]"))
AGBPlot_proj_1541

AGBPlot_proj_1541_f <- AGBPlot_proj_1541 %>% filter(AGB_MgC_ha > 80) 
AGBPlot_proj_1541_f

p1541_pr <- data.frame(AGBPlot_proj_1541_f %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1541"),
                       EquationType = rep("Project [4]"))
p1541_pr

## Alt equations 1541 ####

# Alt equation 1 - Araujo 1999 
VCS1541_AGB_alt1_function <- function(D) {
  AGB = 0.6 *(4.06 * (D^1.76)) * exp((0.3^2)/2)
  print(AGB)
}
AGBPlot_alt1_v1_1541 <- by(md_and01_1541_higher10, md_and01_1541_higher10$plotID,
                        function(x) VCS1541_AGB_alt1_function(D = x$D),
                        simplify = F)
AGBplot_alt1_1541 <- sapply(AGBPlot_alt1_v1_1541, sum)
AGBplot_alt1_1541 # kg per 1 ha

AGBplot_alt1_1541_c <- sapply(AGBPlot_alt1_v1_1541, length)
AGBplot_alt1_1541_c

# Error
AGB_MgC_ha_alt1_1541 = (AGBplot_alt1_1541/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_alt1_1541
SDAGB_alt1_1541 = AGB_MgC_ha_alt1_1541 * 0.20
SEAGB_alt1_1541 = SDAGB_alt1_1541/sqrt(AGBplot_alt1_1541_c)
HighCI_alt1_1541 = AGB_MgC_ha_alt1_1541 + (1.96*SEAGB_alt1_1541)
LowCI_alt1_1541 = AGB_MgC_ha_alt1_1541 - (1.96*SEAGB_alt1_1541)

AGBPlot_alt1_1541 <-  data.frame (PlotID = unique(levels(md_and01_1541_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_alt1_1541,
                                  HighCI = HighCI_alt1_1541,
                                  LowCI = LowCI_alt1_1541,
                                  Project_ID = rep("VCS1541"),
                                  EquationType = rep("Araujo (1999) [4]"))
AGBPlot_alt1_1541

AGBPlot_alt1_1541_f <- AGBPlot_alt1_1541 %>% filter(AGB_MgC_ha > 80) 
AGBPlot_alt1_1541_f

p1541_alt1 <- data.frame(AGBPlot_alt1_1541_f %>%
                         summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                   se = sd(AGB_MgC_ha)/sqrt(n()),
                                   HCI = mAGB_MgC_ha + (1.96*se), 
                                   LCI = mAGB_MgC_ha - (1.96*se),
                                   N = n()), 
                       Project_ID = rep("VCS1541"),
                       EquationType = rep("Araujo (1999) [4]"))
p1541_alt1

## Alt equation 2 - Arreaga 2002 eq 22 ####

VCS1541_AGB_alt_function2 <- function(D) { # create a function with the name my_function
  AGB = exp(-9.44041 + (2.57782 * log(D))) * exp((0.3^2)/2)
  print(AGB)
}
AGBPlot_alt2_v1_1541 <- by(md_and01_1541_higher10, md_and01_1541_higher10$plotID,
                           function(x) VCS1541_AGB_alt_function2(D = x$D),
                           simplify = F)
AGBplot_alt2_1541 <- sapply(AGBPlot_alt2_v1_1541, sum)
AGBplot_alt2_1541 # kg per 1 ha

AGBplot_alt2_1541_c <- sapply(AGBPlot_alt2_v1_1541, length)
AGBplot_alt2_1541_c

# 10% error

AGB_MgC_ha_alt2_1541 = (AGBplot_alt2_1541) * 0.456 # carbon fraction by Martin et al 2018
SDAGB_alt2_1541 = AGB_MgC_ha_alt2_1541 * 0.20
SEAGB_alt2_1541 = SDAGB_alt2_1541/sqrt(AGBplot_alt2_1541_c)
HighCI_alt2_1541 = AGB_MgC_ha_alt2_1541 + (1.96*SEAGB_alt2_1541)
LowCI_alt2_1541 = AGB_MgC_ha_alt2_1541 - (1.96*SEAGB_alt2_1541)

AGBPlot_alt2_1541 <-  data.frame (PlotID = unique(levels(md_and01_1541_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_alt2_1541,
                                  HighCI = HighCI_alt2_1541,
                                  LowCI = LowCI_alt2_1541,
                                  Project_ID = rep("VCS1541"),
                                  EquationType = rep("Arreaga (2002) [4]"))
AGBPlot_alt2_1541

AGBPlot_alt2_1541_f <- AGBPlot_alt2_1541 %>% filter(AGB_MgC_ha > 80) 
AGBPlot_alt2_1541_f

p1541_alt2 <- data.frame(AGBPlot_alt2_1541_f %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1541"),
                         EquationType = rep("Arreaga (2002) [4]"))
p1541_alt2

# Alvarez 2012 Type II.5 Tropical moist 

VCS1541_AGB_alt_function3 <- function(D, WD) { # create a function with the name my_function
  AGB = exp(-0.983 + (2.350 * log(D)) +  log(WD)) * exp((0.364^2)/2)
  print(AGB)
}

AGBPlot_alt3_v1_1541 <- by(md_and01_1541_higher10, md_and01_1541_higher10$plotID,
                           function(x) VCS1541_AGB_alt_function3(D = x$D, WD = x$WD),
                           simplify = F)
AGBplot_alt3_1541 <- sapply(AGBPlot_alt3_v1_1541, sum)
AGBplot_alt3_1541 # kg per 1 ha

AGBplot_alt3_1541_c <- sapply(AGBPlot_alt3_v1_1541, length)
AGBplot_alt3_1541_c

# 10% error
AGB_MgC_ha_alt3_1541 = (AGBplot_alt3_1541/1000) * 0.456 # carbon fraction by Martin et al 2018
SDAGB_alt3_1541 = AGB_MgC_ha_alt3_1541 * 0.20
SEAGB_alt3_1541 = SDAGB_alt3_1541/sqrt(AGBplot_alt3_1541_c)
HighCI_alt3_1541 = AGB_MgC_ha_alt3_1541 + (1.96*SEAGB_alt3_1541)
LowCI_alt3_1541 = AGB_MgC_ha_alt3_1541 - (1.96*SEAGB_alt3_1541)

AGBPlot_alt3_1541 <-  data.frame (PlotID = unique(levels(md_and01_1541_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_alt3_1541,
                                  HighCI = HighCI_alt3_1541,
                                  LowCI = LowCI_alt3_1541,
                                  Project_ID = rep("VCS1541"),
                                  EquationType = rep("Alvarez (2012) [4]"))
AGBPlot_alt3_1541

AGBPlot_alt3_1541_f <- AGBPlot_alt3_1541 %>% filter(AGB_MgC_ha > 40) 
AGBPlot_alt3_1541_f

p1541_alt3 <- data.frame(AGBPlot_alt3_1541_f %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1541"),
                         EquationType = rep("Alvarez (2012) [4]"))
p1541_alt3

# Alternative equation 4 - Nogueira 2008 

VCS1541_AGB_alt_function4 <- function(D) { 
  AGB = exp(-1.716 + (2.413 * log(D))) * exp((0.306^2)/2)
  print(AGB) # AGB in Mg
}
AGBPlot_alt4_v1_1541 <- by(md_and01_1541_higher10, md_and01_1541_higher10$plotID,
                           function(x) VCS1541_AGB_alt_function4(D = x$D),
                           simplify = F)
AGBplot_alt4_1541 <- sapply(AGBPlot_alt4_v1_1541, sum)
AGBplot_alt4_1541 # kg per 1 ha

AGBplot_alt4_1541_c <- sapply(AGBPlot_alt4_v1_1541, length)
AGBplot_alt4_1541_c

# Error
AGB_MgC_ha_alt4_1541 = (AGBplot_alt4_1541/1000) * 0.456 # carbon fraction by Martin et al 2018
SDAGB_alt4_1541 = AGB_MgC_ha_alt4_1541 * 0.10
SEAGB_alt4_1541 = SDAGB_alt4_1541/sqrt(AGBplot_alt4_1541_c)
HighCI_alt4_1541 = AGB_MgC_ha_alt4_1541 + (1.96*SEAGB_alt4_1541)
LowCI_alt4_1541 = AGB_MgC_ha_alt4_1541 - (1.96*SEAGB_alt4_1541)

AGBPlot_alt4_1541 <-  data.frame (PlotID  = unique(levels(md_and01_1541_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_alt4_1541,
                                  HighCI = HighCI_alt4_1541,
                                  LowCI = LowCI_alt4_1541,
                                  Project_ID = rep("VCS1541"),
                                  EquationType = rep("Nogueira (2008) [4]"))
AGBPlot_alt4_1541

AGBPlot_alt4_1541_f <- AGBPlot_alt4_1541 %>% filter(AGB_MgC_ha > 40) 
AGBPlot_alt4_1541_f

p1541_alt4 <- data.frame(AGBPlot_alt4_1541_f %>%
                           summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                     se = sd(AGB_MgC_ha)/sqrt(n()),
                                     HCI = mAGB_MgC_ha + (1.96*se), 
                                     LCI = mAGB_MgC_ha - (1.96*se),
                                     N = n()), 
                         Project_ID = rep("VCS1541"),
                         EquationType = rep("Nogueira (2008) [4]"))
p1541_alt4

## 1541 plotting ####

VCS1541_AGB_Data2 <- rbind(p1541_er, 
                           p1541_r5,
                          p1541_alt1,
                          p1541_alt2,
                          p1541_alt3,
                          p1541_alt4)
#AGBPlot_alt1_1541)
VCS1541_AGB_Data2  

vcs1541_toplot2 <-VCS1541_AGB_Data2 #%>% filter(PlotID<5)

# Calculating tCO2e/ha\
vcs1541_toplot2$EquationType = as.factor(vcs1541_toplot2$EquationType)
vcs1541_toplot2$AGC_tCO2_ha = as.numeric(vcs1541_toplot2$mAGB_MgC_ha * (44/12))
vcs1541_toplot2$HCI_tCO2_ha = as.numeric(vcs1541_toplot2$HCI * (44/12))
vcs1541_toplot2$LCI_tCO2_ha = as.numeric(vcs1541_toplot2$LCI * (44/12))

p_1541 <- ggplot(vcs1541_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha), 
                                      y = AGC_tCO2_ha, 
                                      # ymin = LCI_tCO2_ha, 
                                      # ymax = HCI_tCO2_ha, 
                                      color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,600)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS1541 (VM0015)", 
       subtitle = "7 1-ha moist forest plots")+
  theme_pubr()+ #scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0015] VCS1541 Guatemala")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9))+
  theme(axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p_1541

#b1541 <- list(geom_hline(yintercept = 366.28, color = '#003f5c',alpha=0.4),
 #            annotate("rect", ymin = 344.12, ymax = 388.45,xmin = -Inf, xmax = Inf,alpha=0.1,linetype=2))

#p_1541_final <- p_1541+geom_hline(aes(yintercept=366.28), lty=2, color = "gray", cex=1.2, alpha = 0.6)+b1541
#p_1541_final

# 1541 Data Stats ####

VCS1541_AGB_Data3 <- rbind(p1541_er, 
                           p1541_r5,
                           p1541_alt1,
                           p1541_alt2,
                           p1541_alt3,
                           p1541_alt4,
                           p1541_pr)
#AGBPlot_alt1_1541)
VCS1541_AGB_Data3 

vcs1541_toplot3 <-VCS1541_AGB_Data3 #%>% filter(PlotID<5)

# Calculating tCO2e/ha\
vcs1541_toplot3$EquationType = as.factor(vcs1541_toplot3$EquationType)
vcs1541_toplot3$AGC_tCO2_ha = as.numeric(vcs1541_toplot3$mAGB_MgC_ha * (44/12))
vcs1541_toplot3$HCI_tCO2_ha = as.numeric(vcs1541_toplot3$HCI * (44/12))
vcs1541_toplot3$LCI_tCO2_ha = as.numeric(vcs1541_toplot3$LCI * (44/12))

# Project VCS944 (Peru) ####

# Equation used  AGB = 1.96 - 1.098 ln(DBH)+ 1.169 (ln(DBH))^2 -0.122 (ln(DBH))^3+ 1.061ln(WD) # Alvarez pre montane moist

VCS944_AGB_function <- function(D, WD) { # create a function with the name my_function
  AGB = exp(1.96 - (1.098 * log(D)) + (1.169 * (log(D)^2)) - (0.122 * (log(D)^3)) + (1.061 * log(WD))) * exp((0.336^2)/2)
  print(AGB) # this returns AGB in kg
}
AGBPlot_Proj_944 <- by(md_944_higher10, md_944_higher10$plotID,
                       function(x) VCS944_AGB_function(D = x$D, WD = x$WD),
                       simplify = F)
AGBplot_ProjD_WD_944 <- sapply(AGBPlot_Proj_944, sum)
AGBplot_ProjD_WD_944

AGBplot_ProjD_WD_944_c <- sapply(AGBPlot_Proj_944, length)

# 10% error
AGB_MgC_ha_proj944 = (AGBplot_ProjD_WD_944/1000) * 0.50 # carbon fraction used by the project
AGB_MgC_ha_proj944
SDAGB_proj_944 = AGB_MgC_ha_proj944 * 0.20
SEAGB_proj_944 = SDAGB_proj_944/sqrt(AGBplot_ProjD_WD_944_c)
HighCI_proj_944 = AGB_MgC_ha_proj944 + (1.96*SEAGB_proj_944)
LowCI_proj_944 = AGB_MgC_ha_proj944 - (1.96*SEAGB_proj_944)

AGBPlot_proj_944 <-  data.frame (PlotID  = unique(levels(md_944_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_proj944,
                                  HighCI = HighCI_proj_944,
                                  LowCI = LowCI_proj_944,
                                  Project_ID = rep("VCS944"),
                                  EquationType = rep("Project [4]"))
AGBPlot_proj_944

p944_pr <- data.frame(AGBPlot_proj_944 %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                  se = sd(AGB_MgC_ha)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS944"),
                      EquationType = rep("Project [4]"))
p944_pr

## Alt equation 1 944 ####
# Alt 1 Alvarez 2012 type I.1 Pm moist
VCS944_AGB_alt_function1 <- function(D,H,WD) { #Alvarez 2012 type I.1 Pmw
  AGB = exp(-2.221 + 2.081 * log(D) + 0.587 * log(H) + 1.089 * log(WD)) * exp((0.336^2)/2)
  print(AGB)
}
AGBPlot_alt1_v1_944 <- by(md_944_higher10, md_944_higher10$plotID,
                           function(x) VCS944_AGB_alt_function1(D = x$D, H = x$Hfeld, WD = x$WD),
                           simplify = F)
AGBplot_alt1_944 <- sapply(AGBPlot_alt1_v1_944, sum)
AGBplot_alt1_944 # Mg per 1 ha

AGBplot_alt1_944_c <- sapply(AGBPlot_alt1_v1_944, length)
AGBplot_alt1_944_c

# Error
AGB_MgC_ha_alt1_944 = (AGBplot_alt1_944/1000) * 0.456 # carbon fraction by Martin et al 2018
SDAGB_alt1_944 = AGB_MgC_ha_alt1_944 * 0.20
SEAGB_alt1_944 = SDAGB_alt1_944/sqrt(AGBplot_alt1_944_c)
HighCI_alt1_944 = AGB_MgC_ha_alt1_944 + (1.96*SEAGB_alt1_944)
LowCI_alt1_944 = AGB_MgC_ha_alt1_944 - (1.96*SEAGB_alt1_944)

AGBPlot_alt1_944 <-  data.frame (PlotID  = unique(levels(md_944_higher10$plotID)),
                                  AGB_MgC_ha = AGB_MgC_ha_alt1_944,
                                  HighCI = HighCI_alt1_944,
                                  LowCI = LowCI_alt1_944,
                                  Project_ID = rep("VCS944"),
                                  EquationType = rep("Alvarez (2012) [4]"))
AGBPlot_alt1_944

p944_alt1 <- data.frame(AGBPlot_alt1_944 %>%
                        summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                  se = sd(AGB_MgC_ha)/sqrt(n()),
                                  HCI = mAGB_MgC_ha + (1.96*se), 
                                  LCI = mAGB_MgC_ha - (1.96*se),
                                  N = n()), 
                      Project_ID = rep("VCS944"),
                      EquationType = rep("Alvarez (2012) [4]"))
p944_alt1

## Alt equation 2 944 ####
# Araujo et al 1999 moist
VCS944_AGB_alt2.1_function <- function(D) { 
   AGB = 0.6 *(4.06 * (D^1.76)) * exp((0.336^2)/2)
   print(AGB)
}

AGBPlot_alt2.1_v1_944 <- by(md_944_higher10, md_944_higher10$plotID,
                          function(x) VCS944_AGB_alt2.1_function(D = x$D),
                          simplify = F)
AGBplot_alt2.1_944 <- sapply(AGBPlot_alt2.1_v1_944, sum)
AGBplot_alt2.1_944 # kg per 1 ha

AGBplot_alt2.1_944_c <- sapply(AGBPlot_alt2.1_v1_944, length)
AGBplot_alt2.1_944_c

# Error
AGB_MgC_ha_alt2.1_944 = (AGBplot_alt2.1_944/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_alt2.1_944
SDAGB_alt2.1_944 = AGB_MgC_ha_alt2.1_944 * 0.20
SEAGB_alt2.1_944 = SDAGB_alt2.1_944/sqrt(AGBplot_alt2.1_944_c)
HighCI_alt2.1_944 = AGB_MgC_ha_alt2.1_944 + (1.96*SEAGB_alt2.1_944)
LowCI_alt2.1_944 = AGB_MgC_ha_alt2.1_944 - (1.96*SEAGB_alt2.1_944)

AGBPlot_alt2.1_944 <-  data.frame (PlotID  = unique(levels(md_944_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt2.1_944,
                                 HighCI = HighCI_alt2.1_944,
                                 LowCI = LowCI_alt2.1_944,
                                 Project_ID = rep("VCS944"),
                                 EquationType = rep("Araujo (1999) [4]"))
AGBPlot_alt2.1_944

p944_alt2 <- data.frame(AGBPlot_alt2.1_944 %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                    se = sd(AGB_MgC_ha)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS944"),
                        EquationType = rep("Araujo (1999) [4]"))
p944_alt2

## Alt equation 3 944 ####
# Alt 3 Nenninger 2006 in Román-Cuesta et al 2011 # AGB =  0.07 * D^2.417 * exp((0.336^2)/2)
# DeWalt and Chave 2004 Biomass=exp(0.298+(1.027*log((BA))))
VCS944_AGB_alt_function3 <- function(WD,D) { 
  AGB =  (0.07 * D^2.417) * exp((0.336^2)/2)
  print(AGB) # AGB in kg
}
AGBPlot_alt3_v1_944 <- by(md_944_higher10, md_944_higher10$plotID,
                          function(x) VCS944_AGB_alt_function3(D = x$D),
                          simplify = F)
AGBplot_alt3_944 <- sapply(AGBPlot_alt3_v1_944, sum)
AGBplot_alt3_944 # kg per 1 ha

AGBplot_alt3_944_c <- sapply(AGBPlot_alt3_v1_944, length)
AGBplot_alt3_944_c

# Error
AGB_MgC_ha_alt3_944 = (AGBplot_alt3_944/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_alt3_944
SDAGB_alt3_944 = AGB_MgC_ha_alt3_944 * 0.20
SEAGB_alt3_944 = SDAGB_alt3_944/sqrt(AGBplot_alt3_944_c)
HighCI_alt3_944 = AGB_MgC_ha_alt3_944 + (1.96*SEAGB_alt3_944)
LowCI_alt3_944 = AGB_MgC_ha_alt3_944 - (1.96*SEAGB_alt3_944)

AGBPlot_alt3_944 <-  data.frame (PlotID  = unique(levels(md_944_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt3_944,
                                 HighCI = HighCI_alt3_944,
                                 LowCI = LowCI_alt3_944,
                                 Project_ID = rep("VCS944"),
                                 EquationType = rep("Nenninger (2006) [4]"))
AGBPlot_alt3_944

p944_alt3 <- data.frame(AGBPlot_alt3_944 %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                    se = sd(AGB_MgC_ha)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS944"),
                        EquationType = rep("Nenninger (2006) [4]"))
p944_alt3

## Alt equation 4 944 ## 
# Nogueira et al 2008

VCS944_AGB_alt_function4 <- function(D) { 
  AGB = exp(-1.716 + (2.413 * log(D)))
  print(AGB) # AGB in Mg
}
AGBPlot_alt4_v1_944 <- by(md_944_higher10, md_944_higher10$plotID,
                          function(x) VCS944_AGB_alt_function4(D = x$D),
                          simplify = F)
AGBplot_alt4_944 <- sapply(AGBPlot_alt4_v1_944, sum)
AGBplot_alt4_944 # kg per 1 ha

AGBplot_alt4_944_c <- sapply(AGBPlot_alt4_v1_944, length)
AGBplot_alt4_944_c

# Error
AGB_MgC_ha_alt4_944 = (AGBplot_alt4_944/1000) * 0.456 # carbon fraction by Martin et al 2018
AGB_MgC_ha_alt4_944
SDAGB_alt4_944 = AGB_MgC_ha_alt4_944 * 0.20
SEAGB_alt4_944 = SDAGB_alt4_944/sqrt(AGBplot_alt4_944_c)
HighCI_alt4_944 = AGB_MgC_ha_alt4_944 + (1.96*SEAGB_alt4_944)
LowCI_alt4_944 = AGB_MgC_ha_alt4_944 - (1.96*SEAGB_alt4_944)

AGBPlot_alt4_944 <-  data.frame (PlotID  = unique(levels(md_944_higher10$plotID)),
                                 AGB_MgC_ha = AGB_MgC_ha_alt4_944,
                                 HighCI = HighCI_alt4_944,
                                 LowCI = LowCI_alt4_944,
                                 Project_ID = rep("VCS944"),
                                 EquationType = rep("Nogueira (2008) [4]"))
AGBPlot_alt4_944

p944_alt4 <- data.frame(AGBPlot_alt4_944 %>%
                          summarise(mAGB_MgC_ha = mean(AGB_MgC_ha),
                                    se = sd(AGB_MgC_ha)/sqrt(n()),
                                    HCI = mAGB_MgC_ha + (1.96*se), 
                                    LCI = mAGB_MgC_ha - (1.96*se),
                                    N = n()), 
                        Project_ID = rep("VCS944"),
                        EquationType = rep("Nogueira (2008) [4]"))
p944_alt4

## 944 plotting ####
VCS944_AGB_Data2 <- rbind(p944_er,
                         p944_r5,
                         p944_alt1,
                         p944_alt2,
                         #p944_alt3,
                         p944_alt4)
VCS944_AGB_Data2

vcs944_toplot2 <-VCS944_AGB_Data2 #%>% filter(AGB_MgC_ha>0)
vcs944_toplot2 #8 1-ha plots

# Calculating tCO2e/ha
vcs944_toplot2$EquationType = as.factor(vcs944_toplot2$EquationType)
vcs944_toplot2$AGC_tCO2_ha = as.numeric(vcs944_toplot2$mAGB_MgC_ha * (44/12))
vcs944_toplot2$HCI_tCO2_ha = as.numeric(vcs944_toplot2$HCI * (44/12))
vcs944_toplot2$LCI_tCO2_ha = as.numeric(vcs944_toplot2$LCI * (44/12))

p_944 <- ggplot(vcs944_toplot2, aes(x = reorder(EquationType, AGC_tCO2_ha), 
                                     y = AGC_tCO2_ha, 
                                     # ymin = LCI_tCO2_ha, 
                                     # ymax = HCI_tCO2_ha, 
                                     color = EquationType)) +
  geom_point(alpha=0.9, size=4, color = "black", shape = 21, fill = "darkgray")+ylim(0,700)+
  #scale_y_break(c(50, 220),scales=75)+
  labs(y = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       x = "",title = "VCS944 (VM0015)", 
       subtitle = "8 1-ha moist forest plots")+
  theme_pubr()+ #scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("[VM0015] VCS944 Peru")+
  theme(axis.text.x = element_text(size=12, angle = 25, vjust = 1, hjust=0.9),
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 16),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"),
        plot.subtitle = element_text(size = 16), 
        axis.title.y = element_text(face="bold"))#+gghighlight(EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p_944
  
# 944 Data Stats ###

VCS944_AGB_Data3 <- rbind(p944_er,
                          p944_r5,
                          p944_alt1,
                          p944_alt2,
                          #p944_alt3,
                          p944_alt4,
                          p944_pr)
VCS944_AGB_Data3

vcs944_toplot3 <-VCS944_AGB_Data3 #%>% filter(AGB_MgC_ha>0)
vcs944_toplot3 #8 1-ha plots

# Calculating tCO2e/ha
vcs944_toplot3$EquationType = as.factor(vcs944_toplot3$EquationType)
vcs944_toplot3$AGC_tCO2_ha = as.numeric(vcs944_toplot3$mAGB_MgC_ha * (44/12))
vcs944_toplot3$HCI_tCO2_ha = as.numeric(vcs944_toplot3$HCI * (44/12))
vcs944_toplot3$LCI_tCO2_ha = as.numeric(vcs944_toplot3$LCI * (44/12))

# Plot VM0015 ####
#p_VM15 <- (p_944_final|p_1094_final)+p_1541_final+plot_layout(widths = c(2,1), ncol=2)#+plot_annotation(tag_levels = 'a')& 
  #theme(plot.tag = element_text(size = 14,face="bold"),plot.tag.position =c(0.05,1))
#p_VM15

p_VM15 <- (p_944|p_1094)+p_1541+plot_layout(widths = c(1,1,1), ncol=3)#+plot_annotation(tag_levels = 'a')& 
#theme(plot.tag = element_text(size = 14,face="bold"),plot.tag.position =c(0.05,1))
p_VM15

ggsave(filename = "FigVM0015_v6.png",
       plot = p_VM15, width = 14, height = 8, units = 'cm',
       scale = 2, dpi = 800)

## Figure 2 ######

p_fig2 <- p_VM6 /p_VM7/p_VM9 /p_VM15 + plot_layout(ncol=1)  #+ p_VM9 + p_VM15 +plot_layout(ncol=1)
p_fig2

# Final Plot ####
p_fig2a <- p_VM6 /p_VM7/p_VM9 /p_VM15 + plot_layout(ncol=1) +plot_annotation(tag_levels = 'a')& 
  theme(plot.tag = element_text(size = 18,face="bold"),plot.tag.position =c(0.1,1)) #+ p_VM9 + p_VM15 +plot_layout(ncol=1)
p_fig2a

# saving Final Plot in high-resolution
ggsave(filename = "Fig2_vSA_Final3.png",
       plot = p_fig2a, width = 20, height = 20, units = 'cm',
       scale = 2, dpi = 1200)
# extra

# VCS1094 alternative functions####
# ln(B) = exp(−8.26077 + (1.73728 * log(D)) + (0.89154 * log(H)) + (0.96957 * log(WD))) from Romero et al 2020

#VCS1112_AGB_alt_function3 <- function(D, H, WD) { # create a function with the name my_function
# AGB = exp(-8.26077 + (1.73728 * log(D)) + (0.89154 * log(H)) + (0.96957 * log(WD)))
# print(AGB) # AGB in Mg
#}

## END ####












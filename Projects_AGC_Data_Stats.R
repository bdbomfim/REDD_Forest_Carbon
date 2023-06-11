
library(rstatix)
library(tidyverse)

## Combining all AGC data across projects

## New data frames ####
#vm6
data_1396<- vcs1396_toplot3
data_1396
data_1392<- vcs1392_toplot3
data_1392
data_1359<- vcs1359_toplot3
data_1359

#vm7
data_985<- vcs985_toplot3
data_985
data_1112<- vcs1112_toplot3
data_1112
data_1566<- vcs1566_toplot3
data_1566

#vm9
data_902<- vcs902_toplot2 # this one does not have a "Project" value
data_934<- vcs934_toplot3
data_934
data_1775<- vcs1775_toplot3 # full range of DBH, not included the project estimates yet
data_1775
data_1775_l65<- vcs1775_toplot4 # this for BGB
data_1775_l65

# vm15
data_944<- vcs944_toplot3
data_1094<- vcs1094_toplot3
data_1541<- vcs1541_toplot3


agc_all <- rbind(data_1396, data_1392, data_1359, 
                 data_985, data_1112, data_1566,
                 data_902, data_934, data_1775_l65, 
                 data_944, data_1094, data_1541)
agc_all

agc_all$Methodology <- c(rep("VM0006",3),
                         rep("VM0007",3),
                         rep("VM0009",3),
                         rep("VM0015",3))
agc_all

agc_all$Project_ID = as.factor(agc_all$Project_ID)
agc_all$Methodology = as.factor(agc_all$Methodology)

#### Data Stats by Project ####

## VM 6 ####

# 1396

data_1396$EquationType = as.factor(data_1396$EquationType)
str(data_1396)
data_1396

summary(data_1396)

proj_1396 <- data_1396 %>% filter(EquationType == "Project [3]")
proj_1396$AGC_tCO2_ha

data_1396$EquationType
range_1396_SA <- data_1396 %>% filter(EquationType!= "Project [3]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_all_alt = ((proj_1396$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1396_SA # rel_diff 1396 = 83.6%

data_1396$EquationType

range_1396_Pr_Comp <- data_1396 %>% 
  filter(EquationType == "Alvarez (2012) [3]" | 
   EquationType == "Alvarez (2012)-II.5 [3]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_1396$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            diff_proj_alt = ((proj_1396$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_1396_Pr_Comp # diff proj to alt = 8.27%

# 1392

proj_1392 <- data_1392 %>% filter(EquationType == "Project [3]")
proj_1392$AGC_tCO2_ha

data_1392$EquationType

range_1392_SA <- data_1392 %>% filter(EquationType!= "Project [3]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1392_SA # rel_diff_1392 = 125.29%

unique(levels(data_1392$EquationType))

range_1392_Pr_Comp <- data_1392 %>% 
  filter(EquationType == "Alvarez (2012)-I.1 [3]" | 
          EquationType == "Alvarez (2012)-II.1 [3]" | 
           EquationType == "Alvarez (2012)-II.5 [3]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_1392$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            diff_proj_alt = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_1392_Pr_Comp # diff_proj_alt = 31.307%

# 1359 

data_1359
proj_1359 <- data_1359 %>% filter(EquationType == "Project [4]")
proj_1359$AGC_tCO2_ha

data_1359$EquationType

range_1359_SA <- data_1359 %>% filter(EquationType!= "Project [4]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1359_SA # rel_diff_1359 = 50.02%

unique(levels(data_1359$EquationType))

range_1359_Pr_Comp <- data_1359 %>% 
  filter(EquationType == "Djomo (2010)-1 [4]" | 
           EquationType == "Djomo (2010)-2 [4]" | 
           EquationType == "Djomo (2010)-3 [4]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_1359$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100,
            diff_proj_alt = ((proj_1359$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_1359_Pr_Comp # diff_proj_alt = 135.36%

# VM 6 summary

mean_vm6_Pr_Comp <- (range_1396_Pr_Comp$diff_proj_alt + range_1392_Pr_Comp$diff_proj_alt + range_1359_Pr_Comp$diff_proj_alt)/3
mean_vm6_Pr_Comp

mean_vm6_r <- (range_1396_SA$rel_diff + range_1392_SA$rel_diff + range_1359_SA$rel_diff)/3
mean_vm6_r

# VM 6 data

vm6_dat <- rbind(range_1359_Pr_Comp,
                 range_1396_Pr_Comp,
                 range_1392_Pr_Comp)
vm6_dat$ProjectID <- c("VCS1359",
                       "VCS1396",
                       "VCS1392")
vm6_dat

vm6_dat$Methodology <- rep("VM0006",3)
vm6_dat

# VM 7 ####

# 985 

proj_985 <- data_985%>% filter(EquationType == "Project [5]")
proj_985

data_985$EquationType

range_985_SA <- data_985 %>% filter(EquationType!= "Project [5]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_985_SA # rel_diff_1112 = 37.75%

unique(levels(data_985$EquationType))

range_985_Pr_Comp <- data_985 %>% 
  filter(EquationType!= "Project [5]") %>%
  filter(EquationType!= "Chave (2014) [6]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            Proj_AGC = proj_985$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            diff_proj_alt = ((proj_985$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_985_Pr_Comp # diff_proj_alt = 4.48%

# 1112

proj_1112 <- data_1112 %>% filter(EquationType == "Project [5]")
proj_1112$AGC_tCO2_ha

data_1112$EquationType

range_1112_SA <- data_1112 %>% filter(EquationType!= "Project [5]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1112_SA # rel_diff_1112 = 30.18%

unique(levels(data_1112$EquationType))

range_1112_Pr_Comp <- data_1112 %>% 
  filter(EquationType!= "Project [5]") %>%
  filter(EquationType!= "Chave (2014) [6]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_1112$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            diff_proj_alt = ((proj_1112$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_1112_Pr_Comp # diff_proj_alt = 36.54%

# 1566
data_1566
proj_1566 <- data_1566 %>% filter(EquationType == "Project [4]")
proj_1566$AGC_tCO2_ha

data_1566$EquationType

range_1566_SA <- data_1566 %>% filter(EquationType!= "Project [4]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1566_SA # rel_diff_1112 = 86.79%

unique(levels(data_1566$EquationType))

range_1566_Pr_Comp <- data_1566 %>% 
  filter(EquationType!= "Project [4]") %>%
  filter(EquationType!= "Chave (2005) [5]") %>%
  filter(EquationType!= "Chave (2014) [6]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_1566$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            diff_proj_alt = ((proj_1566$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_1566_Pr_Comp # diff_proj_alt = 11.69%

# VM7 summary 
mean_vm7_Pr_Comp <- (range_1566_Pr_Comp$diff_proj_alt + range_1112_Pr_Comp$diff_proj_alt + range_985_Pr_Comp$diff_proj_alt)/3
mean_vm7_Pr_Comp

mean_vm7_r <- (range_1566_SA$rel_diff + range_1112_SA$rel_diff + range_985_SA$rel_diff)/3
mean_vm7_r

# VM 7 data

vm7_dat <- rbind(range_1566_Pr_Comp,
                 range_985_Pr_Comp,
                 range_1112_Pr_Comp)
vm7_dat$ProjectID <- c("VCS1566",
                       "VCS985",
                       "VCS1112")
vm7_dat

vm7_dat$Methodology <- rep("VM0007",3)
vm7_dat

# VM 9 ####

# 902
proj_902 <- data_902 %>% filter(EquationType == "Guy (1981) [4]")

range_902_SA <- data_902 %>% #filter(EquationType!= "Project [3]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_902_SA # rel_diff_1392 = 147.51%

unique(levels(data_902$EquationType))

range_902_Pr_Comp <- data_902 %>% 
  filter(EquationType!= "Chave (2005) [5]") %>%
  filter(EquationType!= "Chave (2014) [6]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_902$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            diff_proj_alt = ((proj_902$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_902_Pr_Comp # diff mean value to alternatives that are ranked 4 is 1.794969%

# 934 

proj_934 <- data_934 %>% filter(EquationType == "Project [6]")
proj_934

range_934_SA <- data_934 %>% filter(EquationType!= "Project [6]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_934_SA # rel_diff_934 = 184.0053%

unique(levels(data_934$EquationType))

range_934_Pr_Comp <- data_934%>% 
       filter(EquationType!= "Project [6]")%>%
         summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_934$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            diff_proj_alt = ((proj_934$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_934_Pr_Comp # diff proj = 61.04%

# 1775 

proj_1775_l65 <- data_1775_l65 %>% filter(EquationType == "Project [4]")
proj_1775_l65

data_1775_l65$EquationType

range_1775_SA <- data_1775 %>% #filter(EquationType!= "Project [5]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1775_SA # rel_diff_1775 = 100.16%

unique(levels(data_1775_l65$EquationType))

data_1775_l65

range_1775_Pr_Comp <- data_1775_l65 %>% 
  filter(EquationType!= "Project [4]") %>%
  filter(EquationType!= "Chave (2005) [5]") %>%
  filter(EquationType!= "Chave (2014) [6]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_1775$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            diff_proj_alt = ((proj_1775_l65$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_1775_Pr_Comp # diff_proj_alt = 29.79%

# VM9 summary

mean_vm9_Pr_Comp <- (range_1775_Pr_Comp$diff_proj_alt  + range_934_Pr_Comp$diff_proj_alt)/2
mean_vm9_Pr_Comp

mean_vm9_r <- (range_934_SA$rel_diff + range_1775_SA$rel_diff)/2
mean_vm9_r

# VM 9 data ## CONTINUE HERE TO FINISH FULL DATA FRAME AND
# PREPARE FIGURE WITH PROJ VS MEAN OF BETTER ALTERNATIVES

vm9_dat <- rbind(range_934_Pr_Comp,
                 range_1775_Pr_Comp)
vm9_dat$ProjectID <- c("VCS934",
                       "VCS1775")
vm9_dat$Methodology <- rep("VM0009",2)
vm9_dat

# VM 15####

proj_944 <- data_944 %>% filter(EquationType == "Project [4]")
proj_944

range_944_SA <- data_944 %>% filter(EquationType!= "Project [4]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_944_SA # rel_diff = 49.39%

unique(levels(data_944$EquationType))

range_944_Pr_Comp <- data_944 %>% 
  filter(EquationType!= "Chave (2005) [5]") %>% 
  filter(EquationType!= "Chave (2014) [6]") %>% 
  filter(EquationType!= "Project [4]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_944$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100,
            diff_proj_alt = ((proj_944$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_944_Pr_Comp # diff_proj_alt = 18.58%

# 1094

data_1094

proj_1094 <- data_1094 %>% filter(EquationType == "Project [4]")
proj_1094

range_1094_SA <- data_1094 %>% filter(EquationType!= "Project [4]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1094_SA # rel_diff_1359 = 22.21%

unique(levels(data_1094$EquationType))

range_1094_Pr_Comp <- data_1094 %>% 
  filter(EquationType!= "Chave (2005) [5]") %>% 
  filter(EquationType!= "Chave (2014) [6]") %>% 
  filter(EquationType!= "Brown (1997) [5]") %>% 
  filter(EquationType!= "Project [4]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_1094$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100,
            diff_proj_alt = ((proj_1094$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_1094_Pr_Comp # diff_proj_alt = 10.60%

# 1541
proj_1541 <- data_1541 %>% filter(EquationType == "Project [4]")
proj_1541

range_1541_SA <- data_1541 %>% filter(EquationType!= "Project [4]") %>%
  summarise(meanAGC = mean(AGC_tCO2_ha), min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha), 
            rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100, 
            meanhci = mean(HCI_tCO2_ha), minhci = min(HCI_tCO2_ha), maxhci = max(HCI_tCO2_ha),
            meanlci = mean(LCI_tCO2_ha), minlci = min(LCI_tCO2_ha), maxlci = max(LCI_tCO2_ha),
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1541_SA # rel_diff = 49.73%

unique(levels(data_1541$EquationType))

range_1541_Pr_Comp <- data_1541 %>% 
  filter(EquationType!= "Chave (2005) [5]") %>% 
  filter(EquationType!= "Chave (2014) [6]") %>% 
  filter(EquationType!= "Project [4]") %>% 
  summarise(meanAGC = mean(AGC_tCO2_ha), 
            min = min(AGC_tCO2_ha), max = max(AGC_tCO2_ha), 
            meanHCI = mean(HCI_tCO2_ha),
            meanLCI = mean(LCI_tCO2_ha),
            Proj_AGC = proj_1541$AGC_tCO2_ha,
            #rel_diff = ((max(AGC_tCO2_ha) - min(AGC_tCO2_ha))/min(AGC_tCO2_ha)) * 100,
            diff_proj_alt = ((proj_1541$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100,
            n = n()) 
range_1541_Pr_Comp # diff_proj_alt = -6.00%

mean_vm15_Pr_Comp <- (range_1541_Pr_Comp$diff_proj_alt  + range_944_Pr_Comp$diff_proj_alt + range_1094_Pr_Comp$diff_proj_alt )/3
mean_vm15_Pr_Comp

mean_vm15_r <- (range_944_SA$rel_diff + range_1094_SA$rel_diff + range_1541_SA$rel_diff)/3
mean_vm15_r

# VM 15 data

vm15_dat <- rbind(range_1541_Pr_Comp,
                 range_944_Pr_Comp,
                 range_1094_Pr_Comp)
vm15_dat$ProjectID <- c("VCS1541",
                       "VCS944",
                       "VCS1094")
vm15_dat$Methodology <- rep("VM0015",3)
vm15_dat

## Overall means across methodologies ####

overall_mean_pr <- (mean_vm6_Pr_Comp + mean_vm7_Pr_Comp + mean_vm9_Pr_Comp + mean_vm15_Pr_Comp)/4
overall_mean_pr

overall_r <- (mean_vm7_r + mean_vm6_r + mean_vm9_r + mean_vm15_r)/4
overall_r

mean_vm6 <- (range_1396$diff + range_1392$diff + range_1359$diff)/3
mean_vm6

mean_vm7 <- (range_985$diff + range_1112$diff + range_1566$diff)/3
mean_vm7

mean_vm9 <- (range_902$diff + range_934$diff + range_1775$diff)/3
mean_vm9

mean_vm15 <- (range_944$diff + range_1094$diff + range_1541$diff)/3
mean_vm15

overall_mean <- (mean_vm6 + mean_vm7 + mean_vm9 + mean_vm15)/4
overall_mean

## Plot project value vs mean of alternatives for all methodologies

## Data for plotting project comparisons #

vm_all_dat <- rbind(vm6_dat, vm7_dat, vm9_dat, vm15_dat)
vm_all_dat

vm_all_to_plot <- rbind(data.frame(Methodology = vm_all_dat$Methodology, 
                                   ProjectID = vm_all_dat$ProjectID,
                       Alternative_AGC = round(vm_all_dat$meanAGC,1), 
                       Project_AGC = round(vm_all_dat$Proj_AGC,1)))
str(vm_all_to_plot)

vm_all_to_plot$Methodology = as.factor(vm_all_to_plot$Methodology)

vm_all_data_long <- gather(vm_all_to_plot, condition, AGC, Alternative_AGC:Project_AGC, factor_key=TRUE)
vm_all_data_long$condition

str(vm_all_data_long)

# Rename factor names

levels(vm_all_data_long$condition)[levels(vm_all_data_long$condition)=="Alternative_AGC"] <- "Alternative"
levels(vm_all_data_long$condition)[levels(vm_all_data_long$condition)=="Project_AGC"] <- "Project"

# https://r-graphics.org/recipe-bar-graph-dot-plot

# Get the names, sorted first by lg, then by avg
nameorder <- vm_all_to_plot$ProjectID[order(vm_all_to_plot$Methodology, vm_all_to_plot$Project_AGC)]
nameorder2 <- vm_all_to_plot$ProjectID[order(vm_all_to_plot$Methodology)]

# Turn name into a factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels = nameorder)
vm_all_to_plot$ProjectID <- factor(vm_all_to_plot$ProjectID, levels = nameorder2)

head(vm_all_data_long)
levels(vm_all_data_long$Methodology)

## Plot Fig 3 proj comparison ####
p_fig3 <- ggplot(vm_all_data_long, aes(x = AGC, y = ProjectID)) +
  geom_segment(aes(yend = ProjectID), xend = 0, colour = "#B1A9A9") +
  geom_point(size = 3, aes(fill = condition), shape = 21, color = "black") +
  scale_fill_manual(values = c("darkgrey", "red"), limits = c("Alternative", "Project")) +#palette = "Set1",
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(Methodology ~ ., scales = "free_y", space = "free_y")+
  labs(x = expression(paste("Tree aboveground carbon stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       y = "REDD+ Project ID",title = "Project vs Alternative AGC Estimates",
       fill = "")+theme(axis.text.x = element_text(size=14, vjust = 1, hjust=0.5),
                          axis.text.y = element_text(size = 14), 
                          axis.title = element_text(size = 16),
                          plot.title = element_text(size = 18, face="bold"),
                          legend.text = element_text(size=16),
                          legend.position = c(0.85, 0.95),
                        legend.background = element_rect(fill='transparent'))+
  theme(strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="darkgrey", fill="white"))
p_fig3

ggsave(filename = "Fig3_v4.png",
       plot = p_fig3, width = 9, height = 9, units = 'cm',
       scale = 2, dpi = 1200)

ggplot(tophit, aes(x = avg, y = name)) +
  geom_segment(aes(yend = name), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = lg)) +
  scale_colour_brewer(palette = "Set1", limits = c("NL", "AL")) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),   # No horizontal grid lines
    legend.position = c(1, 0.55),           # Put legend inside plot area
    legend.justification = c(1, 0.5)
  )

ggplot(tophit, aes(x = avg, y = name)) +
  geom_segment(aes(yend = name), xend = 0, colour = "grey50") +
  geom_point(size = 3, aes(colour = lg)) +
  scale_colour_brewer(palette = "Set1", limits = c("NL", "AL"), guide = FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales = "free_y", space = "free_y")

## BGB

bgb_data_1775 <- p1775_bgb_toplot
bgb_data_1775

proj_1775_bgb <- bgb_data_1775 %>% filter(BGB_EquationType == "Project")
proj_1775_bgb$m

range_1775_bgb <- bgb_data_1775 %>%
  summarise(mean = mean(m), min = min(m), max = max(m), 
            diff = ((max(m) - min(m))/min(m)) * 100)
range_1775_bgb # 213.8%

# To obtain the mean of the five alternative equations
range_1775b_bgb <- bgb_data_1775 %>% filter(BGB_EquationType!= "Project") %>%
  summarise(mean = mean(m), min = min(m), max = max(m), 
            diff = ((max(m) - min(m))/min(m)) * 100,
            meanhci = mean(mHCI), minhci = min(mHCI), maxhci = max(mHCI),
            meanlci = mean(mLCI), minlci = min(mLCI), maxlci = max(mLCI),
            diff_proj = ((proj_1775_bgb$m - mean(m))/mean(m)) * 100) # this is the value representing de difference between the project and the alternative mean
range_1775b_bgb

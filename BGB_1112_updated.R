
## BGB plotting ##

library(ggplot2)
library(scales)
library(patchwork)
library(tidyverse)

# VM0007 ####

# Project 1112 data from VM0007_SA script

VCS1112_AGB_Data2 <- rbind(p1112_r5,
                           p1112_pr,
                            p1112_er,
                            p1112_alt1.1,
                            p1112_alt1a,
                            p1112_alt2,
                            p1112_alt3,
                            p1112_alt4)

VCS1112_AGB_Data2
str(VCS1112_AGB_Data2)

vcs1112_toplot<-VCS1112_AGB_Data2

vcs1112_toplot$Project_ID = as.factor(vcs1112_toplot$Project_ID)
vcs1112_toplot$EquationType = as.factor(vcs1112_toplot$EquationType)

#Calcularing tCO2e/ha
vcs1112_toplot$AGC_tCO2_ha = as.numeric(vcs1112_toplot$mAGB_MgC_ha * (44/12))
vcs1112_toplot$HCI_tCO2_ha = as.numeric(vcs1112_toplot$HCI * (44/12))
vcs1112_toplot$LCI_tCO2_ha = as.numeric(vcs1112_toplot$LCI * (44/12))

vcs1112_toplot

# Red dot - Project's BGB method ####
# BGB (t/ha) = exp (-1.0587 + 0.9256 * log(AGB t/ha))

bgb_1112_proj <- vcs1112_toplot %>% filter(EquationType == "Project [5]")

bgb_1112_proj$BGB_tCO2_ha = exp(-1.085 + (0.9256 * log(bgb_1112_proj$AGC_tCO2_ha)))
str(bgb_1392_proj)

bgb_1112_proj$BGB_EquationType = rep("Project-eq")
names(bgb_1112_proj)
bgb_1112_proj

# alternative

bgb_1112_proj2 <- bgc_all_to_plot %>% 
  filter(condition == "Project") %>% 
  filter(Project_ID == "VCS1112")
bgb_1112_proj2

bgb_1112_proj2$BGB_tCO2_ha = exp(-1.085 + (0.9256 * log(bgb_1112_proj2$MeanAGC_tCO2_ha)))
str(bgb_1112_proj2)

bgb_1112_proj2$BGB_EquationType = rep("Project-eq")
names(bgb_1112_proj2)
bgb_1112_proj2

# Blue Dot - Project BGB * Alternative AGB ####

bgb_1112_proj3 <- bgc_all_to_plot %>% 
  filter(condition == "Project") %>% 
  filter(Project_ID == "VCS1112")
bgb_1112_proj3

bgb_1112_proj3$BGB_tCO2_ha = exp(-1.085 + (0.9256 * log(bgb_1112_alt1.1$MeanAGC_tCO2_ha))) #this is the bgb method used by Mugasha 2013 v1

bgb_1112_proj3$BGB_EquationType = rep("Project_alt")
names(bgb_1112_proj3)
bgb_1112_proj3

# Gray dots - Alternative BGB with Alternative AGB ####

# Data frame for alternative methods
bgb_1112_alt <- vcs1112_toplot %>% filter(EquationType!="Project [5]")
bgb_1112_alt

# Alt 1 BGB ####
#0.2 R:S ratio (Vogt et al 1996)##

BGBPlot_alt1_1112<-  data.frame (BGB_tCO2_ha = bgb_1112_alt$AGC_tCO2_ha * 0.2,
                                 Project_ID = rep("VCS1112"),
                                 BGB_EquationType = rep("Vogt (1996)-rsr"))
BGBPlot_alt1_1112

p_bgb_1112_alt1 <- data.frame(BGBPlot_alt1_1112 %>%
                                summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha)),
                              Project_ID = rep("VCS1112"),
                              BGB_EquationType = rep("Vogt (1996)-rsr"))
p_bgb_1112_alt1

# alternartive

bgb_1112_alt1.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1112")
bgb_1112_alt1.1

bgb_1112_alt1.1$BGB_tCO2_ha = bgb_1112_alt1.1$MeanAGC_tCO2_ha * 0.2

bgb_1112_alt1.1$BGB_EquationType = rep("Vogt (1996)-rsr")

bgb_1112_alt1.1

# Alt 2 BGB ####
#0.28 R:S ratio (Ledo et al 2018)##

BGBPlot_alt2_1112<-  data.frame (BGB_tCO2_ha = bgb_1112_alt$AGC_tCO2_ha * 0.28,
                                 Project_ID = rep("VCS1112"),
                                 BGB_EquationType = rep("Ledo (2018)-rsr"))
BGBPlot_alt2_1112

p_bgb_1112_alt2 <- data.frame(BGBPlot_alt2_1112 %>%
                               summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha)),
                             Project_ID = rep("VCS1112"),
                             BGB_EquationType = rep("Ledo (2018)-rsr"))
p_bgb_1112_alt2

# alternative

bgb_1112_alt2.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1112")
bgb_1112_alt2.1

bgb_1112_alt2.1$BGB_tCO2_ha = bgb_1112_alt2.1$MeanAGC_tCO2_ha * 0.28

bgb_1112_alt2.1$BGB_EquationType = rep("Ledo (2018)-rsr")

bgb_1112_alt2.1

# Alt 3 BGB ####
#0.17 R:S ratio (Sanford & Cuevas 1996)##

BGBPlot_alt3_1112<-  data.frame (BGB_tCO2_ha = bgb_1112_alt$AGC_tCO2_ha * 0.17,
                                 Project_ID = rep("VCS1112"),
                                 BGB_EquationType = rep("Sanford-&-Cuevas (1996)-rsr"))
BGBPlot_alt3_1112

p_bgb_1112_alt3 <- data.frame(BGBPlot_alt3_1112 %>%
                                summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha)),
                              Project_ID = rep("VCS1112"),
                              BGB_EquationType = rep("Sanford-&-Cuevas (1996)-rsr"))
p_bgb_1112_alt3

# alternative

bgb_1112_alt3.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1112")
bgb_1112_alt3.1

bgb_1112_alt3.1$BGB_tCO2_ha = bgb_1112_alt2.1$MeanAGC_tCO2_ha * 0.17

bgb_1112_alt3.1$BGB_EquationType = rep("Sanford-&-Cuevas (1996)-rsr")

bgb_1112_alt3.1

# Combining data frames

VCS1112_BGB_Data <- rbind(bgb_1112_proj2,
                          bgb_1112_proj3,
                          bgb_1112_alt1.1,
                          bgb_1112_alt2.1, 
                          bgb_1112_alt3.1)
VCS1112_BGB_Data

VCS1112_BGB_Data$Project_ID = as.factor(VCS1112_BGB_Data$Project_ID)
VCS1112_BGB_Data$BGB_EquationType = as.factor(VCS1112_BGB_Data$BGB_EquationType)

str(VCS1112_BGB_Data)

vcs1112_bgb_final <- VCS1112_BGB_Data

vcs1112_bgb_final

vcs1112_bgb_final$condition2 <- as.factor(c(rep("Proj-BGB * Proj-AGB",1),
                                            rep("Proj-BGB * Alt-AGB",1),
                                            rep("Alt-BGB * Alt-AGB",3)))

levels(as.factor(vcs1112_bgb_final$condition2))
vcs1112_bgb_final

vcs1112_bgb_final$condition3 <- as.factor(c(rep("Project BGC equation (using the project AGC)",1),
                                            rep("Project BGC equation (using our alternative AGC)",1),
                                            rep("Alternative BGC equations (using our alternative AGC)",3)))

levels(as.factor(vcs1112_bgb_final$condition3))
vcs1112_bgb_final

vcs1112_bgb_final$BGB_EquationType2 <- as.factor(c(rep("Proj-BGC * Proj-AGC",1),
                                                   rep("Proj-BGC *Alt-AGC",1),
                                                   rep("Vogt (1996)",1),
                                                   rep("Ledo (2018)",1),
                                                   rep("Sanford-&-Cuevas (1996)",1)))

levels(as.factor(vcs1112_bgb_final$BGB_EquationType2))
vcs1112_bgb_final

## Plot  VCS1112 #######

p_1112_bgb_v2 <- ggplot(vcs1112_bgb_final, aes(y = reorder(BGB_EquationType2,BGB_tCO2_ha), x = BGB_tCO2_ha)) +
  geom_segment(aes(yend = BGB_EquationType2), xend = 0, colour = "#B1A9A9") +
  geom_point(size = 3, aes(fill = condition3), shape = 21, color = "black") +
  scale_fill_manual(values = c("darkgrey", "blue", "red"), 
                    limits = c("Alternative BGC equations (using our alternative AGC)",
                               "Project BGC equation (using our alternative AGC)",
                               "Project BGC equation (using the project AGC)"))+#palette = "Set1",
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  #facet_grid(Methodology ~ ., scales = "free_y", space = "free_y")+
  labs(x = expression(paste("Tree belowground carbon stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       y = "",title = "[VM0007] VCS1112",
       fill = "")+theme(axis.text.x = element_text(size=14, vjust = 1, hjust=0.5),
                        axis.text.y = element_text(size = 14), 
                        axis.title = element_text(size = 17),
                        plot.title = element_text(size = 17, face="bold"),
                        legend.text = element_text(size=14),
                        legend.position = "none",
                        legend.background = element_rect(fill='transparent'))+
  theme(strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="darkgrey", fill="white"))
p_1112_bgb_v2

p_1112_bgb <- ggplot(VCS1112_BGB_Data, aes(y = reorder(BGB_EquationType,BGB_tCO2_ha), x = BGB_tCO2_ha)) +
  geom_segment(aes(yend = BGB_EquationType), xend = 0, colour = "#B1A9A9") +
  geom_point(size = 3, aes(fill = condition), shape = 21, color = "black") +
  scale_fill_manual(values = c("darkgrey", "red"), limits = c("Alternative","Project")) +#palette = "Set1",
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  #facet_grid(Methodology ~ ., scales = "free_y", space = "free_y")+
  labs(x = expression(paste("Tree BGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       y = "",title = "[VM0007] VCS1112",
       fill = "")+theme(axis.text.x = element_text(size=14, vjust = 1, hjust=0.5),
                        axis.text.y = element_text(size = 14), 
                        axis.title = element_text(size = 17),
                        plot.title = element_text(size = 17, face="bold"),
                        legend.text = element_text(size=16),
                        legend.position = c(0.7, 0.15),
                        legend.background = element_rect(fill='transparent'))+
  theme(strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="darkgrey", fill="white"))
p_1112_bgb

ggsave(filename = "BGB_1112_updated.png",
       plot = p_1112_bgb_v2, width = 10, height = 8, units = 'cm',
       scale = 2, dpi = 1200)

# BGB plots so far

p_BGB <- p_1392_bgb / p_1112_bgb / p_1775_bgb_v2  + plot_layout(ncol=1) +plot_annotation(tag_levels = 'a')& 
  theme(plot.tag = element_text(size = 18,face="bold"),plot.tag.position =c(0.1,1))
p_BGB

ggsave(filename = "BGB_vm6_7_9.png",
       plot = p_BGB, width = 10, height = 15, units = 'cm',
       scale = 2, dpi = 1200)

# END####
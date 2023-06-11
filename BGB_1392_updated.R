
## BGB plotting ##

library(ggplot2)
library(scales)

# VM0006 ####

# Project 1392 - data from script VM0006_SA

VCS1392_AGB_Data2 <- rbind(#Wet_AGBPlot_proj_1392,
  p1392_er,
  p1392_er_Chr5,
  p1392_er_alt1.1a,
  p1392_er_alt2.1a,
  p1392_er_alt3,
  p1392_er_alt4,
  p1392_er_alt5)
VCS1392_AGB_Data2
str(VCS1392_AGB_Data2)

vcs1392_toplot$Project_ID = as.factor(vcs1392_toplot$Project_ID)
vcs1392_toplot$EquationType = as.factor(vcs1392_toplot$EquationType)

#Calcularing tCO2e/ha
vcs1392_toplot$AGC_tCO2_ha = as.numeric(vcs1392_toplot$mAGB_MgC_ha * (44/12))
vcs1392_toplot$HCI_tCO2_ha = as.numeric(vcs1392_toplot$HCI * (44/12))
vcs1392_toplot$LCI_tCO2_ha = as.numeric(vcs1392_toplot$LCI * (44/12))

vcs1392_toplot

# Red dot - Project's BGB method ####
# BGB = 0.489 * AGB^0.89

bgb_1392_proj <- vcs1392_toplot %>% filter(EquationType == "Project [3]")
bgb_1392_proj$BGB_tCO2_ha = 0.489 * (bgb_1392_proj$AGC_tCO2_ha)^0.89
str(bgb_1392_proj)
bgb_1392_proj$BGB_EquationType = rep("Project-eq")
names(bgb_1392_proj)
bgb_1392_proj

# alternative project

bgb_1392_proj2 <- bgc_all_to_plot %>% 
  filter(condition == "Project") %>% 
  filter(Project_ID == "VCS1392")
bgb_1392_proj2

bgb_1392_proj2$BGB_tCO2_ha = 0.489 * (bgb_1392_proj2$MeanAGC_tCO2_ha)^0.89
str(bgb_1392_proj2)

bgb_1392_proj2$BGB_EquationType = rep("Project-rsr")
names(bgb_1392_proj2)
bgb_1392_proj2

# Blue Dot - Project BGB * Alternative AGB ####

bgb_1392_proj3 <- bgc_all_to_plot %>% 
  filter(condition == "Project") %>% 
  filter(Project_ID == "VCS1392")
bgb_1392_proj3

bgb_1392_proj3$BGB_tCO2_ha = 0.489 * (bgb_1392_alt1.1$MeanAGC_tCO2_ha)^0.89 #this is the bgb method used by Mugasha 2013 v1

bgb_1392_proj3$BGB_EquationType = rep("Project_alt")
names(bgb_1392_proj3)
bgb_1392_proj3

# Gray dots - Alternative BGB with Alternative AGB ####

# Alt 1 BGB ####
# BGB (Mg/ha) = exp (-1.0587 + 0.8836 x ln AGB (Mg/ha)) Cairns et al 1997

bgb_1392_alt <- vcs1392_toplot %>% filter(EquationType!="Project [3]")
bgb_1392_alt$BGB_tCO2_ha = exp(-1.0587 + 0.8836 * log(bgb_1392_alt$AGC_tCO2_ha))

bgb_1392_alt$BGB_EquationType = rep("Cairns (1997)-eq")
names(bgb_1392_alt)

bgb_1392_alt

p_bgb_1392_alt <- data.frame(bgb_1392_alt %>%
                               summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha)),
                             Project_ID = rep("VCS1392"),
                             BGB_EquationType = rep("Cairns (1997)-eq"))
p_bgb_1392_alt

# Combining data frames

bgb_1392_proj_toplot <- bgb_1392_proj[,c('BGB_tCO2_ha', 'Project_ID', 'BGB_EquationType')]
bgb_1392_proj_toplot

bgb_1392 <- rbind(bgb_1392_proj_toplot, p_bgb_1392_alt)
str(bgb_1392)
bgb_1392

# alternative Alt1

bgb_1392_alt1.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1392")
bgb_1392_alt1.1

bgb_1392_alt1.1$BGB_tCO2_ha = exp(-1.0587 + 0.8836 * log(bgb_1392_alt1.1$MeanAGC_tCO2_ha))

bgb_1392_alt1.1$BGB_EquationType = rep("Cairns (1997)-eq")

bgb_1392_alt1.1

# Alt 2 BGB ####
#0.235 R:S ratio (Mokany et al 2006)##

bgb_1392_alt$BGB_tCO2_ha_1 = as.numeric(bgb_1392_alt$AGC_tCO2_ha) * 0.235 #this is the bgb method used by Mugasha 2013 v1
bgb_1392_alt$BGB_EquationType = rep("Mokany (2006)-rsr")
names(bgb_1392_alt)
bgb_1392_alt

BGBPlot_alt2_1392<-  data.frame (BGB_tCO2_ha = bgb_1392_alt$AGC_tCO2_ha * 0.235,
                                 Project_ID = rep("VCS1392"),
                                 BGB_EquationType = rep("Mokany (2006)-rsr"))
BGBPlot_alt2_1392

p_bgb_1392_alt2 <- data.frame(BGBPlot_alt2_1392 %>%
                                summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha)),
                              Project_ID = rep("VCS1392"),
                              BGB_EquationType = rep("Mokany (2006)-rsr"))
p_bgb_1392_alt2

# alternative

bgb_1392_alt2.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1392")
bgb_1392_alt2.1

bgb_1392_alt2.1$BGB_tCO2_ha = bgb_1392_alt2.1$MeanAGC_tCO2_ha * 0.235
bgb_1392_alt2.1$BGB_EquationType = rep("Mokany (2006)-rsr")
bgb_1392_alt2.1

# Alt 3 BGB ####
#0.2 R:S ratio (Vogt et al 2006)##

BGBPlot_alt3_1392<-  data.frame (BGB_tCO2_ha = bgb_1392_alt$AGC_tCO2_ha * 0.2,
                                 Project_ID = rep("VCS1392"),
                                 BGB_EquationType = rep("Vogt (1996)-rsr"))
BGBPlot_alt3_1392

p_bgb_1392_alt3 <- data.frame(BGBPlot_alt3_1392 %>%
                                summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha)),
                              Project_ID = rep("VCS1392"),
                              BGB_EquationType = rep("Vogt (1996)-rsr"))
p_bgb_1392_alt3

# alternative

bgb_1392_alt3.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1392")
bgb_1392_alt3.1

bgb_1392_alt3.1$BGB_tCO2_ha = bgb_1392_alt3.1$MeanAGC_tCO2_ha * 0.2
bgb_1392_alt3.1$BGB_EquationType = rep("Vogt (1996)-rsr")
bgb_1392_alt3.1

# combining data frames

VCS1392_BGB_Data <- rbind(bgb_1392_proj2,
                          bgb_1392_proj3,
                          bgb_1392_alt1.1,
                          bgb_1392_alt2.1, 
                          bgb_1392_alt3.1)
VCS1392_BGB_Data

vcs1392_toplot2 <- VCS1392_BGB_Data
str(vcs1392_toplot2)

vcs1392_toplot2$Project_ID = as.factor(vcs1392_toplot2$Project_ID)
vcs1392_toplot2$BGB_EquationType = as.factor(vcs1392_toplot2$BGB_EquationType)

names(vcs1392_toplot2)

# Combining data frames to summarize and plot ####

vcs1392_bgb_final <- vcs1392_toplot2
str(vcs1392_bgb_final)

vcs1392_bgb_final

vcs1392_bgb_final$condition2 <- as.factor(c(rep("Proj-BGB * Proj-AGB",1),
                                            rep("Proj-BGB * Alt-AGB",1),
                                            rep("Alt-BGB * Alt-AGB",3)))

levels(as.factor(vcs1392_bgb_final$condition2))
vcs1392_bgb_final

vcs1392_bgb_final$condition3 <- as.factor(c(rep("Project BGC equation (using the project AGC)",1),
                                            rep("Project BGC equation (using our alternative AGC)",1),
                                            rep("Alternative BGC equations (using our alternative AGC)",3)))

levels(as.factor(vcs1392_bgb_final$condition3))
vcs1392_bgb_final

vcs1392_bgb_final$BGB_EquationType2 <- as.factor(c(rep("Proj-BGC * Proj-AGC",1),
                                                   rep("Proj-BGC *Alt-AGC",1),
                                                   rep("Cairns (1997)",1),
                                                   rep("Mokany (2006)",1),
                                                   rep("Vogt (1996)",1)))

levels(as.factor(vcs1392_bgb_final$BGB_EquationType2))
vcs1392_bgb_final

## Plot  VCS1392 #######

p_1392_bgb_v2 <- ggplot(vcs1392_bgb_final, aes(y = reorder(BGB_EquationType2,BGB_tCO2_ha), x = BGB_tCO2_ha)) +
  geom_segment(aes(yend = BGB_EquationType2), xend = 0, colour = "#B1A9A9") +
  geom_point(size = 3, aes(fill = condition3), shape = 21, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("darkgrey", "blue", "red"), 
                    limits = c("Alternative BGC equations (using our alternative AGC)",
                               "Project BGC equation (using our alternative AGC)",
                               "Project BGC equation (using the project AGC)")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  #facet_grid(Methodology ~ ., scales = "free_y", space = "free_y")+
  labs(x = expression(paste("Tree belowground carbon stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       y = "",title = "[VM0006] VCS1392",
       fill = "")+theme(axis.text.x = element_text(size=14, vjust = 1, hjust=0.5),
                        axis.text.y = element_text(size = 14), 
                        axis.title = element_text(size = 15),
                        plot.title = element_text(size = 17, face="bold"),
                        legend.text = element_text(size=14),
                        legend.position = "none",
                        legend.background = element_rect(fill='transparent'))+
  theme(strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="darkgrey", fill="white"))
p_1392_bgb_v2

p_1392_bgb <- ggplot(vcs1392_bgb_final, aes(y = reorder(BGB_EquationType,BGB_tCO2_ha), x = BGB_tCO2_ha)) +
  geom_segment(aes(yend = BGB_EquationType), xend = 0, colour = "#B1A9A9") +
  geom_point(size = 3, aes(fill = condition), shape = 21, color = "black") +
  scale_fill_manual(values = c("darkgrey", "red"), limits = c("Alternative","Project")) +#palette = "Set1",
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  #facet_grid(Methodology ~ ., scales = "free_y", space = "free_y")+
  labs(x = expression(paste("Tree BGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       y = "",title = "[VM0006] VCS1392",
       fill = "")+theme(axis.text.x = element_text(size=14, vjust = 1, hjust=0.5),
                        axis.text.y = element_text(size = 14), 
                        axis.title = element_text(size = 15),
                        plot.title = element_text(size = 17, face="bold"),
                        legend.text = element_text(size=16),
                        legend.position = c(0.7, 0.15),
                        legend.background = element_rect(fill='transparent'))+
  theme(strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="darkgrey", fill="white"))
p_1392_bgb

ggsave(filename = "BGB_1392_updated.png",
       plot = p_1392_bgb_v2, width = 10, height = 8, units = 'cm',
       scale = 2, dpi = 1200)

# Both plots so far

p_BGB <- p_1392_bgb / p_1775_bgb_v2 + plot_layout(ncol=1) +plot_annotation(tag_levels = 'a')& 
  theme(plot.tag = element_text(size = 18,face="bold"),plot.tag.position =c(0.1,1))
p_BGB

ggsave(filename = "BGB_vm6_9.png",
       plot = p_BGB, width = 10, height = 12, units = 'cm',
       scale = 2, dpi = 1200)

# END####
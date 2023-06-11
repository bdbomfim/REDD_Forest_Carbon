
## BGB plotting ##

library(ggplot2)
library(scales)
library(patchwork)
library(tidyverse)
library(gridExtra)

# VM0015 ####

# Project 1541 data from VM0015_SA script

VCS1541_AGB_Data <- rbind(p1541_er, 
                           p1541_r5,
                           p1541_pr,
                           p1541_alt1,
                           p1541_alt2,
                           p1541_alt3,
                           p1541_alt4)
VCS1541_AGB_Data 

vcs1541_toplot<-VCS1541_AGB_Data

vcs1541_toplot$Project_ID = as.factor(vcs1541_toplot$Project_ID)
vcs1541_toplot$EquationType = as.factor(vcs1541_toplot$EquationType)

#Calcularing tCO2e/ha
vcs1541_toplot$AGC_tCO2_ha = as.numeric(vcs1541_toplot$mAGB_MgC_ha * (44/12))
vcs1541_toplot$HCI_tCO2_ha = as.numeric(vcs1541_toplot$HCI * (44/12))
vcs1541_toplot$LCI_tCO2_ha = as.numeric(vcs1541_toplot$LCI * (44/12))

vcs1541_toplot

# Project's BGB method ####
# BGB (t/ha) = exp (-1.085 + 0.9256 * log(AGB t/ha))

bgb_1541_proj <- vcs1541_toplot %>% filter(EquationType == "Project [4]")
bgb_1541_proj

bgb_1541_proj$BGB_tCO2_ha = (bgb_1541_proj$AGC_tCO2_ha) * 0.24
str(bgb_1541_proj)

bgb_1541_proj$BGB_EquationType = rep("Project-rsr")
names(bgb_1541_proj)
bgb_1541_proj

bgc_all_to_plot

# Red dot - project BGB * Project AGB ####

bgb_1541_proj2 <- bgc_all_to_plot %>% 
  filter(condition == "Project") %>% 
  filter(Project_ID == "VCS1541")
bgb_1541_proj2

bgb_1541_proj2$BGB_tCO2_ha = (bgb_1541_proj2$MeanAGC_tCO2_ha) * 0.24
str(bgb_1541_proj2)

bgb_1541_proj2$BGB_EquationType = rep("Project-rsr")
names(bgb_1541_proj2)
bgb_1541_proj2

# Blue Dot - Project BGB * Alternative AGB ####

bgb_1541_proj3 <- bgc_all_to_plot %>% 
  filter(condition == "Project") %>% 
  filter(Project_ID == "VCS1541")
bgb_1541_proj3

bgb_1541_proj3$BGB_tCO2_ha = as.numeric(bgb_1541_alt1.1$MeanAGC_tCO2_ha) * 0.24 #this is the bgb method used by Mugasha 2013 v1

bgb_1541_proj3$BGB_EquationType = rep("Project_alt")
names(bgb_1541_proj3)
bgb_1541_proj3

# Gray dots - Alternative BGB * Alternative AGB ####

# Data frame for alternative methods
bgb_1541_alt <- vcs1541_toplot %>% filter(EquationType!="Project [4]")
bgb_1541_alt

# Alt 1 BGB ####
# BGB (t/ha) = exp (-1.0587 + 0.9256 * log(AGB t/ha)) (Cairns et al 1997)##

BGBPlot_alt1_1541<-  data.frame (BGB_tCO2_ha = exp(-1.0587 + 0.9256 * log(bgb_1541_alt$AGC_tCO2_ha)),
                                 Project_ID = rep("VCS1541"),
                                 BGB_EquationType = rep("Cairns (1997)-eq"))
BGBPlot_alt1_1541

p_bgb_1541_alt1 <- data.frame(BGBPlot_alt1_1541 %>%
                                summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha)),
                              Project_ID = rep("VCS1541"),
                              BGB_EquationType = rep("Cairns (1997)-eq"))
p_bgb_1541_alt1

# using the mean of the best alternatives in data frame bgc_all_to_plot

bgb_1541_alt1.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1541")
bgb_1541_alt1.1

bgb_1541_alt1.1$BGB_tCO2_ha = exp(-1.0587 + 0.9256 * log(bgb_1541_alt1.1$MeanAGC_tCO2_ha))

bgb_1541_alt1.1$BGB_EquationType = rep("Cairns (1997)-eq")

bgb_1541_alt1.1

# Alt 2 BGB ####
#0.28 R:S ratio (Ledo et al 2018)##
summary(bgb_1541_alt$AGC_tCO2_ha)
summary(bgb_1541_alt)
bgb_1541_alt

BGBPlot_alt2_1541 <-  data.frame (BGB_tCO2_ha = bgb_1541_alt$AGC_tCO2_ha * 0.28,
                                 Project_ID = rep("VCS1541"),
                                 BGB_EquationType = rep("Ledo (2018)-rsr"))
BGBPlot_alt2_1541

p_bgb_1541_alt2 <- data.frame(BGBPlot_alt2_1541 %>%
                               summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha)),
                             Project_ID = rep("VCS1541"),
                             BGB_EquationType = rep("Ledo (2018)-rsr"))
p_bgb_1541_alt2

# using the mean AGC of best alternatives

bgb_1541_alt2.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1541")
bgb_1541_alt2.1

bgb_1541_alt2.1$BGB_tCO2_ha = bgb_1541_alt2.1$MeanAGC_tCO2_ha * 0.28

bgb_1541_alt2.1$BGB_EquationType = rep("Ledo (2018)-rsr")

bgb_1541_alt2.1

# Alt 3 BGB ####
#0.2 R:S ratio (Vogt et al 1996)##

BGBPlot_alt3_1541<-  data.frame (BGB_tCO2_ha = bgb_1541_alt$AGC_tCO2_ha * 0.2,
                                 Project_ID = rep("VCS1541"),
                                 BGB_EquationType = rep("Vogt (1996)-rsr"))
BGBPlot_alt3_1541

p_bgb_1541_alt3 <- data.frame(BGBPlot_alt3_1541 %>%
                                summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha)),
                              Project_ID = rep("VCS1541"),
                              BGB_EquationType = rep("Vogt (1996)-rsr"))
p_bgb_1541_alt3

# using mean AGC of best alternative equations

bgb_1541_alt3.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1541")
bgb_1541_alt3.1

bgb_1541_alt3.1$BGB_tCO2_ha = bgb_1541_alt3.1$MeanAGC_tCO2_ha * 0.2

bgb_1541_alt3.1$BGB_EquationType = rep("Vogt (1996)-rsr")

bgb_1541_alt3.1

# Combining data frames
bgb_1541_proj2
bgb_1541_proj3

VCS1541_BGB_Data <- rbind(bgb_1541_proj2,
                          bgb_1541_proj3,
                          bgb_1541_alt1.1,
                          bgb_1541_alt2.1, 
                          bgb_1541_alt3.1)
VCS1541_BGB_Data

vcs1541_toplot2 <- VCS1541_BGB_Data
vcs1541_toplot2$BGB_EquationType = as.factor(vcs1541_toplot2$BGB_EquationType)
vcs1541_toplot2$Project_ID = as.factor(vcs1541_toplot2$Project_ID)

str(vcs1541_toplot2)

names(vcs1541_toplot2)

# Combining data frames to summarize and plot ####
vcs1541_toplot2
vcs1541_bgb_final <- vcs1541_toplot2
str(vcs1541_bgb_final)
vcs1541_bgb_final

vcs1541_bgb_final$condition2 <- as.factor(c(rep("Proj-BGB * Proj-AGB",1),
                                            rep("Proj-BGB * Alt-AGB",1),
                                            rep("Alt-BGB * Alt-AGB",3)))

levels(as.factor(vcs1541_bgb_final$condition2))
vcs1541_bgb_final

vcs1541_bgb_final$condition3 <- as.factor(c(rep("Project BGC equation (using the project AGC)",1),
                                            rep("Project BGC equation (using our alternative AGC)",1),
                                            rep("Alternative BGC equations (using our alternative AGC)",3)))

levels(as.factor(vcs1541_bgb_final$condition3))
vcs1541_bgb_final

vcs1541_bgb_final

vcs1541_bgb_final$BGB_EquationType2 <- as.factor(c(rep("Proj-BGC * Proj-AGC",1),
                                            rep("Proj-BGC *Alt-AGC",1),
                                            rep("Cairns (1997)",1),
                                            rep("Ledo (2018)",1),
                                            rep("Vogt (1996)",1)))

levels(as.factor(vcs1541_bgb_final$BGB_EquationType2))
vcs1541_bgb_final

## Plot  VCS541 #######

p_1541_bgb_v2 <- ggplot(vcs1541_bgb_final, aes(y = reorder(BGB_EquationType2,BGB_tCO2_ha), x = BGB_tCO2_ha)) +
  geom_segment(aes(yend = BGB_EquationType2), xend = 0, colour = "#B1A9A9") +
  geom_point(size = 3, aes(fill = condition3), shape = 21, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("darkgrey", "blue", "red"), 
                    limits = c("Alternative BGC equations (using our alternative AGC)",
                               "Project BGC equation (using our alternative AGC)",
                               "Project BGC equation (using the project AGC)")) +#palette = "Set1",
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  #facet_grid(Methodology ~ ., scales = "free_y", space = "free_y")+
  labs(x = expression(paste("Tree belowground carbon stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       y = "",title = "[VM0015] VCS1541",
       fill = "")+theme(axis.text.x = element_text(size=14, vjust = 1, hjust=0.5),
                        axis.text.y = element_text(size = 14), 
                        axis.title = element_text(size = 15),
                        plot.title = element_text(size = 17, face="bold"),
                        legend.text = element_text(size=16),
                        legend.position = "none",
                        legend.direction = "vertical",
                        legend.background = element_rect(fill='transparent'))+
  theme(strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="darkgrey", fill="white"))
p_1541_bgb_v2

p_1541_bgb <- ggplot(vcs1541_bgb_final, aes(y = reorder(BGB_EquationType,BGB_tCO2_ha), x = BGB_tCO2_ha)) +
  geom_segment(aes(yend = BGB_EquationType2), xend = 0, colour = "#B1A9A9") +
  geom_point(size = 3, aes(fill = condition2), shape = 21, color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("darkgrey", "blue", "red"), 
                    limits = c("Alt-BGB * Alt-AGB","Proj-BGB * Alt-AGB","Proj-BGB * Proj-AGB")) +#palette = "Set1",
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  #facet_grid(Methodology ~ ., scales = "free_y", space = "free_y")+
  labs(x = expression(paste("Tree belowground carbon stock ", "(", tCO[2], sep = " ", ha^-1,")")),
       y = "",title = "[VM0015] VCS1541",
       fill = "")+theme(axis.text.x = element_text(size=14, vjust = 1, hjust=0.5),
                        axis.text.y = element_text(size = 14), 
                        axis.title = element_text(size = 15),
                        plot.title = element_text(size = 17, face="bold"),
                        legend.text = element_text(size=18),
                        legend.position = c(0.7, 0.15),
                        legend.background = element_rect(fill='transparent'))+
  theme(strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="darkgrey", fill="white"))
p_1541_bgb

ggsave(filename = "BGB_1541_updated.png",
       plot = p_1541_bgb_v2, width = 10, height = 8, units = 'cm',
       scale = 2, dpi = 1200)

ggsave(filename = "BGB_1541_vmean_best_equations.png",
       plot = p_1541_bgb, width = 10, height = 8, units = 'cm',
       scale = 2, dpi = 1200)

# All BGB plots

p_BGB_v3 <- (p_1392_bgb_v2 + p_1112_bgb_v2) / (p_1775_bgb_v2 + p_1541_bgb_v2) + guide_area() +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a')& 
  theme(plot.tag = element_text(size = 18,face="bold"),plot.tag.position =c(0.3,0.95))
p_BGB_v3

p_BGB_v2 <- p_1392_bgb_v2 / p_1112_bgb_v2 / p_1775_bgb_v2 / p_1541_bgb_v2 + guide_area() + 
  plot_layout(ncol=2) + 
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a')& 
  theme(plot.tag = element_text(size = 18,face="bold"),plot.tag.position =c(0.3,0.95))
p_BGB_v2

ggsave(filename = "BGB_all_final.png",
       plot = p_BGB_v2, width = 16, height = 14, units = 'cm',
       scale = 2, dpi = 1200)

# Create user-defined function, which extracts legends from ggplots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# Apply user-defined function to extract legend
shared_legend <- extract_legend(p_1541_bgb_v2)

p_BGB_v3 <- grid.arrange(arrangeGrob(p_1392_bgb_v2, p_1112_bgb_v2, p_1775_bgb_v2, p_1541_bgb_v2, ncol = 2),
             shared_legend, nrow = 2, heights = c(10, 1))
p_BGB_v3

ggsave(filename = "BGB_all_final.png",
       plot = p_BGB_v3, width = 17, height = 14, units = 'cm',
       scale = 2, dpi = 1200)

# END####
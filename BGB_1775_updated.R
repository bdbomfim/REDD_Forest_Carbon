
## BGB plotting ##

library(ggplot2)
library(scales)

# VM0009 ####

# Project 1775 - alternatives and project equation

VCS1775_AGB_Data4_agb <- rbind(p1775_er_l65,
                           p1775_pr,
                           p1775_r5_l65,
                           p1775_alt1_l65,
                           p1775_alt2_l65,
                           p1775_alt3.1_l65,
                           p1775_alt4_l65,
                           p1775_alt5_l65)
VCS1775_AGB_Data4_agb
str(VCS1775_AGB_Data4_agb)

vcs1775_toplot <-VCS1775_AGB_Data4_agb 
vcs1775_toplot$Project_ID = as.factor(vcs1775_toplot$Project_ID)
vcs1775_toplot$EquationType = as.factor(vcs1775_toplot$EquationType)

#Calcularing tCO2e/ha
vcs1775_toplot$AGC_tCO2_ha = as.numeric(vcs1775_toplot$mAGB_MgC_ha * (44/12))
vcs1775_toplot$HCI_tCO2_ha = as.numeric(vcs1775_toplot$HCI * (44/12))
vcs1775_toplot$LCI_tCO2_ha = as.numeric(vcs1775_toplot$LCI * (44/12))

vcs1775_toplot

## Red dot - project BGB with project AGB ####

# Filtering the project and applying project's BGB calculation method
bgb_1775_proj <- vcs1775_toplot %>% filter(EquationType == "Project [4]")
bgb_1775_proj

bgb_1775_proj$BGB_tCO2_ha = as.numeric(bgb_1775_proj$AGC_tCO2_ha) * 0.54 #this is the bgb method used by the project
bgb_1775_proj$BGB_HCI_tCO2_ha = as.numeric(bgb_1775_proj$HCI_tCO2_ha) * 0.54
bgb_1775_proj$BGB_LCI_tCO2_ha = as.numeric(bgb_1775_proj$LCI_tCO2_ha) * 0.54

# alternative project
bgc_all_to_plot
bgb_1775_proj2 <- bgc_all_to_plot %>% 
  filter(condition == "Project") %>% 
  filter(Project_ID == "VCS1775")
bgb_1775_proj2

bgb_1775_proj2$BGB_tCO2_ha = bgb_1775_proj2$MeanAGC_tCO2_ha * 0.54
str(bgb_1775_proj2)

bgb_1775_proj2$BGB_EquationType = rep("Project_pr")
names(bgb_1775_proj2)
bgb_1775_proj2

# Blue dot - project BGB with mean of alternative AGB ####
bgb_1775_proj3 <- bgc_all_to_plot %>% 
  filter(condition == "Project") %>% 
  filter(Project_ID == "VCS1775")
bgb_1775_proj3

bgb_1775_proj3$BGB_tCO2_ha = as.numeric(bgb_1775_alt1.1$MeanAGC_tCO2_ha) * 0.54 #this is the bgb method used by Mugasha 2013 v1

bgb_1775_proj3$BGB_EquationType = rep("Project_alt")
names(bgb_1775_proj3)
bgb_1775_proj3

# Checking the data frame
bgb_1775_proj2

# Gray dot - alternative BGB with alternative AGB ####

# Filtering OUT the project AGB TO CALCULATE ALTERNATIVE BGB estimates
bgb_1775_alt <- vcs1775_toplot %>% filter(EquationType!="Project [4]")
bgb_1775_alt

# BGB Alt 1 ####

bgb_1775_alt$BGB_tCO2_ha_1 = as.numeric(bgb_1775_alt$AGC_tCO2_ha) * 0.40 #this is the bgb method used by Mugasha 2013 v1
bgb_1775_alt$BGB_HCI_tCO2_ha_1 = as.numeric(bgb_1775_alt$HCI_tCO2_ha) * 0.40
bgb_1775_alt$BGB_LCI_tCO2_ha_1 = as.numeric(bgb_1775_alt$LCI_tCO2_ha) * 0.40
bgb_1775_alt$BGB_EquationType = rep("Mugasha (2013)-rsr")
names(bgb_1775_alt)
bgb_1775_alt

# Summarizing data and saving in new data frame
p_BGBPlot_alt1_1775 <- data.frame(bgb_1775_alt %>%
                                    summarise(BGB_tCO2_ha = mean(BGB_tCO2_ha_1),
                                              se = sd(BGB_tCO2_ha_1)/sqrt(n()),
                                              BGB_HCI_tCO2_ha = BGB_tCO2_ha + (1.96*se), 
                                              BGB_LCI_tCO2_ha = BGB_tCO2_ha - (1.96*se),
                                              N = n()), 
                                  Project_ID = rep("VCS1775"),
                                  BGB_EquationType = rep("Mugasha (2013)-rsr"))
p_BGBPlot_alt1_1775

# alternative Alt 1.1

bgb_1775_alt1.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1775")
bgb_1775_alt1.1

bgb_1775_alt1.1$BGB_tCO2_ha = bgb_1775_alt1.1$MeanAGC_tCO2_ha * 0.4

bgb_1775_alt1.1$BGB_EquationType = rep("Mugasha (2013)-rsr")

bgb_1775_alt1.1

# Combining data frames to plot
bgb_1775_proj
bgb_1775_proj_toplot <- bgb_1775_proj[,c('BGB_tCO2_ha','BGB_HCI_tCO2_ha', 
                                         'BGB_LCI_tCO2_ha', 'Project_ID', 'BGB_EquationType')]
bgb_1775_proj_toplot

p_BGBPlot_alt1_1775_toplot <- p_BGBPlot_alt1_1775[,c('BGB_tCO2_ha','BGB_HCI_tCO2_ha', 
                                         'BGB_LCI_tCO2_ha', 'Project_ID', 'BGB_EquationType')]
p_BGBPlot_alt1_1775_toplot

# Project and Alt 1 data frames ####
bgb_1775 <- rbind(bgb_1775_proj_toplot, p_BGBPlot_alt1_1775_toplot)
bgb_1775
str(bgb_1775)

# BGB Alt 2 ####
#BGB = 0.2113 * DBH^1.9838 (Mugasha et al 2013)

VCS1775_BGB_alt2 <- function(D) { # create a function with the name my_function
  BGB = (0.2113 * D^1.9838) * exp((0.43^2)/2)
  print(BGB) # this returns AGB in kg
} 
BGBPlot_alt2_1775 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                          function(x) VCS1775_BGB_alt2(D = x$D),
                          simplify = F)
BGBPlot_Alt2_1775 <- sapply(BGBPlot_alt2_1775, sum)
BGBPlot_Alt2_1775 # kg per 1 ha

BGBPlot_Alt2_1775_c <- sapply(BGBPlot_alt2_1775, length)

# Error 
BGB_MgC_ha_alt2_1775 = (BGBPlot_Alt2_1775/1000) * 0.47
BGB_MgC_ha_alt2_1775
SDBGB_alt2_1775 = BGB_MgC_ha_alt2_1775 * 0.20
SEBGB_alt2_1775 = SDBGB_alt2_1775/sqrt(BGBPlot_Alt2_1775_c)
BGBHighCI_alt2_1775 = BGB_MgC_ha_alt2_1775 + (1.96*SEBGB_alt2_1775)
BGBLowCI_alt2_1775 = BGB_MgC_ha_alt2_1775 - (1.96*SEBGB_alt2_1775)

BGBPlot_alt2_1775<-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                                   BGB_MgC_ha = BGB_MgC_ha_alt2_1775,
                                 BGB_HCI = BGBHighCI_alt2_1775,
                                 BGB_LCI = BGBLowCI_alt2_1775,
                                   Project_ID = rep("VCS1775"),
                                 BGB_EquationType = rep("Mugasha (2013)-eq"))
BGBPlot_alt2_1775

BGBPlot_alt2_1775_f <- BGBPlot_alt2_1775 %>% filter(BGB_MgC_ha > 0) 
BGBPlot_alt2_1775_f

p_BGBPlot_alt2_1775 <- data.frame(BGBPlot_alt2_1775_f %>%
                               summarise(mBGB_MgC_ha = mean(BGB_MgC_ha),
                                         se = sd(BGB_MgC_ha)/sqrt(n()),
                                         HCI = mBGB_MgC_ha + (1.96*se), 
                                         LCI = mBGB_MgC_ha - (1.96*se),
                                         N = n()), 
                             Project_ID = rep("VCS1775"),
                             BGB_EquationType = rep("Mugasha (2013)-eq"))
p_BGBPlot_alt2_1775

# alternative 2.1

bgb_1775_alt2.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1775")
bgb_1775_alt2.1

bgb_1775_alt2.1$BGB_tCO2_ha = p_BGBPlot_alt2_1775$mBGB_MgC_ha
bgb_1775_alt2.1$BGB_EquationType = rep("Mugasha (2013)-eq")
bgb_1775_alt2.1

# BGB Alt 3 #### 
#log(BGB) = 2.262*log(DBH)-3.370 (Ryan et al 2011)

VCS1775_BGB_alt3 <- function(D) { # create a function with the name my_function
  BGB = exp(2.262*log(D) - 3.370) * exp((0.43^2)/2)
  print(BGB) # this returns AGB in kg
} 
BGBPlot_alt3_1775 <- by(wd_1775_higher10_low65, wd_1775_higher10_low65$plotID,
                        function(x) VCS1775_BGB_alt3(D = x$D),
                        simplify = F)
BGBPlot_Alt3_1775 <- sapply(BGBPlot_alt3_1775, sum)
BGBPlot_Alt3_1775 # kg per 1 ha

BGBPlot_Alt3_1775_c <- sapply(BGBPlot_alt3_1775, length)

# Error 
BGB_MgC_ha_alt3_1775 = (BGBPlot_Alt3_1775/1000) * 0.47
BGB_MgC_ha_alt3_1775
SDBGB_alt3_1775 = BGB_MgC_ha_alt3_1775 * 0.20
SEBGB_alt3_1775 = SDBGB_alt3_1775/sqrt(BGBPlot_Alt3_1775_c)
BGBHighCI_alt3_1775 = BGB_MgC_ha_alt3_1775 + (1.96*SEBGB_alt3_1775)
BGBLowCI_alt3_1775 = BGB_MgC_ha_alt3_1775 - (1.96*SEBGB_alt3_1775)

BGBPlot_alt3_1775<-  data.frame (PlotID = unique(levels(wd_1775_higher10_low65$plotID)),
                                 BGB_MgC_ha = BGB_MgC_ha_alt3_1775,
                                 BGB_HCI = BGBHighCI_alt3_1775,
                                 BGB_LCI = BGBLowCI_alt3_1775,
                                 Project_ID = rep("VCS1775"),
                                 BGB_EquationType = rep("Ryan (2011)-eq"))
BGBPlot_alt3_1775

BGBPlot_alt3_1775_f <- BGBPlot_alt3_1775 %>% filter(BGB_MgC_ha > 0) 
BGBPlot_alt3_1775_f

p_BGBPlot_alt3_1775 <- data.frame(BGBPlot_alt3_1775_f %>%
                                    summarise(mBGB_MgC_ha = mean(BGB_MgC_ha),
                                              se = sd(BGB_MgC_ha)/sqrt(n()),
                                              HCI = mBGB_MgC_ha + (1.96*se), 
                                              LCI = mBGB_MgC_ha - (1.96*se),
                                              N = n()), 
                                  Project_ID = rep("VCS1775"),
                                  BGB_EquationType = rep("Ryan (2011)-eq"))
p_BGBPlot_alt3_1775

# alternative 3.1

bgb_1775_alt3.1 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1775")
bgb_1775_alt3.1

bgb_1775_alt3.1$BGB_tCO2_ha = p_BGBPlot_alt3_1775$mBGB_MgC_ha
bgb_1775_alt3.1$BGB_EquationType = rep("Ryan (2011)-eq")
bgb_1775_alt3.1

# alternative Alt 3.2 ## continue here! 

bgb_1775_alt3.2 <- bgc_all_to_plot %>% 
  filter(condition == "Alternative") %>% 
  filter(Project_ID == "VCS1775")
bgb_1775_alt3.2

bgb_1775_alt3.2$BGB_tCO2_ha = bgb_1775_alt3.2$MeanAGC_tCO2_ha * 0.42

bgb_1775_alt3.2$BGB_EquationType = rep("Ryan (2011)-rsr")

bgb_1775_alt3.2

# Combining data frames
bgb_1775_proj2
bgb_1775_proj3
bgb_1775_alt1.1
bgb_1775_alt2.1
bgb_1775_alt3.1

VCS1775_BGB_Data <- rbind(bgb_1775_proj2,
                          bgb_1775_proj3,
                          bgb_1775_alt1.1,
                          bgb_1775_alt2.1, 
                          bgb_1775_alt3.1,
                          bgb_1775_alt3.2)
str(VCS1775_BGB_Data)

vcs1775_toplot2 <- VCS1775_BGB_Data
vcs1775_toplot2$BGB_EquationType = as.factor(vcs1775_toplot2$BGB_EquationType)
vcs1775_toplot2$Project_ID = as.factor(vcs1775_toplot2$Project_ID)

str(vcs1775_toplot2)

names(vcs1775_toplot2)

# Combining data frames to summarize and plot ####

vcs1775_bgb_final <- vcs1775_toplot2
str(vcs1775_bgb_final)
vcs1775_bgb_final$condition2 <- as.factor(c(rep("Proj-BGB * Proj-AGB",1),
                                  rep("Proj-BGB * Alt-AGB",1),
                                  rep("Alt-BGB * Alt-AGB",4)))

levels(as.factor(vcs1775_bgb_final$condition2))
vcs1775_bgb_final

vcs1775_bgb_final$condition3 <- as.factor(c(rep("Project BGC equation (using the project AGC)",1),
                                            rep("Project BGC equation (using our alternative AGC)",1),
                                            rep("Alternative BGC equations (using our alternative AGC)",4)))

levels(as.factor(vcs1775_bgb_final$condition3))
vcs1775_bgb_final

vcs1775_bgb_final$BGB_EquationType2 <- as.factor(c(rep("Proj-BGC * Proj-AGC",1),
                                                   rep("Proj-BGC *Alt-AGC",1),
                                                   rep("Mugasha (2013)_rsr",1),
                                                   rep("Mugasha (2013)_eq",1),
                                                   rep("Ryan (2011)-eq",1),
                                                   rep("Ryan (2011)-rsr",1)))

levels(as.factor(vcs1775_bgb_final$BGB_EquationType2))
vcs1775_bgb_final

## Plot  VCS1775 ####### 

p_1775_bgb_v2 <- ggplot(vcs1775_bgb_final, aes(y = reorder(BGB_EquationType2,BGB_tCO2_ha), x = BGB_tCO2_ha)) +
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
       y = "",title = "[VM0009] VCS1775",
       fill = "")+theme(axis.text.x = element_text(size=14, vjust = 1, hjust=0.5),
                        axis.text.y = element_text(size = 14), 
                        axis.title = element_text(size = 15),
                        plot.title = element_text(size = 17, face="bold"),
                        legend.text = element_text(size=14),
                        legend.position = "none",
                        legend.background = element_rect(fill='transparent'))+
  theme(strip.text.y = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="darkgrey", fill="white"))
p_1775_bgb_v2

ggsave(filename = "BGB_1775_updated.png",
       plot = p_1775_bgb_v2, width = 10, height = 8, units = 'cm',
       scale = 2, dpi = 1200)

#p1775_bgb_toplot <- data.frame(vcs1775_bgb_final %>% group_by(BGB_EquationType) %>% summarise(m = mean(BGB_tCO2_ha),
                                                                                  # mHCI = mean(BGB_HCI_tCO2_ha), 
                                                                                  # mLCI = mean(BGB_LCI_tCO2_ha)))
p_1775_bgb <- ggplot(p1775_bgb_toplot, aes(x = reorder(BGB_EquationType, m), y = m, 
                                           ymin = mLCI, ymax = mHCI, color = BGB_EquationType)) +
  geom_pointrange(alpha=0.9,size=0.8, color = "red")+#ylim(0,110)+
  #scale_y_break(c(5, 30),scales=10)+
  labs(x = "", y = "Tree BGC stock (tCO2e/ha)",
       title = "VCS1775 (VM0009)", subtitle = "8 1-ha woodland plots")+theme_pubr()+ #scale_y_continuous(breaks=c(0, 300, 350, 400, 450, 500))+ 
  ggtitle("VCS1775 Zambia | VM0009")+ coord_flip()+
  theme(axis.text.x = element_text(size=12, angle = 0, vjust = 1, hjust=0.5))+
  theme(axis.text.y = element_text(size = 12), axis.title = element_text(size = 14),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.title = element_text(size = 18, face="bold"), axis.title.y = element_text(face="bold"))+
  gghighlight(BGB_EquationType == "Project", unhighlighted_params = list(colour = "#302828"))
p_1775_bgb

b_1775_bgb <- list(geom_hline(yintercept = 22.90, color = '#003f5c',alpha=0.4),
             annotate("rect", ymin = 18.21, ymax = 27.58,xmin = -Inf, xmax = Inf,alpha=0.1,linetype=2))

p_1775_bgb_final <- p_1775_bgb + geom_hline(aes(yintercept=22.90), lty=2, 
                                color = "gray", cex=1.2, alpha = 0.6)+b_1775_bgb
p_1775_bgb_final

ggsave(filename = "BGB_1775_v2.png",
       plot = p_1775_bgb_final, width = 8, height = 6, units = 'cm',
       scale = 2, dpi = 800)

# Segment Plot SAMPLE 
ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=make, 
                   xend=make, 
                   y=min(mileage), 
                   yend=max(mileage)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()

library(rstatix)
library(tidyverse)

## Combining all AGC data across projects

## New data frames ####
#vm6
data_1392<- vcs1392_toplot3
data_1392

#vm7
data_1112<- vcs1112_toplot3
data_1112

#vm9
data_1775<- vcs1775_toplot3 # full range of DBH, not included the project estimates yet
data_1775
data_1775_l65<- vcs1775_toplot4 # this for BGB
data_1775_l65

# vm15
data_1541<- vcs1541_toplot3
data_1541

# Combining data frames for BGC data analysis

bgc_all <- rbind(data_1392, data_1112, 
                 data_1775_l65, data_1541)
bgc_all

bgc_all$Methodology <- c(rep("VM0006",8),
                         rep("VM0007",8),
                         rep("VM0009",8),
                         rep("VM0015",7))
bgc_all

bgc_all$Project_ID = as.factor(bgc_all$Project_ID)
bgc_all$Methodology = as.factor(bgc_all$Methodology)

#### Data Stats by Project ####

## VM 6 ####

# 1392

proj_1392 <- data_1392 %>% filter(EquationType == "Project [3]")
proj_1392$AGC_tCO2_ha

data_1392$EquationType

## Analyzing BGB estimates

# combining data frames

VCS1392_BGB_Data <- rbind(bgb_1392_proj2,
                          bgb_1392_alt1.1,
                          bgb_1392_alt2.1, 
                          bgb_1392_alt3.1)
VCS1392_BGB_Data
str(VCS1392_BGB_Data)

# equation type as factor
VCS1392_BGB_Data$BGB_EquationType = as.factor(VCS1392_BGB_Data$BGB_EquationType)

# project BGC estimate
proj_1392_BGC <- VCS1392_BGB_Data %>% filter(condition == "Project")
proj_1392_BGC

# BGB range - SA withoug project estimate

range_1392_SA_BGB <- VCS1392_BGB_Data %>% filter(condition!= "Project") %>%
  summarise(meanBGC = mean(BGB_tCO2_ha), minBGC = min(BGB_tCO2_ha), maxBGC = max(BGB_tCO2_ha), 
            rel_diff = ((max(BGB_tCO2_ha) - min(BGB_tCO2_ha))/min(BGB_tCO2_ha)) * 100, 
             n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1392_SA_BGB # rel difference = 26.6% (max - min / min)

range_1392_Pr_Comp_BGB <- VCS1392_BGB_Data %>% 
  filter(condition!="Project") %>%
  summarise(meanBGC = mean(BGB_tCO2_ha),
             diff_proj_alt = ((proj_1392_BGC$BGB_tCO2_ha - mean(meanBGC))/mean(meanBGC)) * 100,
            n = n()) 
range_1392_Pr_Comp_BGB # diff_proj_alt = 42.9%

# Use the mean AGC which is 215.34 tCO2e/ha
range_1392_Pr_Comp$meanAGC

# VM 7 ####

# 1112

VCS1112_BGB_Data <- rbind(bgb_1112_proj2,
                          bgb_1112_alt1.1,
                          bgb_1112_alt2.1, 
                          bgb_1112_alt3.1)
VCS1112_BGB_Data

proj_1112_BGB <- VCS1112_BGB_Data %>% filter(condition == "Project")
proj_1112_BGB$BGB_tCO2_ha

data_1112$EquationType

# BGB range - SA withoug project estimate

range_1112_SA_BGB <- VCS1112_BGB_Data %>% filter(condition!= "Project") %>%
  summarise(meanBGC = mean(BGB_tCO2_ha), minBGC = min(BGB_tCO2_ha), maxBGC = max(BGB_tCO2_ha), 
            rel_diff = ((max(BGB_tCO2_ha) - min(BGB_tCO2_ha))/min(BGB_tCO2_ha)) * 100, 
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1112_SA_BGB # rel difference = 64.70% (max - min / min)

range_1112_Pr_Comp_BGB <- VCS1112_BGB_Data %>% 
  filter(condition!="Project") %>%
  summarise(meanBGC = mean(BGB_tCO2_ha),
            diff_proj_alt = ((proj_1392_BGC$BGB_tCO2_ha - mean(meanBGC))/mean(meanBGC)) * 100,
            n = n()) 
range_1112_Pr_Comp_BGB # diff_proj_alt = 65.9%

range_1112_Pr_Comp$meanAGC #206.6576 tCO2e/ha

# VM 9 ####

# 1775 

VCS1775_BGB_Data <- rbind(bgb_1775_proj2,
                          bgb_1775_alt1.1,
                          bgb_1775_alt2.1, 
                          bgb_1775_alt3.1,
                          bgb_1775_alt3.2)
str(VCS1775_BGB_Data)
VCS1775_BGB_Data

proj_1775_l65_BGB <- VCS1775_BGB_Data %>% filter(condition == "Project")
proj_1775_l65_BGB

# BGB range - SA without project estimate

range_1775_SA_BGB <- VCS1775_BGB_Data %>% filter(condition!= "Project") %>%
  summarise(meanBGC = mean(BGB_tCO2_ha), minBGC = min(BGB_tCO2_ha), maxBGC = max(BGB_tCO2_ha), 
            rel_diff = ((max(BGB_tCO2_ha) - min(BGB_tCO2_ha))/min(BGB_tCO2_ha)) * 100, 
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1775_SA_BGB # rel difference = 640% (max - min / min)

range_1775_Pr_Comp_BGB <- VCS1775_BGB_Data %>% 
  filter(condition!="Project") %>%
  summarise(meanBGC = mean(BGB_tCO2_ha),
            diff_proj_alt = ((proj_1775_l65_BGB$BGB_tCO2_ha - mean(meanBGC))/mean(meanBGC)) * 100,
            n = n()) 
range_1775_Pr_Comp_BGB # diff_proj_alt = 176.5% 

range_1775_Pr_Comp$meanAGC #60.06

# VM 15####

# 1541
VCS1541_BGB_Data <- rbind(bgb_1541_proj2,
                          bgb_1541_alt1.1,
                          bgb_1541_alt2.1, 
                          bgb_1541_alt3.1)
VCS1541_BGB_Data

proj_1541_BGB <- VCS1541_BGB_Data %>% filter(condition == "Project")
proj_1541_BGB$BGB_tCO2_ha

# BGB range - SA without project estimate

range_1541_SA_BGB <- VCS1541_BGB_Data %>% filter(condition!= "Project") %>%
  summarise(meanBGC = mean(BGB_tCO2_ha), minBGC = min(BGB_tCO2_ha), maxBGC = max(BGB_tCO2_ha), 
            rel_diff = ((max(BGB_tCO2_ha) - min(BGB_tCO2_ha))/min(BGB_tCO2_ha)) * 100, 
            n = n())#diff_proj_alt_1392 = ((proj_1392$AGC_tCO2_ha - mean(AGC_tCO2_ha))/mean(AGC_tCO2_ha)) * 100) 
range_1541_SA_BGB # rel difference = 40% (max - min / min)

range_1541_Pr_Comp_BGB <- VCS1541_BGB_Data %>% 
  filter(condition!="Project") %>%
  summarise(meanBGC = mean(BGB_tCO2_ha),
            diff_proj_alt = ((proj_1541_BGB$BGB_tCO2_ha - mean(meanBGC))/mean(meanBGC)) * 100,
            n = n()) 
range_1541_Pr_Comp_BGB # diff_proj_alt = -3.34%

range_1541_Pr_Comp$meanAGC #449.75

## Overall means across methodologies ####

# mean of best alternative equations
alt_mean_bgc <- as.data.frame(rbind(range_1392_Pr_Comp$meanAGC, 
                      range_1112_Pr_Comp$meanAGC, 
                      range_1775_Pr_Comp$meanAGC, 
                      range_1541_Pr_Comp$meanAGC))
alt_mean_bgc

alt_mean_bgc$Methodology <- c("VM0006", "VM0007", "VM0009", "VM0015")

alt_mean_bgc$Project_ID <- c("VCS1392", "VCS1112", "VCS1775", "VCS1541")

alt_mean_bgc <- alt_mean_bgc %>% rename("MeanAGC_tCO2_ha" = "V1")

alt_mean_bgc$condition <- rep("Alternative", 4)
alt_mean_bgc
summary(alt_mean_bgc)

# project BGC estimates
proj_mean_bgc <- as.data.frame(rbind(range_1392_Pr_Comp_BGB$diff_proj_alt, 
                                    range_1112_Pr_Comp_BGB$diff_proj_alt, 
                                    range_1775_Pr_Comp_BGB$diff_proj_alt, 
                                    range_1541_Pr_Comp_BGB$diff_proj_alt))
proj_mean_bgc

# OVERALL MEAN DIFFER PROJ ALTERNATIVES

mean_vmall_Pr_Comp <- (range_1541_Pr_Comp_BGB$diff_proj_alt  + 
                         range_1112_Pr_Comp_BGB$diff_proj_alt + 
                         range_1775_Pr_Comp_BGB$diff_proj_alt +
                         range_1392_Pr_Comp_BGB$diff_proj_alt)/4
mean_vmall_Pr_Comp #overall mean 76.5%

mean_vmall_SA_BGB <- (range_1392_SA_BGB$rel_diff  + 
                        range_1112_SA_BGB$rel_diff + 
                        range_1775_SA_BGB$rel_diff +
                        range_1541_SA_BGB$rel_diff)/4
mean_vmall_SA_BGB #193%

proj_mean_agc$Methodology <- c("VM0006", "VM0007", "VM0009", "VM0015")

proj_mean_agc$Project_ID <- c("VCS1392", "VCS1112", "VCS1775", "VCS1541")

proj_mean_agc <- proj_mean_agc %>% rename("MeanAGC_tCO2_ha" = "V1")

proj_mean_agc$condition <- rep("Project", 4)
proj_mean_agc

summary(proj_mean_agc)

## Plot project value vs mean of alternatives for all methodologies

## Data for plotting project comparisons #

bgc_all_to_plot <- rbind(alt_mean_bgc, proj_mean_agc)

str(bgc_all_to_plot)

bgc_all_to_plot$Methodology = as.factor(bgc_all_to_plot$Methodology)
bgc_all_to_plot$Project_ID = as.factor(bgc_all_to_plot$Project_ID)
bgc_all_to_plot$condition = as.factor(bgc_all_to_plot$condition)

bgc_all_to_plot

# Rename factor names

# https://r-graphics.org/recipe-bar-graph-dot-plot

# Get the names, sorted first by lg, then by avg
nameorder <- bgc_all_to_plot$ProjectID[order(bgc_all_to_plot$Methodology, bgc_all_to_plot$MeanAGC)]
nameorder2 <- bgc_all_to_plot$Project_ID[order(bgc_all_to_plot$Methodology)]

# Turn name into a factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels = nameorder)
bgc_all_to_plot$Project_ID <- factor(bgc_all_to_plot$Project_ID, levels = nameorder2)

head(bgc_all_to_plot)
levels(vm_all_data_long$Methodology)

## Plot Fig 3 proj comparison ####
p_fig3 <- ggplot(vm_all_data_long, aes(x = AGC, y = ProjectID)) +
  geom_segment(aes(yend = ProjectID), xend = 0, colour = "#B1A9A9") +
  geom_point(size = 3, aes(fill = condition), shape = 21, color = "black") +
  scale_fill_manual(values = c("darkgrey", "red"), limits = c("Alternative", "Project")) +#palette = "Set1",
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(Methodology ~ ., scales = "free_y", space = "free_y")+
  labs(x = expression(paste("Tree AGC stock ", "(", tCO[2], sep = " ", ha^-1,")")),
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

ggsave(filename = "Fig3_v3.png",
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

install.packages("devtools")
devtools::install_github("valentinitnelav/plotbiomes")

library(plotbiomes)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(extrafont)
library(patchwork)
library(readr)

# Importing project biophysical data
Biophysical_Projects <- read_csv("data:/Biophysical_Projects.csv")
#View(Biophysical_Projects)

proj_biophys_data <- as.data.frame(Biophysical_Projects)# Biophysical_projects
str(proj_biophys_data)

proj_biophys_data$Project_ID = as.factor(proj_biophys_data$Project_ID)
bio_data <- proj_biophys_data %>% filter(Project_ID!="1113") %>% filter(Project_ID!="1399")%>% 
  filter(Project_ID!="612") %>% filter(Project_ID!="1650")
str(bio_data)

# Plotting

plot_1 <- ggplot() +
  # add biome polygons
  geom_polygon(data = Whittaker_biomes,
               aes(x    = temp_c,
                   y    = precp_cm,
                   fill = biome),
               # adjust polygon borders
               colour = "gray98",
               size   = 1) +
  theme_bw()
plot_1

plot_2 <- plot_1 +
  # fill the polygons with predefined colors
  scale_fill_manual(name   = "Whittaker biomes",
                    breaks = names(Ricklefs_colors),
                    labels = names(Ricklefs_colors),
                    values = Ricklefs_colors)
plot_2


# Plot with added points

p1<- whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = bio_data, 
             aes(x = MAT_C, 
                 y = MAP_cm), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()

p1

p2<- whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = bio_data, 
             aes(x = MAT_C, 
                 y = MAP_cm, shape = Methodology), 
             size   = 3,
             colour = "black", 
             fill   = "gray95",
             stroke = 1,
             alpha  = 0.65) +
  theme_classic() + theme(axis.title = element_text(size = 16),
                          axis.text.y = element_text(size = 14),
                          axis.text.x = element_text(size = 14))
p2

# Tag function
place_label <- function(label, size = 6, font = "bold",...) {
  annotate("text", label = label, x = -Inf, y = Inf, 
           hjust = -0.5, vjust = 1.2, size = size, ...)
}

p2 <- p2 + place_label("b")
p2

# saving high res

ggsave(filename = "Fig1_whit_black_met_points.png",
       plot = p2, width = 11, height = 9, units = 'cm',
       scale = 2, dpi = 1000)

## Dumbbel plot AGC

str(proj_biophys_data)

bio_data %>%
  #filter(Year >= 1990) %>% #filter the year
  select(Project_ID, Methodology, MeanAGC_tCO2e_ha)%>% #select columns of interest
  #mutate(diff = Min_AGC_tC_ha - Max_AGC_tC_ha) %>% #calculate difference
  pivot_longer(cols = Project_ID) %>% #get into long format
  rename(Project_ID = value)-> dat_gender
head(dat_gender)

p <- ggplot(bio_data)+
  geom_point(aes(x = reorder(Project_ID, MeanAGC_tCO2e_ha), 
                 y = MeanAGC_tCO2e_ha, shape = Methodology), 
             size = 3, 
             colour = "black", 
             fill   = "black",
             stroke = 1,
             alpha = 0.65,
             show.legend = TRUE)+ theme_classic()+
  labs(x="VCS-REDD+ Project ID", y = expression(paste("Tree abovegraound carbon stock ", "(", tCO[2], sep = " ", ha^-1,")")),)+
  theme(axis.title = element_text(size = 16), 
        axis.text.x = element_text(size = 14, angle = 35, vjust = 1.1, hjust=1),
        axis.text.y = element_text (size = 14), 
        legend.text = element_text (size = 13), 
        legend.title = element_text (size = 14),
        legend.position = c(0.85,0.2))
p

# Adding tag to plot
p <- p + place_label("a")
p

# Preparing panel fig 1
fig1<-(p|p2)#+plot_annotation(tag_levels = 'a')& 
  #theme(plot.tag = element_text(size = 15,face="bold"),plot.tag.position =c(0.11,0.98))
fig1

fig1_v2<-p + theme(plot.tag.position  = c(0, 1), 
                   plot.tag = element_text(vjust = 1.5, hjust = -2.85)) +
  p2 + theme(plot.tag.position  = c(0, 1)) + 
  plot_layout(nrow = 1) & 
  plot_annotation(tag_levels = "a") +
  theme(plot.tag = element_text(size = 15,face="bold"))
fig1_v2

ggsave(filename = "Fig1_panel_v8.png",
       plot = fig1_v2, width = 14, height = 8, units = 'cm',
       scale = 2, dpi = 1200)

## END ## 



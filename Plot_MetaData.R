# Woodland plot lat long
# From CRAN
#install.packages("geobr")

library(tidyverse)
library(geobr)
library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(rnaturalearth)
library(sf)
library(readr)

# Plot map
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

# Development version
#utils::remove.packages('geobr')
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

Plot_Lat_Long <- read_csv("data:/Plot_Lat_Long.csv")
plot_metadata <- data.frame(Plot_Lat_Long)# Plot_Lat_Long file
str(plot_metadata)

# renaming columns
names(plot_metadata)[4] <- 'lat'
names(plot_metadata)[5] <- 'long'
plot_metadata$Project_ID = as.factor(plot_metadata$Project_ID)

## Plot the lat long of the data and the projects ###

# Available data sets

datasets <- list_geobr()

head(datasets)

inter <- read_intermediate_region(
  year=2017,
  showProgress = FALSE
)

# read all states
states <- read_state(
  year=2019, 
  showProgress = FALSE
)

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# Plot all Brazilian states
ggplot() +
  geom_sf(data=states, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis

## World map with project points !!! ####

# Plot map
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

world %>% st_transform(crs = "+proj=moll") %>%
  ggplot() + geom_sf() + theme_minimal()

# Plot points on map
object <- plot_metadata$Project_ID
object
plot_metadata <-  sf::sf_project("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
                      "+proj=moll", plot_metadata) %>%
  as.data.frame() %>%
  setNames(c("long", "lat")) %>%
  cbind(object = object)

world %>% 
  st_transform(crs = "+proj=moll") %>%
  ggplot() + 
  geom_sf() + 
  theme_minimal() +
  geom_point(data = plot_metadata, aes(long, lat)) +
  geom_text(data = plot_metadata, aes(long, lat, label = Project_ID))

# Right plot!!

str(plot_metadata)
plot_metadata$Methodology

proj_loc <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot_metadata, aes(x = long, y = lat, shape = Methodology), size = 2, 
              fill = "red", alpha = 0.8, stroke = 1) +theme_bw()+# shape = 23
  coord_sf(xlim = c(-82, 60), ylim = c(-20, 20), expand = FALSE)

proj_loc

plot_metadata$Project_ID
plot_metadata

proj_locv2 <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot_metadata, aes(x = long, y = lat, color = Project_ID),shape = 1, 
             size = 2, stroke = 1) + 
  labs(x = "Longitude", 
       y = "Latitude",
       color="Project ID", 
       size="", title = "Project and forest data location")+
  scale_color_manual(values=c("#970808","#970808","#970808",
                              "#370ABF","#370ABF","#370ABF",
                              "#665191", "#665191", "#665191",
                              "#ffa600","#ffa600","#ffa600"))+#"#665191","#af060f"))+
  theme_bw()+# shape = 23
  coord_sf(xlim = c(-82, 80), ylim = c(-20, 20), expand = FALSE)+
  guides(size = "none")+ theme(legend.position = "bottom") 
proj_locv2

proj_locv2.1 <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot_metadata, aes(x = long, y = lat, color = Methodology,shape = Lat_type), 
             size = 3.5, stroke = 0.7, alpha = 0.9) + 
  labs(x = "Longitude", 
       y = "Latitude",
       color="Project ID", 
       size="", title = "Location of REDD+ projects and forest data sets",
       shape = "",
       fill = "Methodology")+
  scale_color_manual(values=c("#003f5c",
                              "#7a5195",
                              "#ef5675", 
                              "#ffa600"))+#"#665191","#af060f"))+
  theme_bw()+# shape = 23
  coord_sf(xlim = c(-100, 60), ylim = c(-26, 26), expand = FALSE)+
  guides(size = "none")+ theme(legend.position = "bottom") + 
  theme(legend.text =  element_text(size=11), legend.title = element_text (size = 12))
proj_locv2.1
h
# Final map
shape <- c(rep("data set", 14))
shape

plot_metadata$Shape_Data <- shape
str(plot_metadata)

proj_locv3 <- proj_locv2.1 +
  geom_point(data = plot_metadata, aes(x = Data_Long, y = Data_Lat, 
             fill = Methodology), size = 3.5, stroke = 0.8, shape = 23, 
             color = "black", alpha = 0.9)+ 
  labs(x = "Longitude", 
       y = "Latitude",
       color="Project ID", 
       size="", title = "Location of REDD+ projects and forest data sets",
       shape = "Project",
       fill = c("Project\nmethodology", "Data set"))
proj_locv3

## Final plot
proj_locv4 <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot_metadata, aes(x = long, y = lat, color = Methodology),shape = 1, 
             size = 2, stroke = 1) + 
  labs(color="Project ID", size="", shape = "Project/Data Location", 
       title = "Project and Forest data location")+
  scale_color_manual(values=c("#970808",
                              "#370ABF",
                              "black", 
                              "#ffa600"))+#"#665191","#af060f"))+
  theme_bw()+# shape = 23
  coord_sf(xlim = c(-95, 60), ylim = c(-25, 25), expand = FALSE)+
  guides(size = "none")+ theme(legend.position = "bottom") + 
  theme(legend.text =  element_text(size=11), legend.title = element_text (size = 12))+
  geom_point(data = plot_metadata, aes(x = Data_Long, y = Data_Lat, 
                                       color = Methodology), shape = 3, size = 2, 
             stroke = 0.8)
proj_locv4

ggsave(filename = "FigS1_Project_Location_v2",
       plot = proj_locv4, width = 13, height = 6, units = 'cm',
       scale = 2, dpi = 800)


proj_loc2 <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot_metadata, aes(x = long, y = lat, shape = Methodology, size = 3, 
             alpha = 0.8, stroke = 1)) +theme_bw()+# shape = 23
  coord_sf(xlim = c(-82, 60), ylim = c(-20, 20), expand = FALSE)+
  geom_point(data = plot_metadata, aes(x = Data_Long, y = Data_Lat, shape = Methodology,
                                       colour = "red"), size = 3, 
             alpha = 0.8, stroke = 1)+scale_colour_hue(h = c(0, 180))+ 
  geom_text(data = plot_metadata, aes(Data_Long, Data_Lat, label = Project_ID), size = 3)
proj_loc2

proj_loc3 <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot_metadata, aes(x = long, y = lat, shape = Methodology, size = 3, 
                                       alpha = 0.8, stroke = 1)) +theme_bw()+# shape = 23
  coord_sf(xlim = c(-82, 60), ylim = c(-20, 20), expand = FALSE)+
  geom_point(data = plot_metadata, aes(x = Data_Long, y = Data_Lat, shape = Methodology,
                                       colour = "red"), size = 3, 
             alpha = 0.8, stroke = 1)+scale_colour_hue(h = c(0, 180))+ 
  geom_label(data = plot_metadata, aes(Data_Long, Data_Lat, label = Project_ID), size = 3, 
             fontface = "bold", 
             nudge_y = -1)
proj_loc3

plot_metadata$Project_ID

proj_loc4 <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = plot_metadata, aes(x = long, y = lat, color = Project_ID, size = 3, 
                                       alpha = 0.8, stroke = 1)) +
  scale_color_manual(values=c("#1dabe6","#1dabe6","#1dabe6","#b35a2d",
                              "#b35a2d","#b35a2d","#c3ced0","#c3ced0",
                              "#c3ced0","#ffa600","#ffa600","#ffa600","#ffa600"))+#"#665191","#af060f"))+
  theme_bw()+# shape = 23
  coord_sf(xlim = c(-82, 80), ylim = c(-25, 25), expand = FALSE)+
  geom_point(data = plot_metadata, aes(x = Data_Long, y = Data_Lat, shape = Methodology,
                                       colour = "red"), size = 3, 
             alpha = 0.8, stroke = 1)+
  #geom_label(data = plot_metadata, aes(Data_Long, Data_Lat, label = Project_ID), size = 3, 
           #  fontface = "bold", nudge_y = -1)+
  geom_text_repel(data = plot_metadata, aes(Data_Long, Data_Lat, label = Project_ID), size = 2.5, color = "red",
                  fontface = "bold", nudge_x = c(0.5, 0.5, -0.5,-1.5, 1, 0.8, -1, -0.8, -0.5), 
                  nudge_y = c(0.5, 0.25, 0.5, 0.25, -0.25, 0.5, 0.5, -0.5, 1))
proj_loc4

# save high res
ggsave(filename = "FigS1_Project_Location",
       plot = proj_loc, width = 13, height = 6, units = 'cm',
       scale = 2, dpi = 800)

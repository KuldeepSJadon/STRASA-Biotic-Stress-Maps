##############################################################################
# title         : BB_BS_LB_Maps.R;
# purpose       : Generate .png files for display of predicted disease severity;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, September 2015;
# inputs        : EPIRICE output from 2001-2008 for BB, BS and LB;
# outputs       : maps of BB, BS and LB for BGD, IND and NPL ;
# remarks 1     : requires BB_BS_LB_csv_for_GIS.R to have been run and generated
#               : obligatory .csv files;
# Licence:      : GPL2;
##############################################################################

#### Load libraries #####
library(raster)
library(rgdal)
library(maptools)
library(gpclib)
library(ggplot2)
library(plyr)
library(RColorBrewer)
#### End load libraries ####

#### Load data ####
# GAUL Level 2 country layer (FAO)
gaul <- readOGR(dsn = "/Users/asparks/Google Drive/Data/gaul/g2015_2014_2/BGD_IND_NPL",
                layer = "BGD_IND_NPL")
# original GAUL unit layers are available from FAO:
#http://data.fao.org/map?entryId=f7e7adb0-88fd-11da-a88f-000d939bc5d8

BGD.BB <- read.csv("csv files/BGD_BB.csv")
BGD.BS <- read.csv("csv files/BGD_BS.csv")
BGD.LB <- read.csv("csv files/BGD_LB.csv")

IND.BB <- read.csv("csv files/IND_BB.csv")
IND.BB.mod <- read.csv("csv files/Modified_IND_BB.csv")
IND.BS <- read.csv("csv files/IND_BS.csv")
IND.BS.mod <- read.csv("csv files/Modified_IND_BS.csv")
IND.LB <- read.csv("csv files/IND_LB.csv")

NPL.BB <- read.csv("csv files/NPL_BB.csv")
NPL.BS <- read.csv("csv files/NPL_BS.csv")
NPL.LB <- read.csv("csv files/NPL_LB.csv")

# rearrange factors for proper plotting since defaults to alphabetical

BGD.BB$BB <- factor(BGD.BB$BB, levels(BGD.BB$BB)[c(2, 1)])
BGD.BS$BS <- factor(BGD.BS$BS, levels(BGD.BS$BS)[c(1, 3, 2, 4, 5)])
BGD.BB$BB <- factor(BGD.LB$LB, levels(BGD.LB$LB)[c(2, 1)])

IND.BB$BB <- factor(IND.BB$BB, levels(IND.BB$BB)[c(1, 3, 2, 4, 5)])
IND.BS$BS <- factor(IND.BS$BS, levels(IND.BS$BS)[c(1, 3, 2, 4, 5)])
IND.LB$LB <- factor(IND.LB$LB, levels(IND.LB$LB)[c(1, 3, 2, 4, 5)])

NPL.BB$BB <- factor(NPL.BB$BB, levels(NPL.BB$BB)[c(1, 3, 2, 4, 5)])
NPL.BS$BS <- factor(NPL.BS$BS, levels(NPL.BS$BS)[c(1, 3, 2, 4, 5)])
NPL.LB$LB <- factor(NPL.LB$LB, levels(NPL.LB$LB)[c(1, 3, 2, 4, 5)])

BGD <- gaul[gaul@data$ADM0_NAME == "Bangladesh", ]
IND <- gaul[gaul@data$ADM0_NAME == "India", ]
NPL <- gaul[gaul@data$ADM0_NAME == "Nepal", ]

BGD@data$id <- rownames(BGD@data)
IND@data$id <- rownames(IND@data)
NPL@data$id <- rownames(NPL@data)

BGD.df <- fortify(BGD, region = "id")
IND.df <- fortify(IND, region = "id")
NPL.df <- fortify(NPL, region = "id")

BGD.df <- join(BGD.df, BGD@data, by = "id")
IND.df <- join(IND.df, IND@data, by = "id")
NPL.df <- join(NPL.df, NPL@data, by = "id")

BGD.BB.df <- join(BGD.df, BGD.BB, by = "ADM2_CODE")
BGD.BS.df <- join(BGD.df, BGD.BS, by = "ADM2_CODE")
BGD.LB.df <- join(BGD.df, BGD.LB, by = "ADM2_CODE")

IND.BB.df <- join(IND.df, IND.BB, by = "ADM2_CODE")
IND.BB.mod.df <- join(IND.df, IND.BB.mod, by = "ADM2_CODE")
IND.BS.df <- join(IND.df, IND.BS, by = "ADM2_CODE")
IND.BS.mod.df <- join(IND.df, IND.BS.mod, by = "ADM2_CODE")
IND.LB.df <- join(IND.df, IND.LB, by = "ADM2_CODE")

NPL.BB.df <- join(NPL.df, NPL.BB, by = "ADM2_CODE")
NPL.BS.df <- join(NPL.df, NPL.BS, by = "ADM2_CODE")
NPL.LB.df <- join(NPL.df, NPL.LB, by = "ADM2_CODE")

#### Begin plotting maps using ggplot2 ####
ggplot(data = BGD.BB.df, aes(long, lat, group = group, fill = BB)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Bacterial Blight for Bangladesh") +
  coord_map()
ggsave("Maps/BGD_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = BGD.BS.df, aes(long, lat, group = group, fill = BS)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Brown Spot for Bangladesh") +
  coord_map()
ggsave("Maps/BGD_BS.png", width = 6, height = 6, units = "in")

# LB
ggplot(data = BGD.LB.df, aes(long, lat, group = group, fill = LB)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Leaf Blast for Bangladesh") +
  coord_map()
ggsave("Maps/BGD_LB.png", width = 6, height = 6, units = "in")

# IND
# BB
ggplot(data = IND.BB.df, aes(long, lat, group = group, fill = BB)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 9),
        strip.text.x = element_text(size = 10),
        legend.title = element_blank()) +
  coord_map()
ggsave("Maps/IND_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = IND.BS.df, aes(long, lat, group = group, fill = BS)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Brown Spot for India") +
  coord_map()
ggsave("Maps/IND_BS.png", width = 6, height = 6, units = "in")

# LB
ggplot(data = IND.LB.df, aes(long, lat, group = group, fill = LB)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Leaf Blast for India") +
  coord_map()
ggsave("Maps/IND_LB.png", width = 6, height = 6, units = "in")

# NPL
# BB
ggplot(data = NPL.BB.df, aes(long, lat, group = group, fill = BB)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Bacterial Blight for Nepal") +
  coord_map()
ggsave("Maps/NPL_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = NPL.BS.df, aes(long, lat, group = group, fill = BS)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Brown Spot for Nepal") +
  coord_map()
ggsave("Maps/NPL_BS.png", width = 6, height = 6, units = "in")

# LB
ggplot(data = NPL.LB.df, aes(long, lat, group = group, fill = LB)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Leaf Blast for Nepal") +
  coord_map()
ggsave("Maps/NPL_LB.png", width = 6, height = 6, units = "in")

#### End data visualisation original data ####

#### Begin data visualisation of modified data ####
# BB
ggplot(data = IND.BB.mod.df, aes(long, lat, group = group, fill = BB)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Corrected Relative Risk of Bacterial Blight for India") +
  coord_map()
ggsave("Maps/Modified_IND_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = IND.BS.mod.df, aes(long, lat, group = group, fill = BS)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu", name = "Relative Risk") +
  scale_y_continuous(name = "Latitude") +
  scale_x_continuous(name = "Longitude") +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Corrected Relative Risk of Brown Spot for India") +
  coord_map()
ggsave("Maps/Modified_IND_BS.png", width = 6, height = 6, units = "in")

#eos

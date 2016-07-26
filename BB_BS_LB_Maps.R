##############################################################################
# title         : BB_BS_LB_Maps.R;
# purpose       : Generate .png files for display of predicted disease severity;
# producer      : prepared by A. Sparks;
# last update   : in Toowoomba, QLD, July 2016;
# inputs        : EPIRICE output from 2001-2008 for BB, BS and LB;
# outputs       : maps of BB, BS and LB for BGD, IND and NPL ;
# remarks 1     : requires BB_BS_LB_csv_for_GIS.R to have been run and generated
#               : obligatory .csv files;
# Licence:      : GPL2;
##############################################################################

# Load libraries ---------------------------------------------------------------
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(RColorBrewer)

# Load data --------------------------------------------------------------------
# This is the shapefile that is used for plotting the disease severity results
SA <- readOGR(dsn = path.expand("~/Google Drive/Data/gaul/g2015_2014_2/BGD_IND_NPL"),
                layer = "BGD_IND_NPL")
# original GAUL unit layers are available from FAO:

BGD_BB <- read.csv("csv files/BGD_BB_csv")
BGD_BS <- read.csv("csv files/BGD_BS_csv")
BGD_LB <- read.csv("csv files/BGD_LB_csv")

IND_BB <- read.csv("csv files/IND_BB_csv")
IND_BB_mod <- read.csv("csv files/Modified_IND_BB_csv")
IND_BS <- read.csv("csv files/IND_BS_csv")
IND_BS_mod <- read.csv("csv files/Modified_IND_BS_csv")
IND_LB <- read.csv("csv files/IND_LB_csv")

NPL_BB <- read.csv("csv files/NPL_BB_csv")
NPL_BS <- read.csv("csv files/NPL_BS_csv")
NPL_LB <- read.csv("csv files/NPL_LB_csv")

# rearrange factors for proper plotting since defaults to alphabetical----------
BGD_BS$BS <- factor(BGD_BS$BS, levels(BGD_BS$BS)[c(1, 3, 2, 4, 5)])
BGD_BB$BB <- factor(BGD_LB$LB, levels(BGD_LB$LB)[c(2, 1)])

IND_BB$BB <- factor(IND_BB$BB, levels(IND_BB$BB)[c(1, 3, 2, 4, 5)])
IND_BB_mod$BB <- factor(IND_BB_mod$BB, levels(IND_BB_mod$BB)[c(1, 3, 2, 4, 5)])
IND_BS$BS <- factor(IND_BS$BS, levels(IND_BS$BS)[c(1, 3, 2, 4, 5)])
IND_BS_mod$BS <- factor(IND_BS_mod$BS, levels(IND_BS_mod$BS)[c(1, 3, 2, 4, 5)])
IND_LB$LB <- factor(IND_LB$LB, levels(IND_LB$LB)[c(1, 3, 2, 4, 5)])

NPL_BB$BB <- factor(NPL_BB$BB, levels(NPL_BB$BB)[c(1, 3, 2, 4, 5)])
NPL_BS$BS <- factor(NPL_BS$BS, levels(NPL_BS$BS)[c(1, 3, 2, 4, 5)])
NPL_LB$LB <- factor(NPL_LB$LB, levels(NPL_LB$LB)[c(1, 3, 2, 4, 5)])

BGD <- SA[SA@data$ADM0_NAME == "Bangladesh", ]
IND <- SA[SA@data$ADM0_NAME == "India", ]
NPL <- SA[SA@data$ADM0_NAME == "Nepal", ]

BGD@data$id <- rownames(BGD@data)
IND@data$id <- rownames(IND@data)
NPL@data$id <- rownames(NPL@data)

BGD.df <- fortify(BGD, region = "id")
IND.df <- fortify(IND, region = "id")
NPL.df <- fortify(NPL, region = "id")

BGD.df <- join(BGD.df, BGD@data, by = "id")
IND.df <- join(IND.df, IND@data, by = "id")
NPL.df <- join(NPL.df, NPL@data, by = "id")

BGD_BB_df <- join(BGD.df, BGD_BB, by = "ADM2_CODE")
BGD_BS_df <- join(BGD.df, BGD_BS, by = "ADM2_CODE")
BGD_LB_df <- join(BGD.df, BGD_LB, by = "ADM2_CODE")

IND_BB_df <- join(IND.df, IND_BB, by = "ADM2_CODE")
IND_BB_mod.df <- join(IND.df, IND_BB_mod, by = "ADM2_CODE")
IND_BS_df <- join(IND.df, IND_BS, by = "ADM2_CODE")
IND_BS_mod.df <- join(IND.df, IND_BS_mod, by = "ADM2_CODE")
IND_LB_df <- join(IND.df, IND_LB, by = "ADM2_CODE")

NPL_BB_df <- join(NPL.df, NPL_BB, by = "ADM2_CODE")
NPL_BS_df <- join(NPL.df, NPL_BS, by = "ADM2_CODE")
NPL_LB_df <- join(NPL.df, NPL_LB, by = "ADM2_CODE")

#### Begin plotting maps using ggplot2 -----------------------------------------
ggplot(data = BGD_BB_df, aes(long, lat, group = group, fill = BB)) +
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
  coord_map("lambert", lat0 = 20.74623, lat1 = 26.63195)
ggsave("Maps/BGD_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = BGD_BS_df, aes(long, lat, group = group, fill = BS)) +
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
  coord_map("lambert", lat0 = 20.74623, lat1 = 26.63195)
ggsave("Maps/BGD_BS.png", width = 6, height = 6, units = "in")

# LB
ggplot(data = BGD_LB_df, aes(long, lat, group = group, fill = LB)) +
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
  coord_map("lambert", lat0 = 20.74623, lat1 = 26.63195)
ggsave("Maps/BGD_LB.png", width = 6, height = 6, units = "in")

# IND
# BB
ggplot(data = IND_BB_df, aes(long, lat, group = group, fill = BB)) +
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
  ggtitle("Relative Risk of Bacterial Blight for India") +
  coord_map("lambert", lat0 = 6.755997, lat1 = 33.17194)
ggsave("Maps/IND_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = IND_BS_df, aes(long, lat, group = group, fill = BS)) +
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
  coord_map("lambert", lat0 = 6.755997, lat1 = 33.17194)
ggsave("Maps/IND_BS.png", width = 6, height = 6, units = "in")

# LB
ggplot(data = IND_LB_df, aes(long, lat, group = group, fill = LB)) +
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
  coord_map("lambert", lat0 = 6.755997, lat1 = 33.17194)
ggsave("Maps/IND_LB.png", width = 6, height = 6, units = "in")

# NPL
# BB
ggplot(data = NPL_BB_df, aes(long, lat, group = group, fill = BB)) +
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
  coord_map("lambert", lat0 = 26.35358, lat1 = 30.44968)
ggsave("Maps/NPL_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = NPL_BS_df, aes(long, lat, group = group, fill = BS)) +
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
  coord_map("lambert", lat0 = 26.35358, lat1 = 30.44968)
ggsave("Maps/NPL_BS.png", width = 6, height = 6, units = "in")

# LB
ggplot(data = NPL_LB_df, aes(long, lat, group = group, fill = LB)) +
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
  coord_map("lambert", lat0 = 26.35358, lat1 = 30.44968)
ggsave("Maps/NPL_LB.png", width = 6, height = 6, units = "in")

#### End data visualisation original data ####

#### Begin data visualisation of modified data ####
# BB
ggplot(data = IND_BB_mod.df, aes(long, lat, group = group, fill = BB)) +
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
  coord_map("lambert", lat0 = 6.755997, lat1 = 33.17194)
ggsave("Maps/Modified_IND_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = IND_BS_mod.df, aes(long, lat, group = group, fill = BS)) +
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
  coord_map("lambert", lat0 = 6.755997, lat1 = 33.17194)
ggsave("Maps/Modified_IND_BS.png", width = 6, height = 6, units = "in")

#eos

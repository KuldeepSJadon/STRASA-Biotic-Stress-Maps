##############################################################################
# title         : BB_BS_LB_Maps.R;
# purpose       : Generate .png files for display of predicted disease severity;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, April 2015;
# inputs        : EPIRICE output from 2001-2008 for BB, BS and LB;
# outputs       : maps of BB, BS and LB for BGD, IND and NPL ;
# remarks 1     : ;
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
library(classInt)
#### End load libraries ####

#### Load data ####

# Disease data from EPIRICE model (IRRI)
diseases <- list(stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bblight_audpc.tif", full.names = TRUE)),
                 stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bspot_audpc.tif", full.names = TRUE)),
                 stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+blast_audpc.tif", full.names = TRUE)))
names(diseases) <- c("BB", "BS", "LB")

# GAUL Level 2 country layer (FAO)
gaul <- readOGR(dsn = "/Users/asparks/Data/gaul/g2015_2014_2/", layer = "g2015_2014_2")
# original GAUL unit layers are available from FAO:
#http://data.fao.org/map?entryId=f7e7adb0-88fd-11da-a88f-000d939bc5d8

countries <- list(BGD <- gaul[gaul@data$ADM0_NAME == "Bangladesh", ],
                  IND <- gaul[gaul@data$ADM0_NAME == "India", ],
                  NPL <- gaul[gaul@data$ADM0_NAME == "Nepal", ])
names(countries) <- c("BGD", "IND", "NPL")

#### End load data ####

#### Start data munging ####
for (i in 1:3) {
  for (j in 1:3) {
    k <- extract(mean(diseases[[j]]), countries[[i]], method = "bilinear",
                 weights = TRUE, fun = mean, na.rm = TRUE)

    k <- data.frame(unlist(lapply(k, FUN = mean, na.rm = TRUE))) # unlist and generate mean values for each polygon

    row.names(k) <- row.names(countries[[i]])
    names(k) <- names(diseases[j])

    row.names(countries[[i]]) <- row.names(countries[[i]])

    assign(paste(names(countries)[i], names(diseases[j]), sep = "."),
           spCbind(countries[[i]], k))
  }
}
rm("i", "j", "k", "countries", "diseases")
#### End data munging ####

#### Start data visualisation in R ####
BGD.BB@data$id <- rownames(BGD.BB@data)
BGD.BS@data$id <- rownames(BGD.BS@data)
BGD.LB@data$id <- rownames(BGD.LB@data)

IND.BB@data$id <- rownames(IND.BB@data)
IND.BS@data$id <- rownames(IND.BS@data)
IND.LB@data$id <- rownames(IND.LB@data)

NPL.BB@data$id <- rownames(NPL.BB@data)
NPL.BS@data$id <- rownames(NPL.BS@data)
NPL.LB@data$id <- rownames(NPL.LB@data)

BGD.BB.df <- fortify(BGD.BB, region = "id")
BGD.BS.df <- fortify(BGD.BS, region = "id")
BGD.LB.df <- fortify(BGD.LB, region = "id")
names(BGD.BB.df) <- names(BGD.BS.df) <- names(BGD.LB.df) <- c("Longitude", "Latitude", "order", "hole", "piece", "group", "id")

IND.BB.df <- fortify(IND.BB, id = "BB", region = "BB")
IND.BS.df <- fortify(IND.BS, id = "BS", region = "BS")
IND.LB.df <- fortify(IND.LB, id = "LB", region = "LB")
names(IND.BB.df) <- names(IND.BS.df) <- names(IND.LB.df) <- c("Longitude", "Latitude", "order", "hole", "piece", "group", "id")

NPL.BB.df <- fortify(NPL.BB, id = "BB", region = "BB")
NPL.BS.df <- fortify(NPL.BS, id = "BS", region = "BS")
NPL.LB.df <- fortify(NPL.LB, id = "LB", region = "LB")
names(NPL.BB.df) <- names(NPL.BS.df) <- names(NPL.LB.df) <- c("Longitude", "Latitude", "order", "hole", "piece", "group", "id")

BGD.BB.df$id <- as.numeric(BGD.BB.df$id)
BGD.BS.df$id <- as.numeric(BGD.BS.df$id)
BGD.LB.df$id <- as.numeric(BGD.LB.df$id)

IND.BB.df$id <- as.numeric(IND.BB.df$id)
IND.BS.df$id <- as.numeric(IND.BS.df$id)
IND.LB.df$id <- as.numeric(IND.LB.df$id)

NPL.BB.df$id <- as.numeric(NPL.BB.df$id)
NPL.BS.df$id <- as.numeric(NPL.BS.df$id)
NPL.LB.df$id <- as.numeric(NPL.LB.df$id)

BGD.BB.breaks <- classIntervals(BGD.BB.df$id, 5, style = "equal", labels = FALSE)$brks
BGD.BS.breaks <- classIntervals(BGD.BS.df$id, 5, style = "equal", labels = FALSE)$brks
BGD.LB.breaks <- classIntervals(BGD.LB.df$id, 2, style = "equal", labels = FALSE)$brks

IND.BB.breaks <- classIntervals(IND.BB.df$id, 5, style = "equal", labels = FALSE)$brks
IND.BS.breaks <- classIntervals(IND.BS.df$id, 5, style = "equal", labels = FALSE)$brks
IND.LB.breaks <- classIntervals(IND.LB.df$id, 5, style = "equal", labels = FALSE)$brks

NPL.BB.breaks <- classIntervals(NPL.BB.df$id, 5, style = "equal", labels = FALSE)$brks
NPL.BS.breaks <- classIntervals(NPL.BS.df$id, 5, style = "equal", labels = FALSE)$brks
NPL.LB.breaks <- classIntervals(NPL.LB.df$id, 5, style = "equal", labels = FALSE)$brks

BGD.BB.df$plot <- cut(BGD.BB.df$id, breaks = BGD.BB.breaks, include.lowest = TRUE)
BGD.BS.df$plot <- cut(BGD.BS.df$id, breaks = BGD.BS.breaks, include.lowest = TRUE)
BGD.LB.df$plot <- cut(BGD.LB.df$id, breaks = BGD.LB.breaks, include.lowest = TRUE)

IND.BB.df$plot <- cut(IND.BB.df$id, breaks = IND.BB.breaks, include.lowest = TRUE)
IND.BS.df$plot <- cut(IND.BS.df$id, breaks = IND.BS.breaks, include.lowest = TRUE)
IND.LB.df$plot <- cut(IND.LB.df$id, breaks = IND.LB.breaks, include.lowest = TRUE)

NPL.BB.df$plot <- cut(NPL.BB.df$id, breaks = NPL.BB.breaks, include.lowest = TRUE)
NPL.BS.df$plot <- cut(NPL.BS.df$id, breaks = NPL.BS.breaks, include.lowest = TRUE)
NPL.LB.df$plot <- cut(NPL.LB.df$id, breaks = NPL.LB.breaks, include.lowest = TRUE)

# BGD
# BB
ggplot(data = BGD.BB.df, aes(Longitude, Latitude, group = group, fill = plot)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu",
                    name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate", "Moderately\nHigh", "High")) +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Bacterial Blight for Bangladesh") +
  coord_equal()
ggsave("Maps/BGD_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = BGD.BS.df, aes(Longitude, Latitude, group = group, fill = plot)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu",
                    name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate", "Moderately\nHigh", "High")) +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Brown Spot for Bangladesh") +
  coord_equal()
ggsave("Maps/BGD_BS.png", width = 6, height = 6, units = "in")

# LB
ggplot(data = BGD.LB.df, aes(Longitude, Latitude, group = group, fill = plot)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu",
                    name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate")) +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Leaf Blast for Bangladesh") +
  coord_equal()
ggsave("Maps/BGD_LB.png", width = 6, height = 6, units = "in")

# IND
# BB
ggplot(data = IND.BB.df, aes(Longitude, Latitude, group = group, fill = plot)) +
  geom_polygon(color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu",
                    name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate", "Moderately\nHigh", "High")) +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 9),
        strip.text.x = element_text(size = 10),
        legend.title = element_blank()) +
  theme(legend.position = "bottom") +
  coord_equal()
ggsave("Maps/IND_BB.png", width = 6, height = 6, units = "in", dpi = 600)

# BS
ggplot(data = IND.BS.df, aes(Longitude, Latitude, group = group)) +
  geom_polygon(aes(group = group, fill = plot), color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu",
                    name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate", "Moderately\nHigh", "High")) +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Brown Spot for India") +
  coord_equal()
ggsave("Maps/IND_BS.png", width = 6, height = 6, units = "in")

# LB
ggplot(data = IND.LB.df, aes(Longitude, Latitude, group = group)) +
  geom_polygon(aes(group = group, fill = plot), color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu",
                    name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate", "Moderately\nHigh", "High")) +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Leaf Blast for India") +
  coord_equal()
ggsave("Maps/IND_LB.png", width = 6, height = 6, units = "in")

# NPL
# BB
ggplot(data = NPL.BB.df, aes(Longitude, Latitude, group = group)) +
  geom_polygon(aes(group = group, fill = plot), color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu",
                    name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate", "Moderately\nHigh", "High")) +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Bacterial Blight for Nepal") +
  coord_equal()
ggsave("Maps/NPL_BB.png", width = 6, height = 6, units = "in")

# BS
ggplot(data = NPL.BS.df, aes(Longitude, Latitude, group = group)) +
  geom_polygon(aes(group = group, fill = plot), color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu",
                    name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate", "Moderately\nHigh", "High")) +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Brown Spot for Nepal") +
  coord_equal()
ggsave("Maps/NPL_BS.png", width = 6, height = 6, units = "in")

# LB
ggplot(data = NPL.LB.df, aes(Longitude, Latitude, group = group)) +
  geom_polygon(aes(group = group, fill = factor(plot)), color = "white", size = 0.2) +
  scale_fill_brewer(palette = "GnBu",
                    name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate", "Moderately\nHigh", "High")) +
  theme(axis.title = element_text(face = "bold", size = 6),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(face = "bold", size = 8),
        legend.text = element_text(size = 5),
        strip.text.x = element_text(size = 6),
        legend.title = element_blank()) +
  ggtitle("Relative Risk of Leaf Blast for Nepal") +
  coord_equal()
ggsave("Maps/NPL_LB.png", width = 6, height = 6, units = "in")

#### End data visualisation ####

#eos

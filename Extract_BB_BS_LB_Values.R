##############################################################################
# title         : Extract_BB_BS_LB_Values.R;
# purpose       : Extract values by political polygon values;
# producer      : prepared by A. Sparks;
# last update   : in Quezon City, PHL, Nov 2015;
# inputs        : EPIRICE output from 2001-2008 for BB, BS and LB;
# outputs       : .csv files for use in mapping unmodified disease risk;
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
diseases <- list(stack(list.files(path = "Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bblight_audpc.tif", full.names = TRUE)),
                 stack(list.files(path = "Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bspot_audpc.tif", full.names = TRUE)),
                 stack(list.files(path = "Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+blast_audpc.tif", full.names = TRUE)))
names(diseases) <- c("BB", "BS", "LB")

countries <- list(getData("GADM", country = "BGD", level = 2),
                  getData("GADM", country = "IND", level = 2),
                  getData("GADM", country = "NPL", level = 2))
names(countries) <- c("BGD", "IND", "NPL")

#### End load data ####

#### Start data munging ####
for(i in 1:3){
  for(j in 1:3){
    k <- extract(mean(diseases[[j]]), countries[[i]], coordinates(countries[[i]]),
                 method = "bilinear", weights = TRUE, fun = mean, na.rm = TRUE)

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

#### Start data visualisation ####
BGD.BB@data$id <- rownames(BGD.BB@data)
BGD.BS@data$id <- rownames(BGD.BS@data)
BGD.LB@data$id <- rownames(BGD.LB@data)

IND.BB@data$id <- rownames(IND.BB@data)
IND.BS@data$id <- rownames(IND.BS@data)
IND.LB@data$id <- rownames(IND.LB@data)

NPL.BB@data$id <- rownames(NPL.BB@data)
NPL.BS@data$id <- rownames(NPL.BS@data)
NPL.LB@data$id <- rownames(NPL.LB@data)

BGD.BB.df <- fortify(BGD.BB, id = "BB", region = "BB")
BGD.BS.df <- fortify(BGD.BS, id = "BS", region = "BS")
BGD.LB.df <- fortify(BGD.LB, id = "LB", region = "LB")
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

BGD.BB.breaks <- round(classIntervals(BGD.BB.df$id, 5, style = "equal", labels = FALSE)$brks, 0)
BGD.BS.breaks <- round(classIntervals(BGD.BS.df$id, 5, style = "equal", labels = FALSE)$brks, 0)
BGD.LB.breaks <- round(classIntervals(BGD.LB.df$id, 3, style = "equal", labels = FALSE)$brks, 0)

IND.BB.breaks <- round(classIntervals(IND.BB.df$id, 5, style = "equal", labels = FALSE)$brks, 0)
IND.BS.breaks <- round(classIntervals(IND.BS.df$id, 5, style = "equal", labels = FALSE)$brks, 0)
IND.LB.breaks <- round(classIntervals(IND.LB.df$id, 5, style = "equal", labels = FALSE)$brks, 0)

NPL.BB.breaks <- round(classIntervals(NPL.BB.df$id, 5, style = "equal", labels = FALSE)$brks, 0)
NPL.BS.breaks <- round(classIntervals(NPL.BS.df$id, 5, style = "equal", labels = FALSE)$brks, 0)
NPL.LB.breaks <- round(classIntervals(NPL.LB.df$id, 5, style = "equal", labels = FALSE)$brks, 0)

BGD.BB.df$plot <- cut(BGD.BB.df$id, breaks = BGD.BB.breaks, include.lowest = TRUE)
BGD.BS.df$plot <- cut(BGD.BS.df$id, breaks = BGD.BS.breaks, include.lowest = TRUE)
BGD.LB.df$plot <- cut(BGD.LB.df$id, breaks = BGD.LB.breaks, include.lowest = TRUE)

IND.BB.df$plot <- cut(IND.BB.df$id, breaks = IND.BB.breaks, include.lowest = TRUE)
IND.BS.df$plot <- cut(IND.BS.df$id, breaks = IND.BS.breaks, include.lowest = TRUE)
IND.LB.df$plot <- cut(IND.LB.df$id, breaks = IND.LB.breaks, include.lowest = TRUE)

NPL.BB.df$plot <- cut(NPL.BB.df$id, breaks = NPL.BB.breaks, include.lowest = TRUE)
NPL.BS.df$plot <- cut(NPL.BS.df$id, breaks = NPL.BS.breaks, include.lowest = TRUE)
NPL.LB.df$plot <- cut(NPL.LB.df$id, breaks = NPL.LB.breaks, include.lowest = TRUE)



#eos

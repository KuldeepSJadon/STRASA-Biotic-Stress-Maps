##############################################################################
# title         : BB_BS_LB_csv_for_GIS.R;
# purpose       : Create .csv files to be linked to FAO GAUL data for mapping and analysis;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, Sep 2015;
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
library(classInt)
#### End load libraries ####

#### Load data ####

# Disease data from EPIRICE model (IRRI)
diseases <- list(stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bblight_audpc.tif$", full.names = TRUE)),
                 stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bspot_audpc.tif$", full.names = TRUE)),
                 stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+blast_audpc.tif$", full.names = TRUE)))
names(diseases) <- c("BB", "BS", "LB")

# GAUL Level 2 country layer (FAO)
gaul <- readOGR(dsn = "/Users/asparks/Google Drive/Data/gaul/g2015_2014_2/BGD_IND_NPL",
                layer = "BGD_IND_NPL")
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

#### Write csv files to disk for use in a GIS ###
BGD.BB.breaks <- classIntervals(BGD.BB@data$BB, 5, style = "equal", labels = FALSE)$brks
BGD.BS.breaks <- classIntervals(BGD.BS@data$BS, 5, style = "equal", labels = FALSE)$brks
BGD.LB.breaks <- classIntervals(BGD.LB@data$LB, 2, style = "equal", labels = FALSE)$brks

IND.BB.breaks <- classIntervals(IND.BB@data$BB, 5, style = "equal", labels = FALSE)$brks
IND.BS.breaks <- classIntervals(IND.BS@data$BS, 5, style = "equal", labels = FALSE)$brks
IND.LB.breaks <- classIntervals(IND.LB@data$LB, 5, style = "equal", labels = FALSE)$brks

NPL.BB.breaks <- classIntervals(NPL.BB@data$BB, 5, style = "equal", labels = FALSE)$brks
NPL.BS.breaks <- classIntervals(NPL.BS@data$BS, 5, style = "equal", labels = FALSE)$brks
NPL.LB.breaks <- classIntervals(NPL.LB@data$LB, 5, style = "equal", labels = FALSE)$brks

labs <- c("Low", "Moderately Low", "Moderate", "Moderately High", "High")
labs2 <- c("Low", "Moderately Low")

BGD.BB@data$BB <- cut(BGD.BB@data$BB, breaks = BGD.BB.breaks,
                      include.lowest = TRUE,
                      labels = labs)
BGD.BS@data$BS <- cut(BGD.BS@data$BS, breaks = BGD.BS.breaks,
                      include.lowest = TRUE,
                      labels = labs)
BGD.LB@data$LB <- cut(BGD.LB@data$LB, breaks = BGD.LB.breaks,
                      include.lowest = TRUE,
                      labels = labs)

IND.BB@data$BB <- cut(IND.BB@data$BB, breaks = IND.BB.breaks,
                      include.lowest = TRUE,
                      labels = labs)
IND.BS@data$BS <- cut(IND.BS@data$BS, breaks = IND.BS.breaks,
                      include.lowest = TRUE,
                      labels = labs)
IND.LB@data$LB <- cut(IND.LB@data$LB, breaks = IND.LB.breaks,
                      include.lowest = TRUE,
                      labels = labs)

NPL.BB@data$BB <- cut(NPL.BB@data$BB, breaks = NPL.BB.breaks,
                      include.lowest = TRUE,
                      labels = labs)
NPL.BS@data$BS <- cut(NPL.BS@data$BS, breaks = NPL.BS.breaks,
                      include.lowest = TRUE,
                      labels = labs)
NPL.LB@data$LB <- cut(NPL.LB@data$LB, breaks = NPL.LB.breaks,
                      include.lowest = TRUE,
                      labels = labs)

write.csv(BGD.BB@data, "Data/BGD_BB.csv", row.names = FALSE)
write.csv(BGD.LB@data, "Data/BGD_LB.csv", row.names = FALSE)
write.csv(BGD.BS@data, "Data/BGD_BS.csv", row.names = FALSE)

write.csv(IND.BB@data, "Data/IND_BB.csv", row.names = FALSE)
write.csv(IND.LB@data, "Data/IND_LB.csv", row.names = FALSE)
write.csv(IND.BS@data, "Data/IND_BS.csv", row.names = FALSE)

write.csv(NPL.BB@data, "Data/NPL_BB.csv", row.names = FALSE)
write.csv(NPL.LB@data, "Data/NPL_LB.csv", row.names = FALSE)
write.csv(NPL.BS@data, "Data/NPL_BS.csv", row.names = FALSE)

# Modify India BS severity levels based on feedback from G. S. Laha
IND.BS@data[, 13] <- as.character(IND.BS@data[, 13])

IND.BS@data[, 13][IND.BS@data[, 6] == "Bihar"] <- "Moderate"
IND.BS@data[, 13][IND.BS@data[, 6] == "Jharkhand"] <- "Moderate"
IND.BS@data[, 13][IND.BS@data[, 6] == "Punjab"] <- "Moderate"
IND.BS@data[, 13][IND.BS@data[, 6] == "Tamil Nadu"] <- "Moderate"
IND.BS@data[, 13][IND.BS@data[, 6] == "Uttar Pradesh"] <- "Moderate"

write.csv(IND.BS@data, "Data/Modified_IND_BS.csv", row.names = FALSE)

#eos

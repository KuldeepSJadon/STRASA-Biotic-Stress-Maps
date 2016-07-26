##############################################################################
# title         : BB_BS_LB_csv_for_GIS.R;
# purpose       : Create .csv files to be linked to FAO GAUL data for mapping
#               : and analysis;
# producer      : prepared by A. Sparks;
# last update   : in Toowoomba, QLD, July 2016;
# inputs        : EPIRICE output from 2001-2008 for BB, BS and LB;
# outputs       : maps of BB, BS and LB for BGD, IND and NPL ;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

# Load libraries ---------------------------------------------------------------
library(raster)
library(rgdal)
library(maptools)
library(plyr)
library(classInt)


# Load data --------------------------------------------------------------------

# Disease data from EPIRICE model (IRRI)
diseases <- list(stack(list.files(path = "./Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bblight_audpc.tif$",
                                  full.names = TRUE)),
                 stack(list.files(path = "./Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bspot_audpc.tif$",
                                  full.names = TRUE)),
                 stack(list.files(path = "./Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+blast_audpc.tif$",
                                  full.names = TRUE)))
names(diseases) <- c("BB", "BS", "LB")

# GAUL Level 2 country layer (FAO)
gaul <- readOGR(dsn = path.expand("~/Google Drive/Data/gaul/g2015_2014_2/BGD_IND_NPL"),
                layer = "BGD_IND_NPL")
# original GAUL unit layers are available from FAO:
#http://data.fao.org/map?entryId=f7e7adb0-88fd-11da-a88f-000d939bc5d8

countries <- list(BGD <- gaul[gaul@data$ADM0_NAME == "Bangladesh", ],
                  IND <- gaul[gaul@data$ADM0_NAME == "India", ],
                  NPL <- gaul[gaul@data$ADM0_NAME == "Nepal", ])
names(countries) <- c("BGD", "IND", "NPL")


# Start data munging -----------------------------------------------------------
for (i in 1:3) {
  for (j in 1:3) {
    k <- extract(mean(diseases[[j]], na.rm = TRUE), countries[[i]],
                 method = "bilinear", fun = mean)

    # unlist and generate mean values for each polygon
    k <- data.frame(unlist(lapply(k, FUN = mean, na.rm = TRUE)))

    row.names(k) <- row.names(countries[[i]])
    names(k) <- names(diseases[j])

    row.names(countries[[i]]) <- row.names(countries[[i]])

    assign(paste(names(countries)[i], names(diseases[j]), sep = "_"),
           spCbind(countries[[i]], k))
  }
}
rm("i", "j", "k", "countries", "diseases")


# Write csv files to disk for use in a GIS -------------------------------------
BGD_BB_breaks <- classIntervals(BGD_BB@data$BB, 2,
                                style = "equal", labels = FALSE)$brks
BGD_BS_breaks <- classIntervals(BGD_BS@data$BS, 5,
                                style = "equal", labels = FALSE)$brks
BGD_LB_breaks <- classIntervals(BGD_LB@data$LB, 2,
                                style = "equal", labels = FALSE)$brks

IND_BB_breaks <- classIntervals(IND_BB@data$BB, 5,
                                style = "equal", labels = FALSE)$brks
IND_BS_breaks <- classIntervals(IND_BS@data$BS, 5,
                                style = "equal", labels = FALSE)$brks
IND_LB_breaks <- classIntervals(IND_LB@data$LB, 5,
                                style = "equal", labels = FALSE)$brks

NPL_BB_breaks <- classIntervals(NPL_BB@data$BB, 5,
                                style = "equal", labels = FALSE)$brks
NPL_BS_breaks <- classIntervals(NPL_BS@data$BS, 5,
                                style = "equal", labels = FALSE)$brks
NPL_LB_breaks <- classIntervals(NPL_LB@data$LB, 5,
                                style = "equal", labels = FALSE)$brks

labs <- c("Low", "Moderately Low", "Moderate", "Moderately Severe", "Severe")
labs2 <- c("Moderately Severe", "Severe")
labs3 <- c("Low", "Moderately Low")

BGD_BB@data$BB <- cut(BGD_BB@data$BB, breaks = BGD_BB_breaks,
                      include.lowest = TRUE,
                      labels = labs2)
BGD_BS@data$BS <- cut(BGD_BS@data$BS, breaks = BGD_BS_breaks,
                      include.lowest = TRUE,
                      labels = labs)
BGD_LB@data$LB <- cut(BGD_LB@data$LB, breaks = BGD_LB_breaks,
                      include.lowest = TRUE,
                      labels = labs3)

IND_BB@data$BB <- cut(IND_BB@data$BB, breaks = IND_BB_breaks,
                      include.lowest = TRUE,
                      labels = labs)
IND_BS@data$BS <- cut(IND_BS@data$BS, breaks = IND_BS_breaks,
                      include.lowest = TRUE,
                      labels = labs)
IND_LB@data$LB <- cut(IND_LB@data$LB, breaks = IND_LB_breaks,
                      include.lowest = TRUE,
                      labels = labs)

NPL_BB@data$BB <- cut(NPL_BB@data$BB, breaks = NPL_BB_breaks,
                      include.lowest = TRUE,
                      labels = labs)
NPL_BS@data$BS <- cut(NPL_BS@data$BS, breaks = NPL_BS_breaks,
                      include.lowest = TRUE,
                      labels = labs)
NPL_LB@data$LB <- cut(NPL_LB@data$LB, breaks = NPL_LB_breaks,
                      include.lowest = TRUE,
                      labels = labs)

write.csv(BGD_BB@data, "csv files/BGD_BB_csv", row.names = FALSE)
write.csv(BGD_LB@data, "csv files/BGD_LB_csv", row.names = FALSE)
write.csv(BGD_BS@data, "csv files/BGD_BS_csv", row.names = FALSE)

write.csv(IND_BB@data, "csv files/IND_BB_csv", row.names = FALSE)
write.csv(IND_LB@data, "csv files/IND_LB_csv", row.names = FALSE)
write.csv(IND_BS@data, "csv files/IND_BS_csv", row.names = FALSE)

write.csv(NPL_BB@data, "csv files/NPL_BB_csv", row.names = FALSE)
write.csv(NPL_LB@data, "csv files/NPL_LB_csv", row.names = FALSE)
write.csv(NPL_BS@data, "csv files/NPL_BS_csv", row.names = FALSE)

# Modify India BS/BB severity levels based on feedback from G. S. Laha ---------
IND_BS@data[, 13] <- as.character(IND_BS@data[, 13])
IND_BB@data[, 13] <- as.character(IND_BB@data[, 13])

#BS
# Small scale changes
IND_BS@data[, 13][IND_BS@data[, 6] == "Bihar"] <- "Moderate"
IND_BS@data[, 13][IND_BS@data[, 6] == "Jharkhand"] <- "Moderate"
IND_BS@data[, 13][IND_BS@data[, 6] == "Punjab"] <- "Moderate"
IND_BS@data[, 13][IND_BS@data[, 6] == "Tamil Nadu"] <- "Moderate"
IND_BS@data[, 13][IND_BS@data[, 6] == "Uttar Pradesh"] <- "Moderate"

# Large scale changes
source("Modify_IND_BS.R")
IND_BS@data$BS[IND_BS@data$ADM2_CODE %in% updateBS] <- "Severe"

#BB
# Small scale changes
IND_BB@data[, 13][IND_BB@data[, 6] == "Andhra Pradesh"] <- "Moderate"
IND_BB@data[, 13][IND_BB@data[, 6] == "Madhya Pradesh"] <- "Moderately Low"
IND_BB@data[, 13][IND_BB@data[, 6] == "Tamil Nadu"] <- "Moderately Severe"
IND_BB@data[, 13][IND_BB@data[, 6] == "West Bengal"] <- "Moderately Severe"

# Large scale changes
source("Modify_IND_BB.R")
IND_BB@data$BB[IND_BB@data$ADM2_CODE %in% updateBB] <- "Severe"

names(IND_BS@data)[13] <- "BS_Modified"
names(IND_BB@data)[13] <- "BB_Modified"

write.csv(IND_BS@data, "csv files/Modified_IND_BS_csv", row.names = FALSE)
write.csv(IND_BB@data, "csv files/Modified_IND_BB_csv", row.names = FALSE)

#eos

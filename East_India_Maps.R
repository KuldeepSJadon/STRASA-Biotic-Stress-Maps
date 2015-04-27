##############################################################################
# title         : East_India_Maps.R;
# purpose       : Extract and map values for diseases for East India as part of STRASA;
# producer      : prepared by A. Sparks;
# last update   : in Los Baños, Laguna, April 2015;
# inputs        : EPIRICE output from 2001-2008 for BB, BS and LB;
# outputs       : maps of BB, BS and LB for IND;
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
diseases <- list(stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bblight_audpc.tif", full.names = TRUE)),
                 stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+bspot_audpc.tif", full.names = TRUE)),
                 stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                                  pattern = "[[:graph:]]+blast_audpc.tif", full.names = TRUE)))
names(diseases) <- c("BB", "BS", "LB")

IDN <- getData("GADM", country = "IND", level = 2)

#### Start data munging ####
for(i in 1:3){
  k <- extract(mean(diseases[[i]]), IDN, method = "bilinear",
               weights = TRUE, fun = mean, na.rm = TRUE)

  k <- data.frame(unlist(lapply(k, FUN = mean, na.rm = TRUE))) # unlist and generate mean values for each polygon

  row.names(k) <- row.names(IDN)
  names(k) <- names(diseases[i])

  row.names(IDN) <- row.names(IDN)

  assign(paste(names(IDN, names(diseases[i]), sep = ".")), spCbind(IDN, k))
}


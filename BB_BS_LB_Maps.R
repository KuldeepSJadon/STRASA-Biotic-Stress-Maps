##############################################################################
# title         : Compare_EPIRICE_BBlight_Outputs.R;
# purpose       : Compare output from EPIRICE when using 1º and 0.25º data;
# producer      : prepared by A. Sparks; modified by M. Noel;
# last update   : in Los Baños, Laguna, April 2015;
# inputs        : EPIRICE output from 2001-2008 for BB, BS and LB;
# outputs       : maps by ;
# remarks 1     : ;
# Licence:      : GPL2;
##############################################################################

#### Load libraries #####
library(raster)
library(rgdal)
library(maptools)
#### End load libraries ####

#### Load data ####
diseases <- list(BB <- stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                       pattern = "[[:graph:]]+bblight_audpc.tif", full.names = TRUE)),
                 BS <- stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                       pattern = "[[:graph:]]+bspot_audpc.tif", full.names = TRUE)),
                 LB <- stack(list.files(path = "~/Google Drive/Data/EPIRICE 25deg 01-08 PK1/",
                       pattern = "[[:graph:]]+blast_audpc.tif", full.names = TRUE)))

countries <- list(IND <- getData("GADM", country = "IND", level = 2),
                  BGD <- getData("GADM", country = "BGD", level = 2),
                  NPL <- getData("GADM", country = "NPL", level = 2))

#### End load data ####

#### Start data munging ####
for (i in countries){
  for(j in diseases){
    extract(mean(mask(crop(j, i), i)), i, method = "bilinear", small = TRUE, fun = mean)
  }
}

IND.BB <- mean(mask(crop(BB, IND), IND))
IND.BS <- mean(mask(crop(BS, IND), IND))
IND.LB <- mean(mask(crop(LB, IND), IND))

BGD.BB <- mean(mask(crop(BB, BGD), BGD))
BGD.BS <- mean(mask(crop(BS, BGD), BGD))
BGD.LB <- mean(mask(crop(LB, BGD), BGD))

NPL.BB <- extract(mean(mask(crop(BB, NPL), NPL)), NPL, method = "bilinear", small = TRUE, fun = mean)
NPL.BS <- mean(mask(crop(BS, NPL), NPL))
NPL.LB <- mean(mask(crop(LB, NPL), NPL))

#### End data munging ####

#### Start data visualisation ####

#### End data visualisation ####

#eos

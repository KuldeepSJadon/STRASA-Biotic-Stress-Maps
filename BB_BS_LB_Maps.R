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

countries <- list(getData("GADM", country = "BGD", level = 2),
                  getData("GADM", country = "IND", level = 2),
                  getData("GADM", country = "NPL", level = 2))
names(countries) <- c("BGD", "IND", "NPL")

#### End load data ####

#### Start data munging ####
for(i in 1:3){
  for(j in 1:3){
    k <- extract(mean(mask(crop(diseases[[j]], countries[[i]]), countries[[i]])),
                 coordinates(countries[[i]]), method = "bilinear",
                 small = TRUE,
                 fun = mean,
                 na.rm = TRUE)

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
i <- ls()
for(j in i)
j@data$id <- rownames(j@data)
j.df <- fortify(j, id = "BB", region = "BB")
j.df$id <- as.numeric(j.df$id)
breaks <- round(classIntervals(j.df$id, 5, style = "equal", labels = FALSE)$brks, 0)

j.df$plot <- cut(j.df$id, breaks = breaks, include.lowest = TRUE)

ggplot(data = j.df, aes(long, lat, group = group)) +
  geom_polygon(aes(group = group, fill = plot), color = "white") +
  theme_minimal() +
  scale_fill_brewer(name = "Relative Risk",
                    labels = c("Low", "Moderately\nLow", "Moderate", "Moderately\nHigh", "High")) +
  ggtitle(paste("Relative Risk of",  "for Bangladesh", sep = " ")) +
  coord_equal()

#### End data visualisation ####

#eos
